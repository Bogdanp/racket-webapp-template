#lang racket/base

(require component
         libuuid
         racket/contract/base
         racket/function
         racket/generic
         struct-plus-plus
         threading
         web-server/http
         web-server/http/id-cookie
         "../util.rkt")

;; Session stores ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [session-store? (-> any/c boolean?)]
  [memory-session-store? (-> any/c boolean?)]
  [make-memory-session-store (->* () (#:ttl exact-positive-integer?) memory-session-store?)]))

(define-generics session-store
  (session-store-generate-id session-store)
  (session-store-load! session-store)
  (session-store-persist! session-store)
  (session-store-ref session-store session-id key default)
  (session-store-set! session-store session-id key value)
  (session-store-remove! session-store session-id key))

(define-logger memory-session-store)

(struct memory-session-store (custodian data file-path)
  #:methods gen:session-store
  [(define (session-store-generate-id _)
     (uuid-generate))

   (define (session-store-load! ss)
     (when (file-exists? (memory-session-store-file-path ss))
       (with-input-from-file (memory-session-store-file-path ss)
         (lambda ()
           (set-box! (memory-session-store-data ss) (read))))))

   (define (session-store-persist! ss)
     (with-output-to-file (memory-session-store-file-path ss)
       #:exists 'truncate/replace
       (lambda ()
         (write (unbox (memory-session-store-data ss))))))

   (define (session-store-ref ss session-id key default)
     (and~> (unbox (memory-session-store-data ss))
            (hash-ref (string->symbol session-id) (cons (current-seconds) (hasheq)))
            (cdr)
            (hash-ref key default)))

   (define (session-store-set! ss session-id key value)
     (memory-session-store-update-session ss session-id (curryr hash-set key value)))

   (define (session-store-remove! ss session-id key)
     (memory-session-store-update-session ss session-id (curryr hash-remove key)))])

(define (memory-session-store-update-session ss session-id f)
  (box-swap!
   (memory-session-store-data ss)
   (lambda (sessions)
     (hash-update sessions
                  (string->symbol session-id)
                  (lambda (session-data)
                    (cons (current-seconds) (f (cdr session-data))))
                  (cons (current-seconds) (hasheq))))))

(define ((memory-session-store-expire-stale-sessions ttl) all-sessions)
  (for/fold ([live-sessions (hasheq)])
            ([(session-id session-data) (in-hash all-sessions)])
    (cond
      [(<= (current-seconds) (+ (car session-data) ttl))
       (hash-set live-sessions session-id session-data)]

      [else
       (log-memory-session-store-debug "session ~v expired" session-id)
       live-sessions])))

(define (make-memory-session-store #:ttl [ttl (* 7 86400)]
                                   #:file-path [file-path "/tmp/memory-store-data.ss"])
  (define custodian (make-custodian))
  (parameterize ([current-custodian custodian])
    (define data-box (box (hasheq)))
    (thread
     (lambda ()
       (let loop ()
         (sleep ttl)
         (log-memory-session-store-debug "expiring stale sessions")
         (box-swap! data-box (memory-session-store-expire-stale-sessions ttl))
         (loop))))

    (memory-session-store custodian data-box file-path)))


;; Session manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [session-manager? (-> any/c boolean?)]
  [make-session-manager (->* (#:cookie-name string?
                              #:secret-key bytes?
                              #:store session-store?)
                             (-> session-manager?))]
  [session-manager-set! (-> session-manager? symbol? any/c void?)]
  [session-manager-remove! (-> session-manager? symbol? void?)]
  [session-manager-ref (->* (session-manager? symbol?) (any/c) any/c)]))

(define-logger session)

(define current-session-id
  (make-parameter #f))

(struct session-manager (cookie-name secret-key store)
  #:methods gen:component
  [(define (component-start sm)
     (begin0 sm
       (session-store-load! (session-manager-store sm))))

   (define (component-stop sm)
     (begin0 sm
       (session-store-persist! (session-manager-store sm))))])

(define ((make-session-manager #:cookie-name cookie-name
                               #:secret-key secret-key
                               #:store store))
  (session-manager cookie-name secret-key store))

(define session-manager-ref
  (case-lambda
    [(sm key default)
     (session-store-ref (session-manager-store sm) (current-session-id) key default)]

    [(sm key)
     (session-manager-ref sm key (lambda ()
                                   (raise-user-error 'session-manager-ref "no value found for key ~a" key)))]))

(define (session-manager-set! sm key value)
  (session-store-set! (session-manager-store sm) (current-session-id) key value))

(define (session-manager-remove! sm key)
  (session-store-remove! (session-manager-store sm) (current-session-id) key))


;; Middleware ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [wrap-session (-> session-manager? (-> (-> request? response?) (-> request? response?)))]))

(define (((wrap-session sm) handler) req)
  (define store (session-manager-store sm))
  (define session-id
    (or (request-id-cookie req
                           #:name (session-manager-cookie-name sm)
                           #:key (session-manager-secret-key sm))
        (session-store-generate-id store)))

  (parameterize ([current-session-id session-id])
    (define cookie
      (make-id-cookie (session-manager-cookie-name sm) session-id
                      #:key (session-manager-secret-key sm)
                      #:path "/"))

    (define resp (handler req))
    (define headers
      (cons (cookie->header cookie)
            (response-headers resp)))

    (struct-copy response resp [headers headers])))
