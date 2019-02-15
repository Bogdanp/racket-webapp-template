#lang racket/base

(require component
         libuuid
         racket/contract/base
         racket/function
         racket/generic
         racket/serialize
         struct-plus-plus
         threading
         web-server/http
         web-server/http/id-cookie
         "../util.rkt"
         "profiler.rkt")

;; Session stores ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [session-store? (-> any/c boolean?)]
  [memory-session-store? (-> any/c boolean?)]
  [make-memory-session-store (->* () (#:ttl exact-positive-integer?
                                      #:file-path path-string?) memory-session-store?)]))

(define-generics session-store
  (session-store-generate-id session-store)
  (session-store-load! session-store)
  (session-store-persist! session-store)
  (session-store-ref session-store session-id key default)
  (session-store-set! session-store session-id key value)
  (session-store-update! session-store session-id key f default)
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
           (define sessions (read))
           (unless (eof-object? sessions)
             (set-box! (memory-session-store-data ss) (deserialize sessions)))))))

   (define (session-store-persist! ss)
     (with-output-to-file (memory-session-store-file-path ss)
       #:exists 'truncate/replace
       (lambda ()
         (write (serialize (unbox (memory-session-store-data ss)))))))

   (define (session-store-ref ss session-id key default)
     (and~> (unbox (memory-session-store-data ss))
            (hash-ref (string->symbol session-id) (cons (current-seconds) (hasheq)))
            (cdr)
            (hash-ref key default)))

   (define (session-store-set! ss session-id key value)
     (memory-session-store-update-session ss session-id (curryr hash-set key value)))

   (define (session-store-update! ss session-id key value default)
     (memory-session-store-update-session ss session-id (curryr hash-update key value default)))

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

(define ((memory-session-store-remove-stale-sessions ttl) all-sessions)
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
         (box-swap! data-box (memory-session-store-remove-stale-sessions ttl))
         (loop))))

    (memory-session-store custodian data-box file-path)))


;; Session manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [session-manager? (-> any/c boolean?)]
  [make-session-manager (->* (#:cookie-name string?
                              #:shelf-life exact-positive-integer?
                              #:secret-key bytes?
                              #:store session-store?)
                             (-> session-manager?))]

  [current-session-id (parameter/c (or/c false/c string?))]

  [session-manager-ref (case-> (-> session-manager? symbol? any/c)
                               (-> session-manager? symbol? any/c any/c))]
  [session-manager-set! (-> session-manager? symbol? any/c void?)]
  [session-manager-remove! (-> session-manager? symbol? void?)]
  [session-manager-update! (case-> (-> session-manager? symbol? (-> any/c any/c) any/c)
                                   (-> session-manager? symbol? (-> any/c any/c) any/c any/c))]))

(define-logger session)

(define current-session-id
  (make-parameter #f))

(struct session-manager (cookie-name shelf-life secret-key store)
  #:methods gen:component
  [(define (component-start sm)
     (begin0 sm
       (session-store-load! (session-manager-store sm))))

   (define (component-stop sm)
     (begin0 sm
       (session-store-persist! (session-manager-store sm))))])

(define ((make-session-manager #:cookie-name cookie-name
                               #:shelf-life shelf-life
                               #:secret-key secret-key
                               #:store store))
  (session-manager cookie-name shelf-life secret-key store))

(define session-manager-ref
  (case-lambda
    [(sm key default)
     (session-store-ref (session-manager-store sm) (current-session-id) key default)]

    [(sm key)
     (session-manager-ref sm key (lambda ()
                                   (raise-user-error 'session-manager-ref "no value found for key ~a" key)))]))

(define (session-manager-set! sm key value)
  (with-timing 'session "session-manager-set!"
    (session-store-set! (session-manager-store sm) (current-session-id) key value)))

(define (session-manager-remove! sm key)
  (with-timing 'session "session-manager-remove!"
    (session-store-remove! (session-manager-store sm) (current-session-id) key)))

(define session-manager-update!
  (case-lambda
    [(sm key f default)
     (with-timing 'session "session-manager-update!"
       (session-store-update! (session-manager-store sm) (current-session-id) key f default))]

    [(sm key f)
     (session-manager-update! sm key f (lambda ()
                                         (raise-user-error 'session-manager-update! "no value found for key ~a" key)))]))


(module+ test
  (require racket/file
           rackunit
           rackunit/text-ui
           (prefix-in config: "../config.rkt"))

  (define session-manager
    ((make-session-manager #:cookie-name config:session-cookie-name
                           #:shelf-life config:session-shelf-life
                           #:secret-key config:session-secret-key
                           #:store (make-memory-session-store #:file-path (make-temporary-file)))))

  (run-tests
   (test-suite
    "session"
    #:before
    (lambda _
      (component-start session-manager))

    #:after
    (lambda _
      (component-stop session-manager))

    (parameterize ([current-session-id "a"])
      (session-manager-set! session-manager 'uid "1")

      (test-case "can ref keys"
        (check-equal? "1" (session-manager-ref session-manager 'uid)))

      (test-case "can ref missing keys w/ error"
        (check-exn
         exn:fail:user?
         (lambda ()
           (session-manager-ref session-manager 'missing))))

      (test-case "can ref missing keys w/ default"
        (check-false (session-manager-ref session-manager 'missing #f)))

      (test-case "can't ref other people's keys"
        (parameterize ([current-session-id "b"])
          (check-false (session-manager-ref session-manager 'uid #f))))

      (test-case "can't ref removed keys"
        (session-manager-remove! session-manager 'uid)
        (check-false (session-manager-ref session-manager 'uid #f)))))))


;; Middleware ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [wrap-session (-> session-manager? (-> (-> request? response?) (-> request? response?)))]))

(define (((wrap-session sm) handler) req)
  (with-timing 'session "wrap-session"
    (define store (session-manager-store sm))
    (define session-id
      (or (request-id-cookie req
                             #:name (session-manager-cookie-name sm)
                             #:key (session-manager-secret-key sm)
                             #:shelf-life (session-manager-shelf-life sm))
          (session-store-generate-id store)))

    (parameterize ([current-session-id session-id])
      (define cookie
        (make-id-cookie (session-manager-cookie-name sm) session-id
                        #:path "/"
                        #:key (session-manager-secret-key sm)
                        #:max-age (session-manager-shelf-life sm)))

      (define resp (handler req))
      (define headers
        (cons (cookie->header cookie)
              (response-headers resp)))

      (struct-copy response resp [headers headers]))))