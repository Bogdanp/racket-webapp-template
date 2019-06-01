#lang racket/base

(require component
         koyo/profiler
         koyo/session
         racket/contract/base
         racket/function
         web-server/http)

;; Flash manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [flash-manager? (-> any/c boolean?)]
  [make-flash-manager (-> session-manager? flash-manager?)]
  [current-flash-messages (parameter/c (listof (cons/c symbol? string?)))]
  [flash (-> flash-manager? symbol? string? void?)]))

(define flash-messages-key 'flash.messages)

(struct flash-manager (session-manager)
  #:methods gen:component
  [(define component-start identity)
   (define component-stop identity)])

(define make-flash-manager
  flash-manager)

(define current-flash-messages
  (make-parameter null))

(define (flash fm key message)
  (with-timing 'flash "flash"
    (session-manager-update! (flash-manager-session-manager fm)
                             flash-messages-key
                             (lambda (messages)
                               (cons (cons key message) messages)) null)))


;; Middleware ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [wrap-flash (-> flash-manager? (-> (-> request? response?) (-> request? response?)))]))

(define (((wrap-flash fm) handler) req)
  (with-timing 'flash "wrap-flash"
    (define sessions (flash-manager-session-manager fm))
    (define flash-messages (session-manager-ref sessions flash-messages-key null))
    (session-manager-remove! sessions flash-messages-key)

    (parameterize ([current-flash-messages flash-messages])
      (handler req))))

(module+ test
  (require racket/file
           rackunit
           rackunit/text-ui
           (prefix-in config: "../config.rkt"))

  (define-system test
    [flashes (sessions) flash-manager]
    [sessions (make-session-manager-factory #:cookie-name config:session-cookie-name
                                            #:shelf-life config:session-shelf-life
                                            #:secret-key config:session-secret-key
                                            #:store (make-memory-session-store #:file-path (make-temporary-file)))])

  (run-tests
   (test-suite
    "flash"
    #:before
    (lambda _
      (system-start test-system))

    #:after
    (lambda _
      (system-stop test-system))

    (test-suite
     "flash"

     (test-case "adds messages"
       (parameterize ([current-session-id "a-session-id"])
         (flash (system-get test-system 'flashes) 'error "Error message 1.")
         (flash (system-get test-system 'flashes) 'info "Info message 1.")
         (flash (system-get test-system 'flashes) 'info "Info message 2.")

         (check-equal?
          (session-manager-ref (system-get test-system 'sessions) flash-messages-key)
          '((info . "Info message 2.")
            (info . "Info message 1.")
            (error . "Error message 1.")))))))))
