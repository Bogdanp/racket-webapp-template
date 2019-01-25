#lang racket/base

(require component
         racket/contract/base
         web-server/web-server

         "app.rkt")

(provide (contract-out
          [struct server [(options hash?)
                          (app app?)
                          (stopper (or/c false/c (-> void?)))]]

          [make-server (->* ()
                            (#:host string?
                             #:port (integer-in 0 65535))
                            (-> app? server?))]))

(define-logger server)

(struct server (options app stopper)
  #:methods gen:component
  [(define (component-start a-server)
     (define options (server-options a-server))
     (define stopper (serve #:dispatch (app-dispatcher (server-app a-server))
                            #:listen-ip (hash-ref options 'host)
                            #:port (hash-ref options 'port)))

     (log-server-info "listening on ~a:~a"
                      (hash-ref options 'host)
                      (hash-ref options 'port))
     (struct-copy server a-server [stopper stopper]))

   (define (component-stop a-server)
     ((server-stopper a-server))
     (struct-copy server a-server [stopper #f]))])

(define ((make-server #:host [host "127.0.0.1"]
                      #:port [port 8000]) app)
  (server (hasheq 'host host
                  'port port) app #f))
