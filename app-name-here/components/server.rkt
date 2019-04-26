#lang racket/base

(require component
         racket/contract
         racket/string
         struct-plus-plus
         web-server/dispatchers/dispatch
         web-server/web-server)

(provide
 make-server
 server?)

(define-logger server)

(define port/c (integer-in 0 65534))

(struct++ server
  ([host non-empty-string?]
   [port port/c]
   [dispatcher dispatcher/c]
   [(stopper #f) (or/c false/c (-> void?))])
  #:transparent
  #:methods gen:component
  [(define (component-start a-server)
     (define stopper
       (serve #:dispatch (server-dispatcher a-server)
              #:listen-ip (server-host a-server)
              #:port (server-port a-server)))

     (log-server-info "listening on ~a:~a"
                      (server-host a-server)
                      (server-port a-server))
     (set-server-stopper a-server stopper))

   (define (component-stop a-server)
     ((server-stopper a-server))
     (set-server-stopper a-server #f))])

(define/contract ((make-server #:host [host "127.0.0.1"]
                               #:port [port 8000]) dispatcher)
  (->* ()
       (#:host non-empty-string?
        #:port port/c)
       (-> dispatcher/c server?))
  (server++ #:host host
            #:port port
            #:dispatcher dispatcher))
