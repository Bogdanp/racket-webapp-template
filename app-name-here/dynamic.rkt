#lang racket/base

(require component
         racket/runtime-path
         "components/app.rkt"
         "components/server.rkt"
         (prefix-in config: "config.rkt"))

(provide start stop reload!)

(define (start)
  (system-start prod-system))

(define (stop)
  (system-stop prod-system))

(define (reload!)
  (set! prod-system (make-system)))

;; When we reload a dependency of this module, this module itself may
;; or may not be reloaded.  If it isn't reloaded then it's going to
;; reference contracts that may no longer be valid (like those of
;; make-app or make-server).
(define (make-system)
  (define-system inner
    [app make-app]
    [server (app) (make-server #:host config:http-host
                               #:port config:http-port)])

  inner-system)

(define prod-system (make-system))
