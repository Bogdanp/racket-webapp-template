#lang racket/base

(require component
         postmark
         racket/runtime-path
         "components/app.rkt"
         "components/auth.rkt"
         "components/database.rkt"
         "components/mail.rkt"
         "components/server.rkt"
         "components/user.rkt"
         (prefix-in config: "config.rkt"))

(provide start stop reload!)

(define (start)
  (system-start prod-system))

(define (stop)
  (system-stop prod-system))

(define (reload!)
  (set! prod-system (make-system)))

(define mail-adapter
  (cond
    [config:email-postmark-token
     (postmark-mail-adapter
      (postmark config:email-postmark-token))]

    [else
     (stub-mail-adapter)]))

;; When we reload a dependency of this module, this module itself may
;; or may not be reloaded.  If it isn't reloaded then it's going to
;; reference contracts that may no longer be valid (like those of
;; make-app or make-server).
(define (make-system)
  (define-system inner
    [app (auth db mailer users) make-app]
    [auth (users) auth-manager]
    [db (make-database #:database config:db-name
                       #:username config:db-username
                       #:password config:db-password
                       #:host config:db-host
                       #:port config:db-port)]
    [mailer (make-mailer #:adapter mail-adapter
                         #:sender config:email-sender)]
    [server (app) (make-server #:host config:http-host
                               #:port config:http-port)]
    [users (db) user-manager])

  inner-system)

(define prod-system (make-system))
