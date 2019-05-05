#lang racket/base

(require component
         postmark
         racket/runtime-path
         "components/app.rkt"
         "components/auth.rkt"
         "components/database.rkt"
         "components/flash.rkt"
         "components/mail.rkt"
         "components/server.rkt"
         "components/session.rkt"
         "components/user.rkt"
         "components/url.rkt"
         (prefix-in config: "config.rkt"))

(provide
 prod-system
 start
 stop)

(define (start)
  (system-start prod-system))

(define (stop)
  (system-stop prod-system))

(define mail-adapter
  (if config:postmark-token
      (postmark-mail-adapter (postmark config:postmark-token))
      (make-stub-mail-adapter)))

(define common-mail-variables
  (hasheq 'product_url     (make-application-url)
          'product_name    config:product-name
          'company_name    config:company-name
          'company_address config:company-address
          'sender_name     config:support-name
          'support_email   config:support-email))

(define-system prod
  [app (auth db flashes mailer sessions users) make-app]
  [auth (sessions users) make-auth-manager]
  [db (make-database #:database config:db-name
                     #:username config:db-username
                     #:password config:db-password
                     #:host config:db-host
                     #:port config:db-port)]
  [flashes (sessions) make-flash-manager]
  [mailer (make-mailer #:adapter mail-adapter
                       #:sender config:support-email
                       #:common-variables common-mail-variables)]
  [server (app) (compose1 (make-server #:host config:http-host
                                       #:port config:http-port) app-dispatcher)]
  [sessions (make-session-manager #:cookie-name config:session-cookie-name
                                  #:shelf-life config:session-shelf-life
                                  #:secret-key config:session-secret-key
                                  #:store (make-memory-session-store))]
  [users (db) user-manager])

(module+ main
  (require "logging.rkt")

  (void (start-logger))
  (start)

  (with-handlers ([exn:break? (lambda (e)
                                (stop)
                                (sync/enable-break (system-idle-evt)))])
    (sync/enable-break never-evt)))
