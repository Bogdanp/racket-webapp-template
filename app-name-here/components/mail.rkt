#lang racket/base

(require component
         postmark
         racket/contract/base
         racket/format
         racket/function
         racket/generic
         racket/string
         "url.rkt"
         "user.rkt")

(provide
 (contract-out
  [mail-adapter? (-> any/c boolean?)]
  [mail-adapter-send-email-with-template (->* (mail-adapter?
                                               #:to string?
                                               #:from string?
                                               #:template-model string?)
                                              (#:template-id (or/c false/c string?)
                                               #:template-alias (or/c false/c string?))
                                              void?)]
  [struct stub-mail-adapter ()]
  [struct postmark-mail-adapter ([client postmark?])]
  [struct mailer ([sender string?]
                  [adapter mail-adapter?])]
  [make-mailer (->* (#:adapter mail-adapter? #:sender string?) () (-> mailer?))]
  [mailer-send-welcome-email (-> mailer? user? void?)]))

(define-logger mail-adapter)

(define-generics mail-adapter
  (mail-adapter-send-email-with-template mail-adapter
                                         #:to to
                                         #:from from
                                         #:template-id [template-id]
                                         #:template-alias [template-alias]
                                         #:template-model template-model))

(struct stub-mail-adapter ()
  #:methods gen:mail-adapter
  [(define (mail-adapter-send-email-with-template _
                                                  #:to to
                                                  #:from from
                                                  #:template-id [template-id #f]
                                                  #:template-alias [template-alias #f]
                                                  #:template-model template-model)
     (unless (or template-id template-alias)
       (raise-user-error 'mail-adapter-send-email-with-template "either template-id or template-alias must be provided"))

     (log-mail-adapter-info (string-join (list "sending templated email"
                                               (format "  to: ~a" to)
                                               (format "  from: ~a" from)
                                               (format "  template: ~a" (or template-id template-alias))
                                               (format "  model: ~a" template-model)) "\n")))])

(struct postmark-mail-adapter (client)
  #:methods gen:mail-adapter
  [(define (mail-adapter-send-email-with-template ma
                                                  #:to to
                                                  #:from from
                                                  #:template-id [template-id #f]
                                                  #:template-alias [template-alias #f]
                                                  #:template-model template-model)
     (void
      (postmark-send-email-with-template
       (postmark-mail-adapter-client ma)
       #:to to
       #:from from
       #:template-id template-id
       #:template-alias template-alias
       #:template-model template-model)))])

(struct mailer (sender adapter)
  #:methods gen:component
  [(define component-start identity)
   (define component-stop identity)])

(define ((make-mailer #:adapter adapter
                      #:sender sender))
  (mailer sender adapter))

(define (mailer-send-welcome-email m user)
  (define action-url
    (make-application-url "verify" (number->string (user-id user)) (user-verification-code user)))

  (mail-adapter-send-email-with-template
   (mailer-adapter m)
   #:to (user-username user)
   #:from (mailer-sender m)
   #:template-alias "welcome"
   #:template-model (with-common-template-variables
                      'action_url action-url
                      'name (user-username user)
                      'username (user-username user))))

(define (with-common-template-variables . variables)
  (apply hasheq
         'company_name "AppNameHere"
         'company_address ""
         'sender_name "Jim"
         'support_email "support@example.com"
         'product_name "AppNameHere"
         'product_url "http://example.com"   ;; FIXME
         variables))
