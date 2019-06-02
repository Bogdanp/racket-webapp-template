#lang racket/base

(require component
         koyo/url
         koyo/util
         postmark
         racket/contract/base
         racket/format
         racket/function
         racket/generic
         racket/hash
         racket/string
         "user.rkt")


;; Adapters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [mail-adapter? (-> any/c boolean?)]
  [mail-adapter-send-email-with-template (->* (mail-adapter?
                                               #:to string?
                                               #:from string?
                                               #:template-model string?)
                                              (#:template-id (or/c false/c string?)
                                               #:template-alias (or/c false/c string?))
                                              void?)]))

(define-logger mail-adapter)

(define-generics mail-adapter
  (mail-adapter-send-email-with-template
   mail-adapter
   #:to to
   #:from from
   #:template-id [template-id]
   #:template-alias [template-alias]
   #:template-model template-model))


;; Stub adapter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [make-stub-mail-adapter (-> stub-mail-adapter?)]
  [stub-mail-adapter? (-> any/c boolean?)]
  [stub-mail-adapter-outbox (-> stub-mail-adapter? (listof hash?))]))

(struct stub-mail-adapter (queue)
  #:methods gen:mail-adapter
  [(define (mail-adapter-send-email-with-template ma
                                                  #:to to
                                                  #:from from
                                                  #:template-id [template-id #f]
                                                  #:template-alias [template-alias #f]
                                                  #:template-model template-model)
     (unless (or template-id template-alias)
       (raise-user-error 'mail-adapter-send-email-with-template "either template-id or template-alias must be provided"))

     (define message
       (hasheq 'to to
               'from from
               'template (or template-id template-alias)
               'template-model template-model))

     (box-swap! (stub-mail-adapter-queue ma) (curry cons message))
     (log-mail-adapter-info "templated email added to outbox ~v" message))])

(define (make-stub-mail-adapter)
  (stub-mail-adapter (box null)))

(define (stub-mail-adapter-outbox ma)
  (unbox (stub-mail-adapter-queue ma)))


;; Postmark adapter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [struct postmark-mail-adapter ([client postmark?])]))

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


;; Mailer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 (contract-out
  [mailer? (-> any/c boolean?)]
  [make-mailer (->* (#:adapter mail-adapter?
                     #:sender string?
                     #:common-variables (hash/c symbol? string?))
                    (-> mailer?))]
  [mailer-send-welcome-email (-> mailer? user? void?)]))

(struct mailer (adapter sender common-variables)
  #:methods gen:component
  [(define component-start identity)
   (define component-stop identity)])

(define ((make-mailer #:adapter adapter
                      #:sender sender
                      #:common-variables common-variables))
  (mailer adapter sender common-variables))

(define (mailer-send-welcome-email m user)
  (define action-url
    (make-application-url "verify" (number->string (user-id user)) (user-verification-code user)))

  (mail-adapter-send-email-with-template
   (mailer-adapter m)
   #:to (user-username user)
   #:from (mailer-sender m)
   #:template-alias "welcome"
   #:template-model (with-common-variables m
                      'action_url action-url
                      'name (user-username user)
                      'username (user-username user))))

(define (with-common-variables m . variables)
  (hash-union
   (mailer-common-variables m)
   (apply hasheq variables)
   #:combine/key (lambda (k _ v) v)))

(module+ test
  (require rackunit
           rackunit/text-ui
           threading)

  (define adapter (make-stub-mail-adapter))
  (define mailer ((make-mailer #:adapter adapter
                               #:sender "support@example.com"
                               #:common-variables (hasheq))))

  (run-tests
   (test-suite
    "mail"

    (test-suite
     "mailer-send-welcome-email"

     (test-case "welcome emails contain valid verification urls"
       (define a-user (user++ #:id 1 #:username "someone@example.com"))
       (mailer-send-welcome-email mailer a-user)
       (check-equal?
        (~> (car (stub-mail-adapter-outbox adapter))
            (hash-ref 'template-model)
            (hash-ref 'action_url))
        (make-application-url "verify" "1" (user-verification-code a-user))))))))
