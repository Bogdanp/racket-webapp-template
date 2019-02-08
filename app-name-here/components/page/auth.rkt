#lang racket/base

(require forms
         racket/contract/base
         racket/match
         web-server/servlet

         "../auth.rkt"
         "../mail.rkt"
         "../user.rkt"
         "../template.rkt")

(provide
 (contract-out
  [login-page (-> auth-manager? (-> request? response?))]
  [logout-page (-> auth-manager? (-> request? response?))]
  [signup-page (-> auth-manager? mailer? user-manager? (-> request? response?))]
  [verify-page (-> user-manager? (-> request? integer? string? response?))]))


(define ((make-labeled-field label name widget) render-widget)
  `(div
    ((class "form__group"))
    (label ,label ,(render-widget name widget))
    ,@(render-widget name (widget-errors #:class "form__errors"))))


;; login & logout ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define login-form
  (form* ([username (ensure binding/email (required))]
          [password (ensure binding/text (required))])
    (list username password)))

(define (render-login-form target render-widget [error-message #f])
  (define render-username-field
    (make-labeled-field "Username" "username" (widget-email #:attributes '((placeholder "bruce@waye.co")))))
  (define render-password-field
    (make-labeled-field "Password" "password" (widget-password #:attributes '((placeholder "••••••••••••••••")))))

  `(form
    ((action ,target)
     (method "POST")
     (class "form form--login"))

    ,@(xexpr-when error-message
        `(ul
          ((class "form__errors"))
          (li ,error-message)))

    ,(render-username-field render-widget)
    ,(render-password-field render-widget)

    (button ((class "button button--primary")
             (type "submit")) "Log in")

    (a ((class "button button--secondary")
        (href "/signup"))
       "Don't have an account? Sign up!")))

(define ((login-page auth) req)
  (send/suspend/dispatch
   (lambda (embed/url)
     (define (render render-widget [error-message #f])
       (page
        #:subtitle "Log in"
        (container
         (render-login-form (embed/url (login-page auth)) render-widget error-message))))

     (match (form-run login-form req)
       [(list 'passed (list username password) render-widget)
        (with-handlers ([exn:fail:auth-manager:unverified?
                         (lambda _
                           (render render-widget "Please verify your e-mail!"))])
          (cond
            [(auth-manager-login auth username password)
             => (lambda (cookie)
                  (redirect-to "/" #:headers (list (cookie->header cookie))))]

            [else
             (render render-widget "Invalid username or password.")]))]

       [(list _ _ render-widget)
        (render render-widget)]))))

(define ((logout-page auth) req)
  (redirect-to "/login" #:headers (list (cookie->header (auth-manager-logout auth)))))


;; signup & verify ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define signup-form
  (form* ([username (ensure binding/email (required))]
          [password (ensure binding/text (required) (longer-than 7))])
    (list username password)))

(define (render-signup-form target render-widget [error-message #f])
  (define render-username-field
    (make-labeled-field "Username" "username" (widget-email #:attributes '((placeholder "bruce@waye.co")))))
  (define render-password-field
    (make-labeled-field "Password" "password" (widget-password #:attributes '((placeholder "••••••••••••••••")))))

  `(form
    ((action ,target)
     (method "POST")
     (class "form form--signup"))

    ,@(xexpr-when error-message
        `(ul
          ((class "form__errors"))
          (li ,error-message)))

    ,(render-username-field render-widget)
    ,(render-password-field render-widget)

    (button ((class "button button--primary")
             (type "submit")) "Sign up")

    (a ((class "button button--secondary")
        (href "/login"))
       "Already have an account? Log in!")))

(define ((signup-page auth mailer users) req)
  (send/suspend/dispatch
   (lambda (embed/url)
     (define (render render-widget [error-message #f])
       (page
        #:subtitle "Sign up"
        (container
         (render-signup-form (embed/url (signup-page auth mailer users)) render-widget error-message))))

     (match (form-run signup-form req)
       [(list 'passed (list username password) render-widget)
        (with-handlers ([exn:fail:user-manager:username-taken?
                         (lambda _
                           (render render-widget "This username is taken."))])
          (define user (user-manager-create-user users username password))
          (mailer-send-welcome-email mailer user)
          (post-signup-page (redirect/get/forget)))]

       [(list _ _ render-widget)
        (render render-widget)]))))

(define (post-signup-page req)
  (page
   #:subtitle "Signed up"
   (container
    '(h1 "You've been signed up")
    '(p "You need to confirm your e-mail address before you can log in."))))

(define ((verify-page users) req user-id verification-code)
  (user-manager-verify users user-id verification-code)
  (redirect-to "/login"))
