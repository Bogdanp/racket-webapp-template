#lang racket/base

(require component
         net/url
         (for-syntax racket/base)
         racket/contract/base
         racket/function
         racket/path
         racket/runtime-path
         racket/string
         threading
         web-server/dispatch
         web-server/dispatchers/dispatch
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         web-server/dispatchers/filesystem-map
         web-server/http
         web-server/servlet-dispatch

         "auth.rkt"
         "database.rkt"
         "http.rkt"
         "mail.rkt"
         "page/auth.rkt"
         "page/common.rkt"
         "page/dashboard.rkt"
         "user.rkt")

(provide
 (contract-out
  [struct app ([dispatcher dispatcher/c])]
  [make-app (-> auth-manager? database? mailer? user-manager? app?)]))

(define-runtime-path static-path
  (build-path 'up 'up "static"))

(define url->path
  (make-url->path static-path))

(define (static-url->path u)
  (url->path (struct-copy url u [path (cdr (url-path u))])))

(define static-dispatcher
  (files:make
   #:url->path static-url->path
   #:path->mime-type path->mime-type))

(struct app (dispatcher)
  #:methods gen:component
  [(define component-start identity)
   (define component-stop identity)])

(define (make-app auth db mailer users)
  (define-values (dispatch-main reverse-main-uri)
    (dispatch-rules
     [("") dashboard-page]))

  (define-values (dispatch-auth reverse-auth-uri)
    (dispatch-rules
     [("login") (login-page auth)]
     [("logout") (logout-page auth)]
     [("signup") (signup-page auth mailer users)]
     [("verify" (integer-arg) (string-arg)) (verify-page users)]))

  (app (sequencer:make
        (filter:make #rx"^/static/.+$" static-dispatcher)
        (dispatch/servlet dispatch-auth)
        (dispatch/servlet
         (~> dispatch-main
             ((with-auth-required users))))
        (dispatch/servlet not-found-page))))
