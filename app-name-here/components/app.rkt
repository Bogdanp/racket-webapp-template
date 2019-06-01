#lang racket/base

(require (for-syntax racket/base)
         component
         koyo/continuation
         koyo/flash
         koyo/profiler
         koyo/session
         net/url
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
         web-server/managers/lru
         web-server/servlet-dispatch
         "auth.rkt"
         "cors.rkt"
         "database.rkt"
         "http.rkt"
         "l10n.rkt"
         "mail.rkt"
         "page/auth.rkt"
         "page/common.rkt"
         "page/dashboard.rkt"
         "preload.rkt"
         "url.rkt"
         "user.rkt")

(provide
 (contract-out
  [struct app ([dispatcher dispatcher/c])]
  [make-app (-> auth-manager? database? flash-manager? mailer? session-manager? user-manager? app?)]))

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

(define (make-app auth db flashes mailer sessions users)
  (define-values (dispatch reverse-uri req-roles)
    (dispatch-rules+roles
     [("") #:roles (user) dashboard-page]
     [("login") (login-page auth)]
     [("logout") (logout-page auth)]
     [("signup") (signup-page auth mailer users)]
     [("verify" (integer-arg) (string-arg)) (verify-page flashes users)]
     [else not-found-page]))

  (current-reverse-uri-fn reverse-uri)

  ;; Requests go up (starting from the last wrapper) and respones go down!
  (define (stack handler)
    (~> handler
        ((wrap-browser-locale sessions))
        ((wrap-auth-required auth req-roles))
        ((wrap-flash flashes))
        ((wrap-session sessions))
        (wrap-protect-continuations)
        (wrap-preload)
        (wrap-cors)
        (wrap-profiler)))

  (define manager
    (make-threshold-LRU-manager (stack (expired-page flashes)) (* 1024 1024 128)))

  (app (sequencer:make
        (filter:make #rx"^/static/.+$" static-dispatcher)
        (dispatch/servlet #:manager manager (stack dispatch)))))
