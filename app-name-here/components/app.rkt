#lang racket/base

(require component
         net/url
         (for-syntax racket/base)
         racket/contract/base
         racket/function
         racket/path
         racket/runtime-path
         racket/string
         web-server/dispatch
         web-server/dispatchers/dispatch
         (prefix-in files: web-server/dispatchers/dispatch-files)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         web-server/dispatchers/filesystem-map
         web-server/http
         web-server/servlet-dispatch

         "http.rkt"
         "pages.rkt")

(provide (contract-out
          [struct app ((dispatcher dispatcher/c))]
          [make-app (-> app?)]))

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
  [(define (component-start app) app)
   (define (component-stop app) app)])

(define (make-app)
  (define-values (dispatch _)
    (dispatch-rules
     [("") home-page]
     [else not-found-page]))

  (app (sequencer:make
        (filter:make #rx"^/static/.+$" static-dispatcher)
        (dispatch/servlet dispatch))))
