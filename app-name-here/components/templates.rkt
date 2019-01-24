#lang racket/base

(require (for-syntax racket/base
                     racket/file
                     racket/path
                     syntax/parse)
         racket/format
         racket/runtime-path
         web-server/http
         (prefix-in config: "../config.rkt"))

(provide container page)

(define-syntax known-static-files
  (let* ([current-dir (build-path (syntax-source #'here) 'up)]
         [static-path (simplify-path (build-path current-dir 'up 'up "static"))])
    (map (lambda (p)
           (find-relative-path static-path p))
         (find-files (compose not directory-exists?) static-path))))

(define-syntax (static-uri stx)
  (syntax-parse stx
    [(static-uri path:string)
     (unless (member (string->path (syntax->datum #'path))
                     (syntax-local-value #'known-static-files))
       (raise-syntax-error 'static-uri (format "static file ~v not found" (syntax->datum #'path))))

     #'(format "/static/~a?rev=~a" path config:version)]))

(define-syntax (xexpr-when stx)
  (syntax-parse stx
    [(xexpr-when condition e ...+)
     #'(if condition
           (list e ...)
           (list))]))

(define (container . content)
  `(div ((class "container")) ,@content))

(define (nav . items)
  `(div
    ((class "nav"))
    ,(container `(ul ((class "nav__items")) ,@items))))

(define (nav-item uri label)
  `(li
    ((class "nav__item"))
    (a ((href ,uri)) ,label)))

(define (page #:subtitle [subtitle #f]
              #:show-nav? [show-nav? #t]
              . content)
  (response/xexpr
   #:preamble #"<!doctype html>"
   `(html
     (head
      (title
       ,(if subtitle
            (~a subtitle " &mdash; AppNameHere")
            "AppNameHere"))
      (link ((rel "stylesheet") (href ,(static-uri "css/screen.css")))))
     (body
      ,@(xexpr-when show-nav?
          (nav (nav-item "/" "Home")
               (nav-item "/login" "Log in")
               (nav-item "/signup" "Sign up")))
      ,@content))))
