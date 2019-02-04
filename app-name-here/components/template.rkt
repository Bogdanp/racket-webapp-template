#lang racket/base

(require (for-syntax racket/base
                     racket/file
                     racket/path
                     syntax/parse)
         racket/format
         racket/runtime-path
         web-server/http
         (prefix-in config: "../config.rkt")
         "preload.rkt")

(provide container static-uri page)

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

     #'(let ([p (format "/static/~a?rev=~a" path config:version)])
         (current-preload-dependencies (cons p (current-preload-dependencies)))
         p)]))

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

(define (render-page #:subtitle [subtitle #f]
                     #:show-nav? [show-nav? #t]
                     . content)
  (define page
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
       ,@content)))

  (response/xexpr
   #:preamble #"<!doctype html>"
   #:headers (make-preload-headers)
   page))

(define-syntax (page stx)
  (syntax-parse stx
    ([_ e ...+]
     #'(parameterize ([current-preload-dependencies null])
         (render-page e ...)))))
