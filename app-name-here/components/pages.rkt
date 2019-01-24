#lang racket/base

(require "templates.rkt")

(provide home-page
         not-found-page)

(define (home-page req)
  (page
   (container `(h1 "Hi"))))

(define (not-found-page req)
  (page
   #:subtitle "Page Not Found"
   (container
    '(h1 "Page Not Found")
    '(p "Couldn't find nothing, " (em "nothing") " I tell ya!"))))
