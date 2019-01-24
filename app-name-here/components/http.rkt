#lang racket/base

(require (for-syntax racket/base)
         racket/list
         racket/match
         racket/runtime-path
         racket/string
         web-server/private/mime-types)

(provide path->mime-type ;; provided without a contract for speed
         )

(define-runtime-path mime.types-path
  (build-path 'up 'up "resources" "mime.types"))

(define path->mime-type
  (make-path->mime-type mime.types-path))

(module+ test
  (require rackunit)

  (check-equal? (path->mime-type (string->path "foo/bar.html")) #"text/html")
  (check-equal? (path->mime-type (string->path "test.js")) #"application/javascript")
  (check-equal? (path->mime-type (string->path "test.min.css")) #"text/css"))
