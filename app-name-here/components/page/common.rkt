#lang racket/base

(require racket/contract/base
         web-server/http
         "../template.rkt")

(provide
 (contract-out
  [not-found-page (-> request? response?)]))

(define (not-found-page req)
  (page
   #:subtitle "Page Not Found"
   (container
    '(h1 "Page Not Found")
    '(p "Couldn't find nothing, " (em "nothing") " I tell ya!"))))
