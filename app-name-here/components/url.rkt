#lang racket/base

(require net/url
         racket/contract
         racket/function
         (prefix-in config: "../config.rkt"))

(provide make-application-url)

(define/contract (make-application-url #:query [query null]
                                       #:fragment [fragment #f]
                                       . path-elements)
  (() (#:query (listof (cons/c symbol? string?))
       #:fragment (or/c false/c string?)) #:rest (listof string?) . ->* . string?)

  (define path
    (map (curryr path/param null) path-elements))

  (define port
    (cond
      [(member config:url-port '("80" "443")) #f]
      [else (string->number config:url-port)]))

  (url->string
   (url config:url-scheme #f config:url-host port #t path query fragment)))
