#lang racket/base

(require net/url
         racket/function
         (prefix-in config: "../config.rkt"))

(provide make-application-url)

(define (make-application-url #:query [query null] . path-elements)
  (define path
    (map (curryr path/param null) path-elements))

  (define port
    (cond
      [(member config:url-port '("80" "443")) #f]
      [else (string->number config:url-port)]))

  (url->string
   (url config:url-scheme #f config:url-host port #t path query #f)))
