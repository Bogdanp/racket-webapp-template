#lang racket/base

(require net/url
         racket/contract/base
         racket/function
         racket/promise
         racket/string
         web-server/http)

(provide
 (contract-out
  [make-test-request (->* ()
                          (#:method false/string/bytes/c
                           #:content false/string/bytes/c
                           #:headers (listof header?)
                           #:bindings (listof binding?)
                           #:scheme string?
                           #:host string?
                           #:port (integer-in 0 65535)
                           #:path string?
                           #:query (listof (cons/c symbol? (or/c false/c string?))))
                          request?)]))

(define false/string/bytes/c
  (or/c false/c string? bytes?))

(define (false/string/bytes->bytes f/s/b)
  (cond
    [(string? f/s/b) (string->bytes/utf-8 f/s/b)]
    [(not f/s/b) #f]
    [else f/s/b]))

(define (make-test-request #:method [method "GET"]
                           #:content [content #f]
                           #:headers [headers null]
                           #:bindings [bindings null]
                           #:scheme [scheme "http"]
                           #:host [host "127.0.0.1"]
                           #:port [port 80]
                           #:path [path "/"]
                           #:query [query null])
  (request (false/string/bytes->bytes method)
           (url scheme #f host port #t (map (curryr path/param null) (string-split path "/")) query #f)
           headers
           (delay bindings)
           (false/string/bytes->bytes content)
           host
           port
           "127.0.0.1"))
