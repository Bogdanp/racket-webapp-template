#lang racket/base

(require net/url
         racket/contract
         racket/function
         racket/match
         racket/promise
         racket/string
         web-server/http)

(provide
 make-test-request)

(define stringy/c
  (or/c string? bytes?))

(define maybe-stringy/c
  (or/c false/c stringy/c))

(define (stringy->bytes s)
  (cond
    [(bytes?  s) s]
    [(string? s) (string->bytes/utf-8 s)]))

(define (maybe-stringy->bytes s)
  (and s (stringy->bytes s)))

(define/contract (make-test-request #:method [method "GET"]
                                    #:content [content #f]
                                    #:headers [headers null]
                                    #:bindings [bindings null]
                                    #:scheme [scheme "http"]
                                    #:host [host "127.0.0.1"]
                                    #:port [port 80]
                                    #:path [path "/"]
                                    #:query [query null]
                                    #:client-ip [client-ip "127.0.0.1"])
  (->* ()
       (#:method maybe-stringy/c
        #:content maybe-stringy/c
        #:headers (listof (or/c header? (cons/c stringy/c stringy/c)))
        #:bindings (listof binding?)
        #:scheme string?
        #:host string?
        #:port (integer-in 0 65535)
        #:path string?
        #:query (listof (cons/c symbol? (or/c false/c string?))))
       request?)

  (let ([method (maybe-stringy->bytes method)]
        [url (url scheme #f host port #t (map (curryr path/param null) (string-split path "/")) query #f)]
        [headers (for/list ([header headers])
                   (match header
                     [(? header?) header]
                     [(cons name value) (make-header (stringy->bytes name)
                                                     (stringy->bytes value))]))]
        [bindings (delay (append bindings (for/list ([param query])
                                            (make-binding:form (string->bytes/utf-8 (symbol->string (car param)))
                                                               (string->bytes/utf-8 (cdr param))))))]
        [content (maybe-stringy->bytes content)])
    (request method url headers bindings content host port client-ip)))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (run-tests
   (test-suite
    "testing"

    (test-suite
     "maybe-stringy->bytes"

     (check-eq? (maybe-stringy->bytes #f) #f)
     (check-equal? (maybe-stringy->bytes "hello") #"hello")
     (check-equal? (maybe-stringy->bytes #"hello") #"hello"))

    (test-suite
     "make-test-request"

     (test-case "appends query params to bindings"
       (define req
         (make-test-request #:bindings (list (make-binding:form #"a" #"a"))
                            #:query '((b . "b"))))

       (check-equal? (request-bindings/raw req)
                     (list (make-binding:form #"a" #"a")
                           (make-binding:form #"b" #"b"))))

     (test-case "converts headers expressed as pairs to header values"
       (define req
         (make-test-request #:headers (list (make-header #"host" #"localhost")
                                            '("x-forwarded-for" . "127.0.0.1"))))

       (check-equal? (request-headers/raw req)
                     (list (make-header #"host" #"localhost")
                           (make-header #"x-forwarded-for" #"127.0.0.1"))))))))
