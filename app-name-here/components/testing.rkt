#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         net/url
         racket/contract
         racket/function
         racket/match
         racket/promise
         racket/string
         web-server/http
         xml)

;; requests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;; responses ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 xexpr->text
 xexpr-select
 xexpr-select-first
 xexpr-select-text)

(define/contract (xexpr->text e [sep " "])
  (->* (xexpr?) (string?) string?)
  (match e
    [(? symbol?) (format "&~a;" e)]
    [(? number?) (format "&#~a;" e)]

    [(and s (? string?)) s]

    [(list _ (list (list (? symbol?) (? string?)) ...) e ...)
     (string-join (map xexpr->text e) sep)]

    [(list _ e ...)
     (string-join (map xexpr->text e) sep)]))

(define selector/c
  (listof (or/c symbol?
                (listof (list/c symbol? string?)))))

(define (list-subset? xs ys)
  (andmap (curryr member xs) ys))

(define xexpr-select-matches?
  (match-lambda**
   [((list '* selector-attrs)
     (and (list _ element-attrs e ...) xexpr))
    (list-subset? element-attrs selector-attrs)]

   [((list tag selector-attrs)
     (and (list tag element-attrs e ...) xexpr))
    (list-subset? element-attrs selector-attrs)]

   [(_ _) #f]))

(define (xexpr-selector-select selector xexpr)
  (define matches?
    (curry xexpr-select-matches? selector))

  (let loop ([selected null]
             [remaining (list xexpr)])
    (cond
      [(null? remaining)
       (reverse selected)]

      [else
       (define-values (selected* remaining*)
         (for/fold ([selected selected]
                    [remaining null])
                   ([xexpr remaining])
           (match xexpr
             [(? matches?)
              (values (cons xexpr selected) remaining)]

             [(or (list _ (list (cons _ _) ...) es ...)
                  (list _ es ...))
              (values selected (append remaining es))]

             [_
              (values selected remaining)])))

       (loop selected* remaining*)])))

(define/contract (make-xexpr-selector selectors)
  (-> (listof selector/c)
      (-> xexpr? (listof xexpr?)))

  (define (xexpr-selector xexpr)
    (let loop ([selectors selectors]
               [selected (list xexpr)])
      (match selectors
        [(list) selected]
        [(cons selector selectors)
         (loop selectors
               (apply append (for/list ([xexpr selected])
                               (xexpr-selector-select selector xexpr))))])))

  xexpr-selector)

(begin-for-syntax
  (define-syntax-class selector
    (pattern tag:id
             #:with attrs #'())

    (pattern (tag:id [(attr:id value:str) ...+])
             #:with attrs #'((attr value) ...))))

(define-syntax (xexpr-select stx)
  (syntax-parse stx
    [(_ e:expr s:selector ...+)
     #'((make-xexpr-selector (list (list 's.tag 's.attrs) ...)) e)]))

(define-syntax (xexpr-select-first stx)
  (syntax-parse stx
    [(_ e:expr s ...+)
     #'(car (xexpr-select e s ...))]))

(define-syntax (xexpr-select-text stx)
  (syntax-parse stx
    [(_ e:expr s ...+)
     #'(map (curryr xexpr->text "") (xexpr-select e s ...))]))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (run-tests
   (test-suite
    "testing"

    (test-suite
     "xexpr->text"

     (test-case "turns xexprs into text"
       (check-equal? (xexpr->text "Hi") "Hi")
       (check-equal? (xexpr->text '(h1)) "")
       (check-equal? (xexpr->text '(h1 "Hello" "there")) "Hello there")
       (check-equal? (xexpr->text '(h1 ((class "heading")) "Hi")) "Hi")
       (check-equal? (xexpr->text '(div
                                    ((class "container"))
                                    (h1
                                     ((class "title"))
                                     "A" (strong "good") "title.")
                                    (p
                                     "And some content!")))
                     "A good title. And some content!")
       (check-equal? (xexpr->text '(p "Xexprs" 'mdash "good?")) "Xexprs &mdash; good?")
       (check-equal? (xexpr->text '(p "Xexprs" 8212 "good?")) "Xexprs &#8212; good?")))

    (test-suite
     "xexpr-select"

     (test-case "finds elements by their tag in a path"
       (define tree
         '(div
           (header
            (h1 "First heading"))
           (main
            (div
             (h1 "Second heading")))
           (footer
            (div
             (div
              (h1 "Third heading")
              (h1 "Fourth heading"))))))

       (check-equal?
        (xexpr-select tree h1)
        '((h1 "First heading")
          (h1 "Second heading")
          (h1 "Third heading")
          (h1 "Fourth heading")))

       (check-equal?
        (xexpr-select tree main h1)
        '((h1 "Second heading")))

       (check-equal?
        (xexpr-select tree footer h1)
        '((h1 "Third heading")
          (h1 "Fourth heading"))))

     (test-case "finds elements by their attributes in a path"
       (define tree
         '(div
           (header
            (a [(class "findme")
                (data-extra "")]))
           (footer
            (a))))

       (check-equal?
        (xexpr-select tree (a [(class "findme")]))
        '((a [(class "findme") (data-extra "")]))))

     (test-case "finds elements by their attributes in a path containing a wildcard"
       (define tree
         '(div
           (header
            (a [(class "findme")]))
           (footer
            (h1 [(class "findme")]))))

       (check-equal?
        (xexpr-select tree (* [(class "findme")]))
        '((a [(class "findme")])
          (h1 [(class "findme")])))

       (check-equal?
        (xexpr-select tree footer (* [(class "findme")]))
        '((h1 [(class "findme")]))))

     (test-suite
      "xexpr-select-text"

      (test-case "finds the text inside elements"
        (define tree
          '(div
            (h1 "first")
            (h1 "second")))

        (check-equal?
         (xexpr-select-text tree h1)
         '("first" "second"))))))))
