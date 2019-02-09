#lang racket/base

(require (for-syntax racket/base)
         racket/contract/base
         racket/function
         racket/match
         racket/runtime-path
         racket/string
         srfi/29
         web-server/http)

(provide
 (contract-out
  [translate (->* (symbol?) #:rest (listof string?) string?)]
  [wrap-browser-locale (-> (-> request? response?) (-> request? response?))]))

(define-runtime-path locales-path
  (build-path 'up 'up "resources" "locales"))

(define locales
  (for*/list ([language-dir (directory-list locales-path)]
              [country-file (directory-list (build-path locales-path language-dir))])
    (define language (string->symbol (path->string language-dir)))
    (define country (string->symbol (path->string (path-replace-extension country-file ""))))
    (define specifier (list 'app-name-here language country))
    (declare-bundle! specifier (call-with-input-file (build-path locales-path language-dir country-file) read))
    (cdr specifier)))

(define (translate message-name . args)
  (cond
    [(localized-template 'app-name-here message-name)
     => (lambda (message)
          (apply format message args))]

    [else (symbol->string message-name)]))

(define (language-header->locale header)
  (define specs
    (for/list ([spec (string-split header ",")])
      (match-define (list _ language country weight)
        (regexp-match #px"\\s*([^-]+)(?:-([^\\s;]+))?(?:;q=([\\d]+))?\\s*" spec))

      (cons (list (string->symbol (string-downcase language))
                  (string->symbol (string-downcase (or country language))))
            (string->number (or weight "1")))))

  (define (key a b)
    (> (cdr a) (cdr b)))

  (for/first ([spec (sort specs key)]
              #:when (member (car spec) locales))
    (car spec)))

(define ((wrap-browser-locale handler) req)
  (define accept-language
    (bytes->string/utf-8
     (cond
       [(headers-assq* #"accept-language" (request-headers/raw req)) => header-value]
       [else #"en-US"])))

  (define locale
    (or (language-header->locale accept-language) '(en us)))

  (parameterize ([current-language (car locale)]
                 [current-country (cadr locale)])
    (handler req)))

(module+ test
  (require rackunit
           rackunit/text-ui)

  (run-tests
   (test-suite
    "l10n"

    (test-suite
     "language-header->locale"

     (check-equal? (language-header->locale "") #f)
     (check-equal? (language-header->locale "en") #f)
     (check-equal? (language-header->locale "en-US") '(en us))
     (check-equal? (language-header->locale "en, en-US;q=0.5, ro-RO;q=1") '(ro ro))
     (check-equal? (language-header->locale "ro, en-GB;q=0.3, en-US;q=0.1, ro-RO;q=0.5") '(ro ro))))))
