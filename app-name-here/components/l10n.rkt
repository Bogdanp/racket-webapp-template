#lang racket/base

(require (for-syntax racket/base)
         gregor
         koyo/profiler
         koyo/session
         racket/contract
         racket/match
         racket/runtime-path
         racket/string
         srfi/29
         web-server/http)

;; Translate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 translate
 localize-date)

(define-runtime-path locales-path
  (build-path 'up 'up "resources" "locales"))

(define locales
  (for*/list ([language-dir (directory-list locales-path)]
              [country-file (directory-list (build-path locales-path language-dir))])
    (define language (string->symbol (path->string language-dir)))
    (define country (string->symbol (path->string (path-replace-extension country-file ""))))
    (define specifier (list 'matchacha language country))
    (declare-bundle! specifier (call-with-input-file (build-path locales-path language-dir country-file) read))
    (cons language country)))

(define/contract (translate message-name . args)
  (-> symbol? any/c ... string?)
  (cond
    [(localized-template 'matchacha message-name)
     => (lambda (message)
          (apply format message args))]

    [else (symbol->string message-name)]))

(define/contract (localize-date d)
  (-> date-provider? string?)
  (match (current-language)
    ['ro (~t d "dd MMMM, yyyy")]
    [_   (~t d "MMMM dd, yyyy")]))


;; Middleware ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 wrap-browser-locale)

(define accept-language-header
  #"accept-language")

(define language-spec-re
  #px"\\s*([^-]+)(?:-([^\\s;]+))?(?:;q=([\\d]+))?\\s*")

(define/contract (((wrap-browser-locale sessions) handler) req)
  (-> session-manager? (-> (-> request? response?)
                           (-> request? response?)))

  (with-timing 'http "wrap-browser-locale"
    (define accept-language
      (bytes->string/utf-8
       (cond
         [(headers-assq* accept-language-header (request-headers/raw req)) => header-value]
         [else #"en-US"])))

    (define user-language
      (session-manager-ref sessions 'l10n.lang accept-language))

    (match-define (cons language country)
      (or (language-header->locale user-language) '(en . us)))

    (parameterize ([current-language language]
                   [current-country country]
                   [current-locale (format "~a_~a.UTF-8" language country)])
      (handler req))))

(define (make-locale-pair language country)
  (cons (string->symbol (string-downcase language))
        (string->symbol (string-downcase (or country language)))))

(define (language-header->locale header)
  (define specs
    (for/list ([spec (string-split header ",")])
      (match-define (list _ language country weight)
        (regexp-match language-spec-re spec))

      (cons (make-locale-pair language country)
            (string->number (or weight "1")))))

  (for/first ([spec (sort specs > #:key cdr)] #:when (member (car spec) locales))
    (car spec)))

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
     (check-equal? (language-header->locale "en-US") '(en . us))
     (check-equal? (language-header->locale "en, en-US;q=0.5, ro-RO;q=1") '(ro . ro))
     (check-equal? (language-header->locale "ro, en-GB;q=0.3, en-US;q=0.1, ro-RO;q=0.5") '(ro . ro))))))
