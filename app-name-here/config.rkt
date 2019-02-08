#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/string)

(define (symbol->option-name s)
  (string-append "APP_NAME_HERE_" (string-replace (string-upcase (symbol->string s)) "-" "_")))

(define-syntax (define-option stx)
  (syntax-parse stx
    [(_ {name:id})              #'(define-option {name #f})]
    [(_ {name:id} e:expr ...+)  #'(define-option {name #f} e ...)]
    [(_ {name:id default:expr}) #'(define-option {name default} name)]

    [(_ {name:id default:expr} e:expr ...+)
     #'(begin
         (define name
           (let ([name (or (getenv (symbol->option-name 'name)) default)])
             e ...))

         (provide name))]))

(define-option {version "dev"})
(define-option {debug}
  (equal? debug "x"))

(define-option {log-level "info"}
  (string->symbol log-level))

(define-option {http-host "127.0.0.1"})
(define-option {http-port "8000"}
  (string->number http-port))

(define-option {url-scheme "http"})
(define-option {url-host "127.0.0.1"})
(define-option {url-port "8000"})

(define-option {db-name "app_name_here"})
(define-option {db-username "app_name_here"})
(define-option {db-password "app_name_here"})
(define-option {db-host "127.0.0.1"})
(define-option {db-port "5432"}
  (string->number db-port))

(define-option {secret-key "supercalifragilisticexpialidocious"}
  (string->bytes/utf-8 secret-key))

(define-option {email-postmark-token #f})
(define-option {email-sender "bot@example.com"})
