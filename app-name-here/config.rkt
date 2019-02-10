#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/string
         web-server/http/id-cookie)

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

(define-option {test-db-name "app_name_here_tests"})
(define-option {test-db-username "app_name_here"})
(define-option {test-db-password "app_name_here"})
(define-option {test-db-host "127.0.0.1"})
(define-option {test-db-port "5432"}
  (string->number test-db-port))

(define-option {session-cookie-name "_sid"})
(define-option {session-shelf-life "86400"}
  (string->number session-shelf-life))
(define-option {session-secret-key-path "/tmp/app-name-here-secret-key"})
(define-option {session-secret-key #f}
  (or session-secret-key (make-secret-salt/file session-secret-key-path)))

(define-option {postmark-token #f})

(define-option {product-name "AppNameHere"})
(define-option {company-name "AppNameHere"})
(define-option {company-address ""})
(define-option {support-name "Bot Botterson"})
(define-option {support-email "support@app-name-here.com"})
