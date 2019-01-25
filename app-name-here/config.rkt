#lang racket/base

(provide (all-defined-out))

(define (getopt name [default #f])
  (or (getenv (string-append "APP_NAME_HERE_" (string-upcase name))) default))

(define debug
  (equal? (getopt "DEBUG") "x"))

(define version
  (getopt "VERSION" "dev"))

(define log-level
  (string->symbol (getopt "LOG_LEVEL" "info")))

(define http-host
  (getopt "HTTP_HOST" "127.0.0.1"))

(define http-port
  (string->number (getopt "PORT" "8000")))
