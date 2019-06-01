#lang racket/base

(require koyo/profiler
         racket/contract
         racket/string
         web-server/http
         (prefix-in config: "../config.rkt"))

(provide wrap-cors)

(define/contract ((wrap-cors handler) req)
  (-> (-> request? response?)
      (-> request? response?))

  (with-timing 'cors "wrap-cors"
    (cond
      [(bytes=? (request-method req) #"OPTIONS")
       (response/full 200 #"OK" (current-seconds) #f OPTIONS-HEADERS null)]

      [else
       (define resp (handler req))
       (define headers (cons ALLOW-ORIGIN-HEADER (response-headers resp)))
       (struct-copy response resp [headers headers])])))

(define ALLOW-ORIGIN-HEADER
  (make-header #"Access-Control-Allow-Origin"
               (string->bytes/utf-8
                (format "~a://~a" config:url-scheme config:url-host))))

(define ALLOW-METHODS-HEADER
  (make-header #"Access-Control-Allow-Methods"
               (string->bytes/utf-8
                (string-join '("HEAD" "DELETE" "GET" "PATCH" "POST" "PUT" "OPTIONS") ","))))

(define ALLOW-HEADERS-HEADER
  (make-header #"Access-Control-Allow-Headers" #"*"))

(define ALLOW-CREDENTIALS-HEADER
  (make-header #"Access-Control-Allow-Credentials" #"true"))

(define MAX-AGE-HEADER
  (make-header #"Access-Control-Max-Age" #"86400"))

(define OPTIONS-HEADERS
  (list ALLOW-ORIGIN-HEADER
        ALLOW-METHODS-HEADER
        ALLOW-HEADERS-HEADER
        ALLOW-CREDENTIALS-HEADER
        MAX-AGE-HEADER))
