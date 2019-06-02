#lang racket/base

(require racket/match
         racket/path
         web-server/http
         "profiler.rkt")

(provide
 current-preload-dependencies
 make-preload-headers
 wrap-preload)

(define current-preload-dependencies
  (make-parameter null))

(define (path-remove-query p)
  (regexp-replace #rx"\\?.*" p ""))

(define (path->preload-as p)
  (match (path-get-extension (path-remove-query p))
    [#".css" "style"]
    [#".js"  "script"]
    [#".mjs" "script"]
    [_       #f]))

(define (make-preload-headers)
  (with-timing 'preload "make-preload-headers"
    (for/list ([path (current-preload-dependencies)] #:when (path->preload-as path))
      (header #"Link" (string->bytes/utf-8 (format "<~a>; rel=preload; as=~a" path (path->preload-as path)))))))

(define ((wrap-preload handler) req)
  (with-timing 'preload "wrap-preload"
    (parameterize ([current-preload-dependencies null])
      (handler req))))