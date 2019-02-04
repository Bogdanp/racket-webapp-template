#lang racket/base

(require racket/match
         racket/path
         web-server/http)

(provide
 current-preload-dependencies
 make-preload-headers)

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
  (for/list ([path (current-preload-dependencies)])
    (header #"Link" (string->bytes/utf-8 (format "<~a>; rel=preload; as=~a" path (path->preload-as path))))))
