#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [box-swap! (-> box? (-> any/c any/c) void?)]))

(define (box-swap! b f)
  (let loop ([v (unbox b)])
    (unless (box-cas! b v (f v))
      (loop (unbox b)))))
