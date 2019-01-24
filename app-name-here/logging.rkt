#lang racket/base

(require gregor
         racket/contract/base
         racket/format
         racket/match
         (prefix-in config: "config.rkt"))

(provide (contract-out
          [start-logger (->* () (port?) thread?)]))

(define (start-logger [output-port (current-error-port)])
  (file-stream-buffer-mode output-port 'line)

  (define log-receiver
    (make-log-receiver
     (current-logger)
     config:log-level 'app
     config:log-level 'server
     config:log-level 'system
     config:log-level 'reloader))

  (thread (lambda ()
            (let loop ()
              (match-define (vector level message _ _) (sync log-receiver))
              (fprintf output-port
                       "[~a] [~a] ~a\n"
                       (~t (now) "yyyy-MM-dd HH:mm:ss")
                       (~a level #:align 'right #:width 7)
                       message)
              (loop)))))
