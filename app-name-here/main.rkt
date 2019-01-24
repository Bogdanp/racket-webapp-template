#lang racket/base

(require racket/runtime-path)

(define-runtime-path here ".")

(module+ main
  (require racket/rerequire
           racket/string
           (prefix-in config: "config.rkt")
           "logging.rkt"
           "reloader.rkt")

  (define dynamic-module-path
    (build-path here "dynamic.rkt"))

  (when config:debug
    (void (dynamic-rerequire dynamic-module-path)))

  (define start (dynamic-require dynamic-module-path 'start))
  (define stop (dynamic-require dynamic-module-path 'stop))

  (void (start-logger))
  (start)

  (when config:debug
    (void
     (start-reloader
      #:path here
      #:handler (lambda (changed-path)
                  (with-handlers ([exn:fail?
                                   (lambda (e)
                                     (log-error "!!! failed to reload code !!!")
                                     ((error-display-handler) (exn-message e) e)
                                     (exit 7))])
                    (stop)
                    (when (string-suffix? (path->string changed-path) ".rkt")
                      (dynamic-rerequire dynamic-module-path #:verbosity 'none)
                      (set! start (dynamic-require dynamic-module-path 'start))
                      (set! stop (dynamic-require dynamic-module-path 'stop))
                      ((dynamic-require dynamic-module-path 'reload!)))
                    (start))))))

  (with-handlers ([exn:break? (lambda (e)
                                (stop)
                                (sync/enable-break (system-idle-evt)))])
    (sync/enable-break never-evt)))
