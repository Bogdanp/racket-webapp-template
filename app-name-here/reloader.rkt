#lang racket/base

(require racket/file
         racket/function
         racket/list
         racket/rerequire
         racket/string)

(provide start-reloader)

(define-logger reloader)

(define (track-file? p)
  (define-values (folder filename _) (split-path p))
  (and (not (directory-exists? p))
       (not (string-prefix? (path->string filename) "."))))

(define (watch path handler)
  (define (collect-tracked-files)
    (map simplify-path (find-files track-file? path)))

  (let loop ([tracked-files (collect-tracked-files)])
    (sync
     (handle-evt
      (filesystem-change-evt path)
      (lambda (e)
        (loop (collect-tracked-files))))

     (handle-evt
      (apply choice-evt (filter-map
                         (lambda (p)
                           (and (file-exists? p)
                                (handle-evt (filesystem-change-evt p) (const p))))
                         tracked-files))
      (lambda (p)
        (handler p)
        (loop tracked-files))))))

(define (start-reloader #:path path
                        #:handler handler)
  (thread
   (lambda ()
     (log-reloader-debug "starting reloader for path ~v" path)
     (watch path (lambda (changed-path)
                   (log-reloader-debug "detected change in ~v" changed-path)
                   (handler changed-path))))))
