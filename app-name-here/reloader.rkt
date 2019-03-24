#lang racket/base

(require racket/contract/base
         racket/file
         racket/function
         racket/list
         racket/match
         racket/path
         racket/rerequire
         racket/set
         racket/string)

(provide
 (contract-out
  [start-reloader (->* (#:path path-string?
                        #:handler (-> path-string? any/c)) thread?)]))

(define-logger reloader)

(define (track-file? p)
  (equal? (path-get-extension p) #".rkt"))

(define (watch path handler)
  (define (collect-tracked-files)
    (map simplify-path (find-files track-file? path)))

  (let loop ([tracked-files (collect-tracked-files)])
    (parameterize ([current-custodian (make-custodian)])
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
          (loop tracked-files)))))))

(define (start-reloader #:path path
                        #:handler handler)
  (thread
   (lambda ()
     (log-reloader-debug "starting reloader for path ~v" (path->string path))
     (watch path (lambda (changed-path)
                   (log-reloader-debug "detected change in ~v" (path->string changed-path))
                   (handler changed-path))))))

(define (build-dependents-tree mod)
  (define (local? mpi)
    (define-values (name _)
      (module-path-index-split mpi))

    (string? name))

  (define (find-dependencies mod)
    (for*/fold ([dependencies null])
               ([phase (module->imports mod)]
                [dependency (cdr phase)]
                #:when (local? dependency))
      (cons (resolved-module-path-name (module-path-index-resolve dependency)) dependencies)))

  (let loop ([dependents (hash)]
             [mods (list mod)]
             [seen (set)])
    (match mods
      [(list)
       dependents]

      [(list (? (curry set-member? seen)) mods ...)
       (loop dependents mods seen)]

      [(list mod mods ...)
       (parameterize ([current-load-relative-directory (simplify-path (build-path mod 'up))])
         (define dependencies (find-dependencies mod))
         (define dependents*
           (for/fold ([dependents dependents])
                     ([dependency dependencies])
             (hash-update dependents dependency (curry cons mod) null)))

         (loop dependents* (append mods dependencies) (set-add seen mod)))])))

(provide
 (contract-out
  [touch-dependents (-> path-string? path-string? void?)]))

(define (find-dependents root mod)
  (define dependents-tree
    (build-dependents-tree (simplify-path root)))

  (let loop ([dependents null]
             [modules (list (simplify-path mod))])
    (match modules
      [(list)
       (set->list (list->set dependents))]

      [(list mod mods ...)
       (define dependents* (hash-ref dependents-tree mod null))
       (loop (append dependents dependents*)
             (append dependents* mods))])))

(define (touch-dependents root mod)
  (for ([path (find-dependents root mod)])
    (file-or-directory-modify-seconds path (current-seconds))))
