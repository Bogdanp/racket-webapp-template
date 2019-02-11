#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     (prefix-in config: "../config.rkt"))
         net/url
         racket/contract/base
         racket/format
         racket/function
         racket/match
         web-server/http
         xml
         (prefix-in config: "../config.rkt")
         "../util.rkt")

(provide
 with-timing

 (contract-out
  [profile? (-> any/c boolean?)]
  [make-profile (-> profile?)]
  [current-profile (parameter/c profile?)]
  [write-profile (->* () (profile? output-port?) void?)]
  [wrap-profiler (-> (-> request? response?) (-> request? response?))]))

(struct timing (id parent label description duration))
(struct profile (data))

(define (make-profile)
  (profile (box (list 0 null))))

(define (profile-next-id! profile)
  (let ([next-id #f])
    (box-swap! (profile-data profile)
               (lambda (data)
                 (match-define (list id timings) data)
                 (set! next-id (add1 id))
                 (list next-id timings)))
    next-id))

(define (profile-add-timing profile timing)
  (box-swap! (profile-data profile)
             (lambda (data)
               (match-define (list id timings) data)
               (list id (cons timing timings)))))

(define (profile-find-roots profile)
  (match-define (list _ timings)
    (unbox (profile-data profile)))

  (reverse
   (filter (lambda (t)
             (= (timing-parent t) 0)) timings)))

(define (profile-find-children profile timing)
  (match-define (list _ timings)
    (unbox (profile-data profile)))

  (reverse
   (filter (lambda (t)
             (= (timing-id timing) (timing-parent t))) timings)))

(define current-profile
  (make-parameter (make-profile)))

(define current-profile-label
  (make-parameter 'root))

(define current-timing-id
  (make-parameter 0))

(define (time #:profile [profile (current-profile)]
              #:label [label (current-profile-label)]
              #:description description
              f)
  (define p  #f)
  (define id #f)
  (define st #f)
  (dynamic-wind
    (lambda ()
      (set! p  (current-timing-id))
      (set! id (profile-next-id! profile))
      (set! st (current-inexact-milliseconds))
      (current-timing-id id))
    (lambda ()
      (parameterize ([current-profile-label label])
        (f)))
    (lambda ()
      (define duration (- (current-inexact-milliseconds) st))
      (define current-timing (timing id p label description duration))
      (profile-add-timing profile current-timing)
      (current-timing-id p))))

(define-syntax (with-timing stx)
  (syntax-parse stx
    [(_ label description e ...+)
     (if config:profile
         #'(time
            #:label label
            #:description description
            (lambda () e ...))
         #'(begin e ...))]

    [(_ description e ...+)
     #'(with-timing (current-profile-label) description
         e ...)]))

(define (write-profile [profile (current-profile)]
                       [out (current-output-port)])
  (define roots
    (profile-find-roots profile))

  (define (format-duration duration)
    (~a (~r duration #:precision '(= 2)) "ms"))

  (define (render-timing timing)
    (define toggle-id (format "uprofiler-timing-toggle-~a" (timing-id timing)))
    `(div
      ((class "uprofiler-timing"))
      (input
       ((class "uprofiler-timing-toggle")
        (type "checkbox")
        (id ,toggle-id)))
      (label
       ((class "uprofiler-timing-label")
        (for ,toggle-id))
       ,(symbol->string (timing-label timing)))
      (span ((class "uprofiler-timing-description")) (code ,(timing-description timing)))
      (span ((class "uprofiler-timing-duration")) ,(format-duration (timing-duration timing)))
      ,@(map render-timing (profile-find-children profile timing))))

  (unless (null? roots)
    (define content
      `(div
        ((class "uprofiler-content"))
        (input
         ((class "uprofiler-toggle")
          (id "uprofiler-toggle")
          (type "checkbox")))
        (label
         ((class "uprofiler-label")
          (for "uprofiler-toggle"))
         ,(format-duration (apply + (map timing-duration roots))))
        (div
         ((class "uprofiler-timings"))
         ,@(map render-timing roots)
         (label
          ((class "uprofiler-close")
           (for "uprofiler-toggle"))
          "Close"))))

    (write-xml/content (xexpr->xml content) out)))

(define ((wrap-profiler handler) req)
  (parameterize ([current-profile (make-profile)])
    (with-timing 'http (format "~a ~a" (request-method req) (url->string (request-uri req)))
      (handler req))))

(module+ test
  (require racket/list
           rackunit
           rackunit/text-ui)

  (run-tests
   (test-suite
    "profiler"

    (test-suite
     "time"

     (parameterize [(current-profile (make-profile))]
       (time #:description "GET /"
             (lambda ()
               (time #:label 'sql
                     #:description "SELECT * FROM users WHERE id = 1"
                     (lambda ()
                       (time #:description "SELECT * FROM profiles WHERE user_id = 1" void)
                       (time #:description "SELECT * FROM profiles WHERE user_id = 1" void)))))

       (match-define (list _ timings)
         (unbox (profile-data (current-profile))))

       (check-equal? (length timings) 4)
       (check-equal? (map timing-id timings) '(1 2 4 3))
       (check-equal? (map timing-parent timings) '(0 1 2 2))
       (check-equal? (map timing-label timings) '(root sql sql sql)))))))
