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
  [wrap-profiler (-> (-> request? response?) (-> request? response?))]))

(struct timing (id parent label description duration))
(struct profile (data))

(define (make-profile)
  (profile (box (list 0 null))))

(define (profile-next-id! profile)
  (let ([id #f])
    (box-swap! (profile-data profile)
               (lambda (data)
                 (match-define (list previous-id timings) data)
                 (set! id (add1 previous-id))
                 (list id timings)))
    id))

(define (profile-add-timing profile timing)
  (box-swap! (profile-data profile)
             (lambda (data)
               (match-define (list id timings) data)
               (list id (cons timing timings)))))

(define (profile-find-root profile)
  (match-define (list _ timings)
    (unbox (profile-data profile)))

  (and (not (null? timings)) (car timings)))

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

(define (time #:label [label (current-profile-label)]
              #:description description
              f)
  (define timing-id (profile-next-id! (current-profile)))
  (define start-time #f)
  (dynamic-wind
    (lambda ()
      (set! start-time (current-inexact-milliseconds)))
    (lambda ()
      (parameterize ([current-profile-label label]
                     [current-timing-id timing-id])
        (f)))
    (lambda ()
      (define duration (- (current-inexact-milliseconds) start-time))
      (profile-add-timing (current-profile)
                          (timing timing-id (current-timing-id) label description duration)))))

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

(define (render-profile profile out)
  (define root
    (profile-find-root profile))

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

  (when root
    (define content
      (xexpr->xml
       `(div
         ((class "uprofiler-content"))
         (input
          ((class "uprofiler-toggle")
           (id "uprofiler-toggle")
           (type "checkbox")))
         (label
          ((class "uprofiler-label")
           (for "uprofiler-toggle"))
          ,(format-duration (timing-duration root)))
         (div
          ((class "uprofiler-timings"))
          ,(render-timing root)
          (label
           ((class "uprofiler-close")
            (for "uprofiler-toggle"))
           "Close")))))

    (write-xml/content content out)))

(define ((wrap-profiler handler) req)
  (cond
    [config:profile
     (define profile (make-profile))
     (parameterize ([current-profile profile])
       (define resp
         (with-timing 'http (format "~a ~a" (request-method req) (url->string (request-uri req)))
           (handler req)))

       (cond
         [(bytes=? #"text/html" (subbytes (response-mime resp) 0 9))
          (struct-copy response resp [output (lambda (out)
                                               ((response-output resp) out)
                                               (render-profile profile out))])]

         [else resp]))]

    [else (handler req)]))

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
