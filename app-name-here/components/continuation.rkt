#lang racket/base

(require koyo/profiler
         libuuid
         racket/contract
         racket/function
         web-server/http
         web-server/managers/manager
         web-server/servlet/servlet-structs
         web-server/servlet/web)

;; The main advantage to using continuations in a web context is that
;; they let you avoid the inversion of control problem.  However, that
;; comes at a price.  If someone guesses the URL of one of a user's
;; continuations then that person can essentially take over their
;; session.
;;
;; The decorators exposed by this module help prevent those sorts of
;; issues by pinning each continuation to a single web browser
;; session.

(provide
 current-continuation-mismatch-handler
 protect-continuation
 wrap-protect-continuations

 send/suspend/protect
 send/forward/protect
 send/suspend/dispatch/protect
 redirect/get/protect
 redirect/get/forget/protect)

(define continuation-key-cookie-name "_k")

(define current-continuation-key
  (make-parameter #f))

(define/contract current-continuation-mismatch-handler
  (parameter/c (-> request? response?))
  (make-parameter
   (lambda (req)
     (response/xexpr
      #:code 403
      #:message #"Forbidden"
      '(h1 "Forbidden")))))

(define (find-continuation-key cookies)
  (cond
    [(findf (compose1 (curry string=? continuation-key-cookie-name) client-cookie-name) cookies)
     => client-cookie-value]

    [else #f]))

(define/contract (protect-request req)
  (-> request? request?)
  (with-timing 'continuation "protect-request"
    (cond
      [(equal? (find-continuation-key (request-cookies req))
               (current-continuation-key))
       req]

      [else
       (raise (make-exn:fail:servlet-manager:no-instance
               "Continuation key mismatch."
               (current-continuation-marks)
               (current-continuation-mismatch-handler)))])))

(define/contract ((protect-continuation k) req)
  (-> (-> request? response?) (-> request? response?))
  (k (protect-request req)))

(define/contract ((wrap-protect-continuations handler) req)
  (-> (-> request? response?) (-> request? response?))
  (with-timing 'continuation "wrap-protect-continuations"
    (define continuation-key
      (or (find-continuation-key (request-cookies req))
          (uuid-generate)))

    (parameterize ([current-continuation-key continuation-key])
      (define the-cookie (make-cookie #:path "/"
                                      #:http-only? #t
                                      continuation-key-cookie-name
                                      continuation-key))
      (define the-response (handler req))
      (struct-copy response the-response [headers (cons
                                                   (cookie->header the-cookie)
                                                   (response-headers the-response))]))))

(define/contract (send/suspend/protect f)
  (-> (-> string? can-be-response?) request?)
  (protect-request (send/suspend f)))

(define/contract (send/forward/protect f)
  (-> (-> string? can-be-response?) request?)
  (protect-request (send/forward f)))

(define/contract (send/suspend/dispatch/protect f)
  (-> (-> (-> (-> request? any) string?) can-be-response?) any)
  (send/suspend/dispatch
   (lambda (embed/url)
     (f (compose1 embed/url protect-continuation)))))

(define/contract (redirect/get/protect #:headers [hs null])
  (->* () (#:headers (listof header?)) request?)
  (send/suspend/protect
   (lambda (k-url)
     (redirect-to k-url #:headers hs))))

(define/contract (redirect/get/forget/protect #:headers [hs null])
  (->* () (#:headers (listof header?)) request?)
  (send/forward/protect
   (lambda (k-url)
     (redirect-to k-url #:headers hs))))

(module+ test
  (require racket/match
           racket/string
           rackunit
           rackunit/text-ui
           "testing.rkt")

  (run-tests
   (test-suite
    "continuation"

    (test-suite
     "protect-continuation"

     (test-case "ensures the key in the request matches the current key"
       (define handler
         (protect-continuation
          (lambda (req)
            (response/xexpr '(h1 "Hello")))))

       (check-exn
        exn:fail:servlet-manager:no-instance?
        (lambda ()
          (parameterize ([current-continuation-key "sekrit"])
            (handler (make-test-request)))))

       (check-equal?
        (response-code (parameterize ([current-continuation-key "sekrit"])
                         (handler (make-test-request #:headers (list (make-header #"Cookie" #"_k=sekrit"))))))
        200)))

    (test-suite
     "wrap-protect-continuations"

     (test-case "adds a continuation key to the response"
       (define handler
         (wrap-protect-continuations (lambda (req)
                                       (response/xexpr '(h1 "Hello")))))

       (define-values (header value)
         (let ([cookie-header (car (response-headers (handler (make-test-request))))])
           (values (bytes->string/utf-8 (header-field cookie-header))
                   (bytes->string/utf-8 (header-value cookie-header)))))

       (check-equal? header "Set-Cookie")
       (check-true (and (string-contains? value "_k=")
                        (string-contains? value "Path=/;")
                        (string-contains? value "HttpOnly"))))))))
