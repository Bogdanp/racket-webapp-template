#lang racket/base

(require koyo/profiler
         koyo/session
         libuuid
         racket/contract/base
         threading
         web-server/http
         "testing.rkt")

(provide
 (contract-out
  [current-csrf-error-handler (parameter/c (-> request? response?))]
  [current-csrf-token (parameter/c (or/c false/c string?))]
  [current-csrf-token-generator (parameter/c (-> string?))]
  [current-request->csrf-token (parameter/c (-> request? (or/c false/c string?)))]
  [wrap-csrf (-> session-manager? (-> (-> request? response?)
                                      (-> request? response?)))]))

(define (request-protected? req)
  (not (member (request-method req) '(#"GET" #"HEAD" #"OPTIONS" #"TRACE"))))

(define (request->csrf-token req)
  (or (and~> (headers-assq* #"x-csrf-token" (request-headers/raw req))
             (header-value)
             (bytes->string/utf-8))
      (and~> (bindings-assq #"csrf-token" (request-bindings/raw req))
             (binding:form-value)
             (bytes->string/utf-8))))

(define (error-handler req)
  (response/xexpr
   #:preamble #"<doctype html>"
   #:code 403
   #:message #"Forbidden"
   '(div
     (h1 "Error")
     (p "Invalid CSRF token."))))

(define current-csrf-token
  (make-parameter #f))

(define current-csrf-token-generator
  (make-parameter uuid-generate))

(define current-request->csrf-token
  (make-parameter request->csrf-token))

(define current-csrf-error-handler
  (make-parameter error-handler))

(define (((wrap-csrf sessions) handler) req)
  (with-timing 'csrf "wrap-csrf"
    (session-manager-update! sessions 'csrf.token (lambda (token-from-session)
                                                    (or token-from-session ((current-csrf-token-generator)))) #f)

    (define csrf-token
      (session-manager-ref sessions 'csrf.token))

    (parameterize [(current-csrf-token csrf-token)]
      (cond
        [(request-protected? req)
         (if (equal? ((current-request->csrf-token) req) csrf-token)
             (handler req)
             (error-handler req))]

        [else
         (handler req)]))))

(module+ test
  (require component
           racket/file
           racket/match
           racket/port
           rackunit
           rackunit/text-ui
           (prefix-in config: "../config.rkt"))

  (define sessions
    (component-start
     ((make-session-manager-factory #:cookie-name config:session-cookie-name
                                    #:shelf-life config:session-shelf-life
                                    #:secret-key config:session-secret-key
                                    #:store (make-memory-session-store #:file-path (make-temporary-file))))))

  (define wrapper
    (compose1 (wrap-session sessions)
              (wrap-csrf sessions)))

  (run-tests
   (test-suite
    "csrf"

    (test-suite
     "request->csrf-token"

     (test-case "returns #f if there is no token in the request"
       (check-false (request->csrf-token (make-test-request))))

     (test-case "extracts CSRF tokens from headers"
       (check-equal?
        (request->csrf-token (make-test-request #:headers (list (make-header #"x-csrf-token" #"a"))))
        "a"))

     (test-case "extracts CSRF tokens from bindings"
       (check-equal?
        (request->csrf-token (make-test-request #:bindings (list (make-binding:form #"csrf-token" #"a"))))
        "a")))

    (test-suite
     "request-protected?"

     (test-case "returns #t when the current request should be protected"
       (check-false (request-protected? (make-test-request)))
       (for ([method '(#"DELETE" #"PATCH" #"POST" #"PUT" #"RANDOM")])
         (check-not-false (request-protected? (make-test-request #:method method))))))

    (test-suite
     "wrap-csrf"

     (test-case "does nothing for GET requests except generate the token"
       ((wrapper (lambda (req)
                   (check-not-false (current-csrf-token))
                   (response/xexpr '(div))))
        (make-test-request)))

     (test-case "fails the request if it does not contain the expected token"
       (check-equal?
        (response-code
         ((wrapper (lambda (req)
                     (response/xexpr '(div))))
          (make-test-request #:method "POST")))
        403))

     (test-case "passes the request if it contains the expected token"
       (define response-1
         ((wrapper (lambda (req)
                     (response/xexpr (current-csrf-token))))
          (make-test-request)))

       (match-define (list _ session-id)
         (regexp-match #px"_sid=([^;]+);" (bytes->string/utf-8 (header-value (car (response-headers response-1))))))

       (define session-id-cookie
         (string->bytes/utf-8 (format "~a=~a" config:session-cookie-name session-id)))

       (define csrf-token
         (string->bytes/utf-8
          (call-with-output-string (response-output response-1))))

       (define response-2
         ((wrapper (lambda (req)
                     (response/xexpr '(p "ok"))))
          (make-test-request #:method "POST"
                             #:headers (list (make-header #"cookie" session-id-cookie)
                                             (make-header #"x-csrf-token" csrf-token)))))

       (check-equal? (response-code response-2) 200)
       (check-equal? (call-with-output-string (response-output response-2)) "<p>ok</p>"))))))
