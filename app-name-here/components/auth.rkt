#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         component
         koyo/profiler
         koyo/session
         net/url
         racket/contract
         racket/function
         racket/match
         racket/string
         threading
         web-server/dispatch
         web-server/dispatchers/dispatch
         web-server/http
         "url.rkt"
         "user.rkt")

;; Manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 current-user

 make-auth-manager
 auth-manager?
 auth-manager-login!
 auth-manager-logout!
 wrap-auth-required

 exn:fail:auth-manager?
 exn:fail:auth-manager:unverified?)

(define/contract current-user
  (parameter/c (or/c false/c user?))
  (make-parameter #f))

(struct exn:fail:auth-manager exn:fail ())
(struct exn:fail:auth-manager:unverified exn:fail:auth-manager ())

(struct auth-manager (session-manager user-manager)
  #:methods gen:component
  [(define component-start identity)
   (define component-stop identity)])

(define/contract (make-auth-manager sessions users)
  (-> session-manager? user-manager? auth-manager?)
  (auth-manager sessions users))

(define/contract (auth-manager-login! am username password)
  (-> auth-manager? non-empty-string? non-empty-string? (or/c false/c user?))
  (match (user-manager-login (auth-manager-user-manager am) username password)
    [#f #f]
    [(and (user id _ _ verified? _ _ _) user)
     (unless verified?
       (raise (exn:fail:auth-manager:unverified "this user is not verified" (current-continuation-marks))))

     (begin0 user
       (session-manager-set! (auth-manager-session-manager am) 'uid (number->string id)))]))

(define/contract (auth-manager-logout! am)
  (-> auth-manager? void?)
  (session-manager-remove! (auth-manager-session-manager am) 'uid))

(define/contract (((wrap-auth-required am req-roles) handler) req)
  (-> auth-manager?
      (-> request? (listof symbol?))
      (-> (-> request? response?)
          (-> request? response?)))

  (with-timing 'auth "wrap-auth-required"
    (define roles (req-roles req))
    (cond
      [(null? roles)
       (handler req)]

      [else
       (define user
         (and~>> (session-manager-ref (auth-manager-session-manager am) 'uid #f)
                 (string->number)
                 (user-manager-lookup/id (auth-manager-user-manager am))))

       ;; NOTE: Roles are not actually checked beyond this point.  If you
       ;; implement roles other than 'user then you're going to want to
       ;; change this.
       (if user
           (parameterize ([current-user user])
             (handler req))
           (redirect-to (make-application-url "login" #:query `((return . ,(url->string (request-uri req)))))))])))

(module+ test
  (require db
           rackunit
           rackunit/text-ui
           (prefix-in config: "../config.rkt")
           "database.rkt")

  (define-system test
    [auth (sessions users) auth-manager]
    [db (make-database #:database config:test-db-name
                       #:username config:test-db-username
                       #:password config:test-db-password)]
    [sessions (make-session-manager-factory #:cookie-name config:session-cookie-name
                                            #:shelf-life config:session-shelf-life
                                            #:secret-key config:session-secret-key
                                            #:store (make-memory-session-store))]
    [users (db) user-manager])

  (define auth #f)
  (define users #f)
  (define user #f)

  (run-tests
   (test-suite
    "auth-manager"
    #:before
    (lambda _
      (system-start test-system)
      (with-database-connection [conn (system-get test-system 'db)]
        (query-exec conn "truncate table users"))

      (set! auth (system-get test-system 'auth))
      (set! users (system-get test-system 'users))
      (set! user (user-manager-create! users "bogdan" "hunter2")))

    #:after
    (lambda _
      (system-stop test-system))

    (test-suite
     "auth-manager-login"

     (test-case "returns #f if the user does not exist"
       (check-false
        (auth-manager-login! auth "idontexist" "hunter2")))

     (test-case "returns #f if the password is wrong"
       (check-false
        (auth-manager-login! auth "bogdan" "invalid")))

     (test-case "fails if the user is not verified"
       (check-exn
        exn:fail:auth-manager:unverified?
        (lambda _
          (auth-manager-login! auth "bogdan" "hunter2"))))

     (test-case "returns the user when verified"
       (parameterize ([current-session-id "fake"])
         (user-manager-verify users (user-id user) (user-verification-code user))
         (check-equal? (user-id user)
                       (user-id (auth-manager-login! auth "bogdan" "hunter2")))))))))


;; Syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 dispatch-rules+roles)

(define (default-else req)
  (next-dispatcher))

(begin-for-syntax
  (define-syntax-class dispatch-fun
    (pattern fun:id
             #:attr name #''fun)

    (pattern (fun:dispatch-fun e ...)
             #:attr name (attribute fun.name))))

(define-syntax (dispatch-rules+roles stx)
  (syntax-parse stx
    #:literals (else)
    [(_
      [pat
       (~alt (~optional (~seq #:method method) #:defaults ([method #'"get"]))
             (~optional (~seq #:roles (role:id ...)) #:defaults ([(role 1) null]))
             (~optional (~seq #:name name) #:defaults ([name #'#f]))) ...
       fun:dispatch-fun]
      ...
      [else else-fun])
     (with-syntax ([(fun-reverse-name ...)
                    (map (lambda (given-name default-name)
                           (if (syntax->datum given-name)
                               given-name
                               default-name))
                         (syntax->list #'(name ...))
                         (syntax->list #'(fun.name ...)))])
       (syntax/loc stx
         (let-values ([(dispatch _)
                       (dispatch-rules
                        [pat #:method method fun] ...
                        [else else-fun])]
                      [(reverse-uri)
                       (dispatch-url
                        [pat fun-reverse-name] ...)]
                      [(roles)
                       (dispatch-case
                        [pat #:method method (const '(role ...))] ...
                        [else (const null)])])
           (values dispatch reverse-uri roles))))]

    [(_
      [pat
       (~alt (~optional (~seq #:method method) #:defaults ([method #'"get"]))
             (~optional (~seq #:roles (role:id ...)) #:defaults ([(role 1) null]))
             (~optional (~seq #:name name) #:defaults ([name #'#f]))) ...
       fun] ...)
     (syntax/loc stx
       (dispatch-rules+roles
        [pat #:method method #:roles (role ...) #:name name fun] ...
        [else default-else]))]))


(module+ test
  (require "./testing.rkt")

  (run-tests
   (test-suite
    "auth"

    (test-suite
     "dispatch-rules+roles"

     (test-case "generates a function from requests to lists of roles"
       (define-values (dispatch reverse-uri req-roles)
         (dispatch-rules+roles
          [("") (const "home")]
          [("lounge") #:roles (user) (const "lounge")]
          [("admin") #:roles (admin) (const "admin")]))

       (check-equal? (req-roles (make-test-request)) '())
       (check-equal? (req-roles (make-test-request #:path "/invalid")) '())
       (check-equal? (req-roles (make-test-request #:path "/lounge")) '(user))
       (check-equal? (req-roles (make-test-request #:path "/lounge" #:method "POST")) '())
       (check-equal? (req-roles (make-test-request #:path "/admin")) '(admin)))

     (test-case "generates a reverse-uri function based upon symbols"
       (define (home-page req)
         'home)

       (define ((order-page orders) req id)
         'order)

       (define ((admin:edit-order-page orders) req id)
         'edit-order)

       (define-values (dispatch reverse-uri req-roles)
         (dispatch-rules+roles
          [("") home-page]
          [("orders" (integer-arg)) (order-page 'order-manager)]
          [("admin" "orders" (integer-arg))
           #:name 'edit-order-page
           (admin:edit-order-page 'order-manager)]))

       (check-equal? (reverse-uri 'home-page) "/")
       (check-equal? (reverse-uri 'order-page 1) "/orders/1")
       (check-equal? (reverse-uri 'edit-order-page 1) "/admin/orders/1"))))))
