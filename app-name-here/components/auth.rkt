#lang racket/base

(require component
         net/url
         racket/contract/base
         racket/function
         racket/match
         threading
         web-server/http
         "profiler.rkt"
         "session.rkt"
         "url.rkt"
         "user.rkt")

(provide
 (contract-out
  [exn:fail:auth-manager? (-> any/c boolean?)]
  [exn:fail:auth-manager:unverified? (-> any/c boolean?)]
  [current-user (parameter/c (or/c false/c user?))]
  [struct auth-manager ([session-manager session-manager?]
                        [user-manager user-manager?])]
  [auth-manager-login! (-> auth-manager? string? string? (or/c false/c void?))]
  [auth-manager-logout! (-> auth-manager? void?)]
  [wrap-auth-required (-> auth-manager? (-> (-> request? response?) (-> request? response?)))]))

(define current-user
  (make-parameter #f))

(struct exn:fail:auth-manager exn:fail ())
(struct exn:fail:auth-manager:unverified exn:fail:auth-manager ())

(struct auth-manager (session-manager user-manager)
  #:methods gen:component
  [(define component-start identity)
   (define component-stop identity)])

(define (auth-manager-login! am username password)
  (match (user-manager-login (auth-manager-user-manager am) username password)
    [#f #f]
    [(user id _ _ verified? _ _ _)
     (unless verified?
       (raise (exn:fail:auth-manager:unverified "this user is not verified" (current-continuation-marks))))

     (session-manager-set! (auth-manager-session-manager am) 'uid (number->string id))]))

(define (auth-manager-logout! am)
  (session-manager-remove! (auth-manager-session-manager am) 'uid))

(define (((wrap-auth-required am) handler) req)
  (with-timing 'auth "wrap-auth-required"
    (define user
      (and~>> (session-manager-ref (auth-manager-session-manager am) 'uid #f)
              (string->number)
              (user-manager-lookup/id (auth-manager-user-manager am))))

    (if user
        (parameterize ([current-user user])
          (handler req))
        (redirect-to (make-application-url "login" #:query `((return . ,(url->string (request-uri req)))))))))


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
    [sessions (make-session-manager #:cookie-name config:session-cookie-name
                                    #:shelf-life config:session-shelf-life
                                    #:secret-key config:session-secret-key
                                    #:store (make-memory-session-store))]
    [users (db) user-manager])

  (run-tests
   (test-suite
    "auth-manager"
    #:before
    (lambda _
      (system-start test-system)
      (with-database-connection [conn (system-get test-system 'db)]
        (query-exec conn "truncate table users"))

      (define users (system-get test-system 'users))
      (user-manager-create! users "bogdan" "hunter2"))

    #:after
    (lambda _
      (system-stop test-system))

    (test-suite
     "auth-manager-login"

     (test-case "returns #f if the user does not exist"
       (check-false
        (auth-manager-login! (system-get test-system 'auth) "idontexist" "hunter2")))

     (test-case "returns #f if the password is wrong"
       (check-false
        (auth-manager-login! (system-get test-system 'auth) "bogdan" "invalid")))

     (test-case "fails if the user is not verified"
       (check-exn
        exn:fail:auth-manager:unverified?
        (lambda _
          (auth-manager-login! (system-get test-system 'auth) "bogdan" "hunter2"))))))))
