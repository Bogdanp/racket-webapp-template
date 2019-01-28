#lang racket/base

(require component
         crypto
         crypto/argon2
         db
         gregor
         openssl/md5
         racket/contract
         racket/port
         racket/random
         racket/string
         sql
         struct-plus-plus
         threading
         "database.rkt")


;; user ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide user?)

;; https://password-hashing.net/argon2-specs.pdf
(define ARGON2-CONFIG
  '((p 8)    ;; parallelism, adjust according to number of cores
    (t 512)  ;; iterations, adjust based on duration
    (m 4096) ;; memory per p in kb, adjust based on available memory
    ))

(struct++ user
  ([id (or/c false/c exact-positive-integer?)]
   [username non-empty-string? string-downcase]
   [(password-hash #f) (or/c false/c non-empty-string?)]
   [(verified #f) boolean?]
   [(verification-code (generate-verification-code)) non-empty-string?]
   [(created-at (now)) moment?]
   [(updated-at (now)) moment?])
  #:transparent)

(define (generate-verification-code [strength 8192])
  (call-with-input-bytes (crypto-random-bytes strength) md5))

(define (set-user-password u p)
  (define password-hash
    (parameterize ([crypto-factories (list argon2-factory)])
      (pwhash 'argon2id (string->bytes/utf-8 p) ARGON2-CONFIG)))

  (set-user-password-hash u password-hash))

(define (user-password-valid? u p)
  (parameterize ([crypto-factories (list argon2-factory)])
    (pwhash-verify #f (string->bytes/utf-8 p) (user-password-hash u))))


;; user-manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 user-manager
 user-manager?
 user-manager-create-user
 user-manager-login)

(struct++ user-manager ([db connection?])
  #:transparent
  #:methods gen:component
  [(define (component-start a-user-manager) a-user-manager)
   (define (component-stop a-user-manager) a-user-manager)])

(define/contract (user-manager-create-user um username password)
  (-> user-manager? string? string? user?)
  (define user (set-user-password (user++ #:id #f #:username username) password))
  (define id
    (with-database-transaction [conn (user-manager-db um)]
      (query-exec conn (insert #:into users
                               #:set
                               [username ,(user-username user)]
                               [password_hash ,(user-password-hash user)]
                               [verification_code ,(user-verification-code user)]
                               [created_at ,(->sql-timestamp (user-created-at user))]
                               [updated_at ,(->sql-timestamp (user-updated-at user))]))
      (query-value conn (select (lastval)))))

  (set-user-id user id))

(define/contract (user-manager-login um username password)
  (-> user-manager? string? string? (or/c false/c user?))
  (define row
    (with-database-transaction [conn (user-manager-db um)]
      (query-maybe-row conn (select id username password_hash
                                    verified verification_code
                                    created_at updated_at
                                    #:from users
                                    #:where (= username ,username)))))

  (define u (and row (apply user (vector->list row))))
  (and u (user-password-valid? u password) u))


(module+ test
  (require rackunit
           rackunit/text-ui)

  (define-system test
    [db (make-database #:database "app_name_here_tests"
                       #:username "app_name_here"
                       #:password "app_name_here")]
    [user-manager (db) user-manager])

  (run-tests
   (test-suite
    "user-manager"
    #:before
    (lambda _
      (system-start test-system)
      (with-database-connection [conn (system-get test-system 'db)]
        (query-exec conn "truncate table users")))

    #:after
    (lambda _
      (system-stop test-system))

    (test-case "creates users"
      (check-match
       (user-manager-create-user (system-get test-system 'user-manager) "bogdan" "hunter2")
       (user (? exact-positive-integer?) "bogdan" _ #f _ _ _)))

    (test-case "returns #f when given invalid login data"
      (check-false (user-manager-login (system-get test-system 'user-manager) "invalid" "invalid"))
      (check-false (user-manager-login (system-get test-system 'user-manager) "bogdan" "invalid")))

    (test-case "returns a user upon successful login"
      (check-match
       (user-manager-login (system-get test-system 'user-manager) "bogdan" "hunter2")
       (user (? exact-positive-integer?) "bogdan" _ _ _ _ _))))))
