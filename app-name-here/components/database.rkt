#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         component
         db
         db/util/postgresql
         gregor
         racket/contract
         racket/match
         racket/sequence
         "profiler.rkt")

(provide
 (contract-out
  [struct database ((connection-pool connection-pool?)
                    (options hash?))]
  [make-database (->* (#:database string?
                       #:username string?
                       #:password string?)
                      (#:host string?
                       #:port (integer-in 0 65535)
                       #:max-connections exact-positive-integer?
                       #:max-idle-connections exact-positive-integer?)
                      (-> database?))]
  [call-with-database-connection (-> database? (-> connection? any/c) any/c)]
  [call-with-database-transaction (->* (database? (-> connection? any/c))
                                       (#:isolation (or/c 'serializable
                                                          'repeatable-read
                                                          'read-committed
                                                          'read-uncommitted
                                                          false/c))
                                       any/c)]

  [call-with-persistent-database-connection (-> database? (-> any/c) any/c)])

 with-database-connection
 with-database-transaction)

(struct database (connection-pool options)
  #:methods gen:component
  [(define (component-start a-database)
     (define options (database-options a-database))
     (struct-copy database a-database
                  [connection-pool (connection-pool
                                    #:max-connections (hash-ref options 'max-connections)
                                    #:max-idle-connections (hash-ref options 'max-idle-connections)
                                    (lambda ()
                                      (postgresql-connect
                                       #:database (hash-ref options 'database)
                                       #:user (hash-ref options 'username)
                                       #:password (hash-ref options 'password)
                                       #:server (hash-ref options 'host)
                                       #:port (hash-ref options 'port))))]))

   (define (component-stop a-database)
     (struct-copy database a-database [connection-pool #f]))])

(define ((make-database #:database database-name
                        #:username username
                        #:password password
                        #:host [host "127.0.0.1"]
                        #:port [port 5432]
                        #:max-connections [max-connections 4]
                        #:max-idle-connections [max-idle-connections 2]))
  (database #f (hasheq 'database database-name
                       'username username
                       'password password
                       'host host
                       'port port
                       'max-connections max-connections
                       'max-idle-connections max-idle-connections)))

(define current-database-connection
  (make-parameter #f))

(define (call-with-database-connection database proc)
  (with-timing 'database "call-with-database-connection"
    (define-values (connection disconnecter)
      (cond
        [(current-database-connection)
         => (lambda (conn)
              (values conn void))]

        [else
         (define connection
           (with-timing "connection-pool-lease"
             (connection-pool-lease (database-connection-pool database))))

         (values connection (lambda ()
                              (disconnect connection)))]))

    (dynamic-wind
      (lambda () #f)
      (lambda () (with-timing "proc" (proc connection)))
      (lambda () (with-timing "disconnect" (disconnecter))))))

;; A variant of call-with-database-connection that ensures that all
;; nested calls to call-with-database-connection use the same connection.
(define (call-with-persistent-database-connection database proc)
  (call-with-database-connection database
    (lambda (conn)
      (parameterize ([current-database-connection conn])
        (proc)))))

(define (call-with-database-transaction database proc #:isolation [isolation #f])
  (with-timing 'database "call-with-database-transaction"
    (with-database-connection [conn database]
      (call-with-transaction conn
        #:isolation isolation
        (lambda ()
          (proc conn))))))

(define-syntax-rule (with-database-connection [name database] e ...)
  (call-with-database-connection database
    (lambda (name)
      e ...)))

(define-syntax (with-database-transaction stx)
  (syntax-parse stx
    [(_ [name:id database:expr] e:expr ...+)
     #'(with-database-transaction [name database]
         #:isolation #f
         e ...)]

    [(_ [name:id database:expr] #:isolation isolation e:expr ...+)
     #'(call-with-database-transaction database
         #:isolation isolation
         (lambda (name)
           e ...))]))

(module+ test
  (require rackunit
           (prefix-in config: "../config.rkt"))

  (define db
    (component-start
     ((make-database #:database config:test-db-name
                     #:username config:test-db-username
                     #:password config:test-db-password))))

  (check-eq?
   (with-database-connection [conn db]
     (query-value conn "select 1"))
   1)

  (check-eq?
   (with-database-transaction [conn db]
     (query-value conn "select 1"))
   1)

  (check-eq?
   (with-database-transaction [conn db]
     #:isolation 'repeatable-read
     (query-value conn "select 1"))
   1))


;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 id/c
 maybe-id/c

 exn:fail:sql:constraint-violation?

 sql->
 row->list
 in-rows

 ->sql-date
 ->sql-timestamp)

(define id/c exact-positive-integer?)
(define maybe-id/c (or/c false/c id/c))

(define/contract exn:fail:sql:constraint-violation?
  (-> any/c boolean?)
  (match-lambda
    [(exn:fail:sql _ _ (or "23503" "23505") _) #t]
    [_ #f]))

(define/contract (in-rows conn stmt . args)
  (-> connection? statement? any/c ... sequence?)
  (sequence-map (lambda cols
                  (apply values (map sql-> cols)))
                (apply in-query conn stmt args)))

(define/contract (row->list row)
  (-> vector? list?)
  (for/list ([c (in-vector row)])
    (sql-> c)))

(define/contract (sql-> v)
  (-> any/c any/c)
  (cond
    [(sql-null? v) #f]

    [(pg-array? v)
     (pg-array->list v)]

    [(sql-timestamp? v)
     (sql-timestamp->moment v)]

    [else v]))

(define (sql-timestamp->moment t)
  (moment (sql-timestamp-year t)
          (sql-timestamp-month t)
          (sql-timestamp-day t)
          (sql-timestamp-hour t)
          (sql-timestamp-minute t)
          (sql-timestamp-second t)
          (sql-timestamp-nanosecond t)
          #:tz (or (sql-timestamp-tz t) (current-timezone))))

(define/contract (->sql-date m)
  (-> time-provider? sql-date?)
  (sql-date (->year m)
            (->month m)
            (->day m)))

(define/contract (->sql-timestamp m)
  (-> time-provider? sql-timestamp?)
  (sql-timestamp (->year m)
                 (->month m)
                 (->day m)
                 (->hours m)
                 (->minutes m)
                 (->seconds m)
                 (->nanoseconds m)
                 (->utc-offset m)))
