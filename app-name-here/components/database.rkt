#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         component
         db
         racket/class
         racket/contract)

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
                                       any/c)])

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
                        #:max-idle-connections [max-idle-connections 1]))
  (database #f (hasheq 'database database-name
                       'username username
                       'password password
                       'host host
                       'port port
                       'max-connections max-connections
                       'max-idle-connections max-idle-connections)))

(define (call-with-database-connection database proc)
  (define pool (database-connection-pool database))
  (define connection (connection-pool-lease pool))
  (dynamic-wind
    (lambda () #f)
    (lambda () (proc connection))
    (lambda () (disconnect connection))))

(define (call-with-database-transaction database proc #:isolation [isolation #f])
  (with-database-connection [conn database]
    (call-with-transaction conn
      #:isolation isolation
      (lambda ()
        (proc conn)))))

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
  (require rackunit)

  (define db
    (component-start
     ((make-database #:database "app_name_here_tests"
                     #:username "app_name_here"
                     #:password "app_name_here"))))

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