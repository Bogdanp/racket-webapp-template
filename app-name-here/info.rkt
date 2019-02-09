#lang info

(define collection "app-name-here")
(define deps '("base"
               "component-lib"
               "crypto-lib"
               "db-lib"
               "gregor-lib"
               "north"
               "postmark-client"
               "sql"
               "srfi-lib"
               "struct-plus-plus"
               "threading-lib"
               "web-server-lib"))
(define build-deps '("rackunit-lib"))
