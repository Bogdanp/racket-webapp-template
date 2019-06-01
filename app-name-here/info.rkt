#lang info

(define collection "app-name-here")
(define deps '("base"
               "component-lib"
               "crypto-lib"
               "db-lib"
               "forms-lib"
               "gregor-lib"
               "koyo-lib"
               "libuuid"
               "north"
               "postmark-client"
               "sql"
               "srfi-lite-lib"
               "struct-plus-plus"
               "threading-lib"
               "web-server-lib"))
(define build-deps '("rackunit-lib"))
