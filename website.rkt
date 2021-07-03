#lang racket

(require racket/cmdline)
(require "minimal.rkt")
(require "homepage.rkt")
(require "notifications.rkt")
(require "database.rkt")
(require (prefix-in trade: "./api/trade.rkt"))

(define DATABASE-PATH (make-parameter null))
(define SSL? (make-parameter #t))
(define SSL-CERT (make-parameter #f))
(define SSL-KEY (make-parameter #f))
(define PORT (make-parameter 443))
(define API-KEY (getenv "API_KEY"))

(command-line 
  #:program "the website"
  #:once-each
  [("-d" "--db-path") database-path "Path to the database"
   (DATABASE-PATH database-path)]
  [("--no-ssl") "Disable SSL" (SSL? #f)]
  [("--ssl-cert") ssl-cert "Path to the Cert" (SSL-CERT ssl-cert)]
  [("--ssl-key") ssl-key "Path to the Key" (SSL-KEY ssl-key)]
  [("--port") port "Port" (PORT (string->number port))])

(define library (initialize-db! (string->path(DATABASE-PATH))))

(serve/servlet (accept API-KEY library)
                handle-websockets
                (trade:API library)
               #:launch-browser? #f
               #:listen-ip #f
               #:port (PORT)
               #:mime-types-path "static/mime.types"
               #:extra-files-paths
               (list "static")
               #:servlet-path "/"
               #:server-root-path (current-directory)
               #:ssl? (SSL?)
               #:ssl-cert (SSL-CERT) 
               #:ssl-key (SSL-KEY)
               #:log-file (current-output-port))
           