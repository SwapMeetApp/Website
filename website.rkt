#lang racket

(require racket/cmdline)
(require "minimal.rkt")
(require "homepage.rkt")
(require net/rfc6455)

(define SSL? (make-parameter #t))
(define SSL-CERT (make-parameter #f))
(define SSL-KEY (make-parameter #f))
(define PORT (make-parameter 443))
(define API-KEY (getenv "API_KEY"))

(command-line 
  #:program "the website"
  #:once-each
  [("--no-ssl") "Disable SSL" (SSL? #f)]
  [("--ssl-cert") ssl-cert "Path to the Cert" (SSL-CERT ssl-cert)]
  [("--ssl-key") ssl-key "Path to the Key" (SSL-KEY ssl-key)]
  [("--port") port "Port" (PORT (string->number port))])

(define handle-websockets
  (lambda (c s) (ws-send! c "Hello world!")))

(serve/servlet (accept API-KEY)
                handle-websockets
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
           