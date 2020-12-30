#lang racket

(require web-server/servlet-env)
(require "homepage.rkt")

(define (start)
    (serve/servlet (accept (call-with-input-file ".env" port->string))
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8000
               #:extra-files-paths
               (list "static")
               #:servlet-path "/"
               #:server-root-path (current-directory)
               #:log-file (current-output-port)))
      
               