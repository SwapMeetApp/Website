#lang racket

(require "minimal.rkt")
(require "homepage.rkt")
(require "notifications.rkt")

(define (start)
  (serve/servlet (accept (call-with-input-file ".env" port->string)
                         (initialize-db! (string->path "db")))
                 handle-websockets
                 #:launch-browser? #f
                 #:listen-ip #f
                 #:port 8000
                 #:mime-types-path "static/mime.types"
                 #:extra-files-paths
                 (list "static")
                 #:servlet-path "/"
                 #:server-root-path (current-directory)
                 #:ssl? #false
                 #:log-file (current-output-port)))


