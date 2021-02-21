#lang racket

(require racket/cmdline)
(require "minimal.rkt")
(require "homepage.rkt")
(require net/rfc6455)

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

(struct chatserver (connections) #:mutable)

(define CHAT (chatserver '()))

(define (handle-websockets conn req)
 (set-chatserver-connections! CHAT (cons conn (chatserver-connections CHAT)))
  (thread (lambda () 
            (let loop ()
             (sync (handle-evt (ws-recv-evt conn)
                               (lambda (message)
                                (if (eof-object? message)
                                  (set-chatserver-connections! CHAT (remove conn (chatserver-connections CHAT)))
                                  (begin (broadcast message) (loop))))))))))

(define (broadcast message)
  (for-each (lambda (c) (unless (ws-conn-closed? c) (ws-send! c message)) 
            (chatserver-connections CHAT))))


(serve/servlet (accept API-KEY (initialize-db! (string->path(DATABASE-PATH))))
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
           