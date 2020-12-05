#lang racket

(require net/http-client)
(require json)
(require web-server/servlet)
(require racket/cmdline)

(define SSL? (make-parameter #t))
(define SSL-CERT (make-parameter #f))
(define SSL-KEY (make-parameter #f))
(define PORT (make-parameter 443))

(command-line 
  #:program "the website"
  #:once-each
  [("--no-ssl") "Disable SSL" (SSL? #f)]
  [("--ssl-cert") ssl-cert "Path to the Cert" (SSL-CERT ssl-cert)]
  [("--ssl-key") ssl-key "Path to the Key" (SSL-KEY ssl-key)]
  [("--port") port "Port" (PORT (string->number port))])

(define hc (http-conn))

(http-conn-open! hc
                 "api.github.com"
                 #:ssl? #t)                               
                 
(http-conn-send! hc
                 "https://api.github.com/users/jsoo1"
                 #:method #"GET"
                 #:headers (list "accept: application/json")
                 #:version #"1.1")

(define-values (status-line headers body-port) 
  (http-conn-recv! hc
                 #:method #"GET"))

(define body (read-json body-port))

(hash-ref body 'bio)    

(define (style-color color)
  (string-append "color:" color ";" "background-color:black;"))

(define (render-home-page request)
    (response/xexpr
     `(html (head (title "Vincent Lay"))
            (body
             (h1 ((style ,(style-color "blue")))"Vincent Lay")
             (h2 ((style "margin:auto;")) ,(hash-ref body 'bio))))))  

(require web-server/servlet-env)
(serve/servlet render-home-page
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port (PORT)
               #:extra-files-paths
               (list "static")
               #:servlet-path "/"
               #:server-root-path (current-directory)
               #:ssl? (SSL?)
               #:ssl-cert (SSL-CERT) 
               #:ssl-key (SSL-KEY)
               #:log-file (current-output-port))

           
               