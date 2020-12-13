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


(define DEF-SEARCH "https://api.github.com/users/jsoo1")

(define (style-color color)
  (string-append "color:" color ";" "background-color:black;"))

;; bindings -> string
(define (make-search-url b)
  (string-append "https://api.github.com/users/"(extract-binding/single 'username b)))

; start: request -> response
; Consumes a request and produces a page that displays all of the
; web content.
(define (start request)
  (define a-search
    (cond 
      [(can-parse-search? (request-bindings request))
        (make-search-url (request-bindings request))]
      [else DEF-SEARCH]))
  (render-home-page a-search request))

;; url-string --> response json
;;; no work 
(define (get url)
  (define hc (http-conn))
  (http-conn-open! hc
                 "api.github.com"
                 #:ssl? #t)    
  (http-conn-send! hc
                 url
                 #:method #"GET"
                 #:headers (list "accept: application/json")
                 #:version #"1.1")
  (define-values (status-line headers body-port) 
  (http-conn-recv! hc
                 #:method #"GET"))
  (read-json body-port))               


; can-parse-search?: bindings -> boolean
; Produces true if bindings contains values for 'username
(define (can-parse-search? bindings)
   (exists-binding? 'username bindings))
 

(define (render-home-page a-search request)
    (response/xexpr
     `(html (head (title "Vincent Lay"))
            (body
             (h1 ((style ,(style-color "blue")))"Vincent Lay")
             (h2 ((style "margin:auto;")) ,(hash-ref (get a-search) 'bio)
             (form
              (input ((name "username")))
              (input ((type "submit")))))))))  

(require web-server/servlet-env)
(serve/servlet start
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

;;           
               