#lang racket

(require net/http-client)
(require json)
(require web-server/servlet)
(require racket/cmdline)
(require net/uri-codec)

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

(define DEF-SEARCH "q=Flowers")

(define (style-color color)
  (string-append "color:" color ";" "background-color:black;"))

;; bindings -> query parameters
(define (make-search-parameters b)
  (string-append "q=" (form-urlencoded-encode (extract-binding/single 'searchterm b))))

; request -> response
; Consumes a request and produces a page that displays all of the
; web content.
(define (accept request)
  (define a-search
    (cond 
      [(can-parse-search? (request-bindings request))
        (make-search-parameters (request-bindings request))]
      [else DEF-SEARCH]))
  (render-home-page a-search request))

;; url-string --> response json
;;; no work 
(define (get searchterms)
  (define hc (http-conn))
  (http-conn-open! hc
                 "www.googleapis.com"
                 #:ssl? #t)    
  (http-conn-send! hc
                 (string-append "https://www.googleapis.com/books/v1/volumes?" searchterms "&" "key=" API-KEY)
                 #:method #"GET")
  (define-values (status-line headers body-port) 
    (http-conn-recv! hc
                 #:method #"GET"))            
  (read-json body-port))               


; can-parse-search?: bindings -> boolean
; Produces true if bindings contains values for 'searchterm
(define (can-parse-search? bindings)
   (exists-binding? 'searchterm bindings))
 

(define (render-home-page a-search request)
  (define titles-and-authors
    (map 
    (lambda (v)
    (list (hash-ref (hash-ref v 'volumeInfo) 'title)
          (hash-ref (hash-ref v 'volumeInfo) 'authors)
          (hash-ref v 'selfLink)))
      (hash-ref (get a-search) 'items)))
    (response/xexpr
     `(html (head (title "booksearch"))
            (body
             (h1 ((style ,(style-color "blue")))"booksearch")
             (ul ((style "margin:auto;")) 
              ,@(map (lambda (t) `(li (a ((href ,(third t))) ,(first t)))) titles-and-authors))
             (form
              (input ((name "searchterm")))
              (input ((type "submit"))))))))  

(require web-server/servlet-env)
(serve/servlet accept
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
      
               