#lang racket

(require net/http-client)
(require json)
(require net/uri-codec)
(require web-server/servlet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Styling
(define (style-color color)
  (string-append "color:" color ";" "background-color:black;"))

;; API-KEY (string) ->  [request -> response]
(define (accept API-KEY)
  (lambda (request)
    (render-home-page request)))
 
; parse-search: bindings -> string
; Extracts a searchterm out of the bindings.
(define (parse-search bindings)
    (extract-binding/single 'search bindings))

;; bindings -> string
(define (make-search-parameters b)
    (form-urlencoded-encode (extract-binding/single b))) 

;;;;;;;;;;;;;;;;;;
;;; get request
(define (get searchterm API-KEY)
  (define hc (http-conn))
  (http-conn-open! hc
                 "www.googleapis.com"
                 #:ssl? #t)    
  (http-conn-send! hc
                 (string-append "https://www.googleapis.com/books/v1/volumes?" searchterm "&" "key=" API-KEY)
                 #:method #"GET")
  (define-values (status-line headers body-port) 
    (http-conn-recv! hc
                 #:method #"GET"))            
  (read-json body-port)) 

;;;;;;;;;;;;;
;; parse-volume-info
 (define (parse-volume-info v)
    (list
      (hash-ref (hash-ref v 'volumeInfo) 'title)
      (hash-ref (hash-ref v 'volumeInfo) 'authors)
      (hash-ref v 'selfLink)
      (hash-ref (first (hash-ref (hash-ref v 'volumeInfo) 'industryIdentifiers)) 'identifier)))
 
; render-home-page:  request -> doesn't return
; Consumes a request, and produces an HTML page
(define (render-home-page request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Booksearch"))
            (body
             (h1 ((style ,(style-color "green")))"Booksearch")
             ,(render-posts a-blog)
             (form ((action
                     ,(embed/url render-results-page)))
                   (input ((name "search")))
                   (input ((type "submit"))))))))
 
  (define (render-results-page request)  
        (define a-search
            (make-search-parameters (request-bindings request)))
        (define titles-and-authors
            (map parse-volume-info (hash-ref (get a-search API-KEY) 'items)))
        (response/xexpr
        `(html (head (title "results"))
            (body
             (h1 ((style ,(style-color "green")))"results")
             (ul ((style "margin:auto;")) 
              ,@(map (lambda (t) `(li (a ((href ,(third t))) ,(first t)))) titles-and-authors))))))
    (send/suspend/dispatch response-generator))

(provide accept)   
 




