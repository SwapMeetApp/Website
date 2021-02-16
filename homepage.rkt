#lang racket

(require net/http-client)
(require json)
(require net/uri-codec)
(require web-server/servlet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct book (title authors self-link isbn))

(struct library (books) #:mutable)

(define LIBRARY (library '()))

;; Styling
(define (style-color color)
  (string-append "color:" color ";" "background-color:black;"))

;; API-KEY (string) ->  [request -> response]
(define (accept API-KEY)
  (lambda (request)
    (render-home-page request API-KEY)))
 
; parse-search: bindings -> string
; Extracts a searchterm out of the bindings.
(define (parse-search bindings)
    (extract-binding/single 'search bindings))

;; bindings -> string
(define (make-search-parameters b)
    (form-urlencoded-encode (extract-binding/single 'search b))) 

;;;;;;;;;;;;;;;;;;
;;; get request
(define (get searchterm API-KEY)
  (define hc (http-conn))
  (http-conn-open! hc
                 "www.googleapis.com"
                 #:ssl? #t)    
  (http-conn-send! hc
                 (string-append "https://www.googleapis.com/books/v1/volumes?q=" searchterm "&" "key=" API-KEY)
                 #:method #"GET")
  (define-values (status-line headers body-port) 
    (http-conn-recv! hc
                 #:method #"GET"))            
  (read-json body-port))

;;;;;;;;;;;;;
;; json -> book
 (define (parse-book v)
    (book
      (hash-ref (hash-ref v 'volumeInfo) 'title)
      (hash-ref (hash-ref v 'volumeInfo) 'authors)
      (hash-ref v 'selfLink)
      (hash-ref (first (hash-ref (hash-ref v 'volumeInfo) 'industryIdentifiers)) 'identifier)))
 
; render-home-page:  request -> doesn't return
; Consumes a request, and produces an HTML page
(define (render-home-page request API-KEY)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Booksearch") (script ((src "/main.js"))))
            (body
             (h1 ((style ,(style-color "green")))"Booksearch")
             (form ((action
                     ,(embed/url (render-results-page API-KEY))))
                   (input ((name "search")))
                   (input ((type "submit"))))
             (section 
              (h2 ((style ,(style-color "yellow"))) "messages")
              (div ((id "messages") (style "overflow-y:auto; height:6em;")))
              (form ((action "javascript:void(0)"))
                   (button ((id "send-message") (onclick "sendMessage()"))
                    "send")
                   (input ((id "chat") (placeholder "type your message")))) 
              )))))
  (send/suspend/dispatch response-generator))

(define (render-results-page API-KEY)
  (lambda (request)
    (define (response-generator embed/url)  
        (define a-search
            (make-search-parameters (request-bindings request)))
        (define titles-and-authors
            (map parse-book (hash-ref (get a-search API-KEY) 'items)))
        (response/xexpr
        `(html (head (title "results"))
            (body
             (h1 ((style ,(style-color "green")))"results")
             (ul ((style "margin:auto;")) 
              ,@(map (lambda (t) `(li (a ((href ,(embed/url (book-selection-confirmation-page API-KEY t)))) 
               ,(book-title t)))) titles-and-authors))))))
    (send/suspend/dispatch response-generator)))

;; API-KEY book -> any!

(define (book-selection-confirmation-page API-KEY book)
  (lambda (request)
   (set-library-books! LIBRARY (cons book (library-books LIBRARY)))
   (define (response-generator embed/url)  
    (response/xexpr
        `(html (head (title "your book"))
            (body
             (h1 ((style ,(style-color "green")))"your book")
             (ul ((style "margin:auto;"))
              (li (a ((href ,(embed/url (browsing-page API-KEY LIBRARY)))) 
               ,(string-append (book-title book) " - " (string-join (book-authors book) ", ") " - " (book-isbn book)))))))))
    (send/suspend/dispatch response-generator)))

(define (browsing-page API-KEY library)
  (lambda (request)
   (define (response-generator embed/url)  
    (response/xexpr
        `(html (head (title "available items"))
            (body
             (h1 ((style ,(style-color "green")))"browse")
             (ul ((style "margin:auto;"))
               ,@(map (lambda (book)
                 `(li ,(string-append (book-title book) " - " (string-join (book-authors book) ", ") " - " (book-isbn book)))) (library-books library)))))))
    (send/suspend/dispatch response-generator)))

;; turn browsing catalog clickable
               



(provide accept)   
 




