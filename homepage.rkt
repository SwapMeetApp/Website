#lang racket

(require net/uri-codec)
(require web-server/servlet)
(require racket/list)
(require "book.rkt")

;; Styling
(define (style-color color)
  (string-append "color:" color ";" "background-color:black;"))

;; API-KEY (string) ->  [request -> response]
(define (accept API-KEY library)
  (lambda (request)
    (render-home-page request API-KEY library)))
 
; parse-search: bindings -> string
; Extracts a searchterm out of the bindings.
(define (parse-search bindings)
  (extract-binding/single 'search bindings))

;; bindings -> string
(define (make-search-parameters b)
  (form-urlencoded-encode (extract-binding/single 'search b))) 
 
; render-home-page:  request -> doesn't return
; Consumes a request, and produces an HTML page
(define (render-home-page request API-KEY library)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Booksearch") (script ((src "/main.js"))))
            (body
             (h1 ((style ,(style-color "green")))"Booksearch")
             (form ((action
                     ,(embed/url (render-results-page API-KEY library))))
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

(define (render-results-page API-KEY library)
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
                   ,@(map (lambda (t) `(li (a ((href ,(embed/url (book-selection-confirmation-page API-KEY library t)))) 
                                              ,(book-title t)))) titles-and-authors))))))
    (send/suspend/dispatch response-generator)))

;; API-KEY book -> any!
(define (book-selection-confirmation-page API-KEY library book)
  (lambda (request)
    (library-insert-book! library (book-title book) (book-authors book) (book-self-link book) (book-isbn book))
    (define (response-generator embed/url)  
      (response/xexpr
       `(html (head (title "your book"))
              (body
               (h1 ((style ,(style-color "green")))"your book")
               (ul ((style "margin:auto;"))
                   (li (a ((href ,(embed/url (browsing-page API-KEY library)))) 
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
                   ,@(map (lambda (row)
                            (match row
                             [(vector title book-id)
                            `(li (a ((href ,(embed/url (available-item-details-page API-KEY library book-id)))) 
                                    ,title))]))
                          (library-titles library)))))))
    (send/suspend/dispatch response-generator)))

(define (available-item-details-page API-KEY library book-id)
  (lambda (request)
    (define item (find-library-book library book-id)) 
    (response/xexpr
     `(html (head (title "item details"))
            (body
             (h1 ((style ,(style-color "green")))"item details")
             ,(book-title item)
             ,@(book-authors item)
             ,(book-self-link item)
             ,(book-isbn item)))))) 

(provide accept)   