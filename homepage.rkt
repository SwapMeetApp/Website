#lang racket

(require net/http-client)
(require json)
(require net/uri-codec)
(require web-server/servlet)
(require db)
(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct book (title authors self-link isbn))

; A library is a (db)
; where db is a connection to a database
(struct library (db))

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
               ,@(map (lambda (title)
                        `(li ,title))
                      (library-titles library)))))))
    (send/suspend/dispatch response-generator)))

;; library -> list-of string
(define (library-titles library)
  (query-list (library-db library)
    "SELECT title FROM books"))

;; turn browsing catalog clickable and take to item details page
               
;; database implementation
; initialize-db! : path? -> library?
; Sets up a  database (if it doesn't exist)
(define (initialize-db! home)
    (define db (sqlite3-connect #:database home #:mode 'create))
    (define l (library db))
    (unless (table-exists? db "books")
        (query-exec db
         (string-append
          "CREATE TABLE books "
          " (id INTEGER PRIMARY KEY, title TEXT, self_link TEXT, isbn TEXT)")))
    (unless (table-exists? db "authors")
        (query-exec db
         "CREATE TABLE authors (bid INTEGER, name TEXT)"))      
    l)

; library-insert-book!: library? string? string? string? string? -> void
; Consumes a library and a book, adds the book at the top of the library.
(define (library-insert-book! library title authors self-link isbn)
  (let ((conn (library-db library)))
    (call-with-transaction conn 
   (lambda () 
    (query-exec conn
     "PRAGMA temp_store = 2; ")
     (query-exec conn
     "CREATE TEMP TABLE _variables(name TEXT PRIMARY KEY, value INTEGER);")
     (query-exec conn
     "INSERT INTO books (title, self_link, isbn) VALUES (?, ?, ?); "
     title self-link isbn)
     (query-exec conn
     "INSERT INTO _variables (name, value) VALUES ('book', last_insert_rowid()); ")     
    (for-each (lambda (author) 
     (query-exec conn
      "INSERT INTO authors SELECT value, ? FROM _variables WHERE name = 'book'; "
      author)) authors)
    (query-exec conn "DROP TABLE _variables;")))))  


(provide accept initialize-db!)   
 




