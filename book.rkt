#lang racket
(require net/http-client)
(require json)
(require db)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct book (title authors self-link isbn))

; A library is a (db)
; where db is a connection to a database
(struct library (db))

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
  (define volume-info (hash-ref v 'volumeInfo))
  (book
   (hash-ref volume-info  'title)
   (hash-ref volume-info  'authors (lambda () '()))
   (hash-ref v 'selfLink)
   (hash-ref (first (hash-ref volume-info 'industryIdentifiers)) 'identifier)))


;; library string -> book
;; looks up a book by title in library
(define (find-library-book library book-id)
  (match
      (query-row (library-db library)
                 "SELECT * from books where ? = books.id" book-id)
    [(vector id title self_link isbn)
     (book title (library-authors-for-book library id) self_link isbn)]))          

;; library -> list-of vector (string, id)
(define (library-titles library)
  (query-rows (library-db library)
              "SELECT title, id FROM books"))
(define (library-authors-for-book library id)
  (query-list (library-db library)
              "SELECT authors.name from authors where ? = authors.bid" id))


; library-insert-book!: library? string? string? string? string? -> void
; Consumes a library and a book, adds the book at the top of the library.
(define (library-insert-book! library title authors self-link isbn)
  (let ((conn (library-db library)))
    (call-with-transaction
     conn 
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

(provide (all-defined-out))                                    