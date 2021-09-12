#lang racket
(require net/http-client)
(require json)
(require db)
(require uuid)
(require db/util/postgresql)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct book (title authors self-link isbn id))

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
;; TO DO - handle erros
(define (parse-book v)
  (define volume-info (hash-ref v 'volumeInfo))
  (book
   (hash-ref volume-info  'title)
   (hash-ref volume-info  'authors (lambda () '()))
   (hash-ref v 'selfLink)
   (match (hash-ref volume-info 'industryIdentifiers (lambda () #f))
                [#f #f]
                [X (hash-ref (first X) 'identifier)])
   (uuid-string)))


;; library string -> book
;; looks up a book by title in library
(define (find-library-book library book-id)
  (match
      (query-row (library-db library)
                 (string-append 
                 "SELECT books.*, array_agg(authors.name) "
                    "FROM books "
                      "INNER JOIN authors ON books.id = authors.bid "
                      "AND $1 = books.id "
                    "GROUP BY books.id") book-id)
    [(vector id title self_link isbn authors)
     (book title (pg-array->list authors) self_link (sql-null->false isbn) id)]))          

;; library -> list-of vector (string, id)
(define (library-titles library)
  (query-rows (library-db library)
              "SELECT title, id FROM books"))

; library-insert-book!: library? string? string? string? string? -> void
; Consumes a library and a book, adds the book at the top of the library.
(define (library-insert-book! library book)
  (let ((conn (library-db library)))
    (call-with-transaction
     conn 
     (lambda ()     
       (query-exec conn
                   "INSERT INTO books (title, self_link, isbn, id) VALUES ($1, $2, $3, $4); "
                   (book-title book) (book-self-link book) (false->sql-null (book-isbn book)) (book-id book))     
       (for-each (lambda (author) 
                   (query-exec conn
                               "INSERT INTO authors (bid, name) VALUES ($1, $2)" (book-id book)
                               author)) (book-authors book)))))) 

(provide (all-defined-out))           