#lang racket
(require uuid)
(require db)
(require "book.rkt")

;; database implementation
; initialize-db! : path? -> library?
; Sets up a  database (if it doesn't exist)
(define (initialize-db! home)
  (define db (sqlite3-connect #:database home #:mode 'create))
  (define l (library db))
  (query-exec db
    "PRAGMA foreign_keys = ON")
  (unless (table-exists? db "books")
    (query-exec db
                (string-append
                 "CREATE TABLE books "
                 " (id TEXT PRIMARY KEY, title TEXT, self_link TEXT, isbn TEXT)")))
  (unless (table-exists? db "authors")
    (query-exec db
                "CREATE TABLE authors (bid INTEGER, name TEXT)"))
  (unless (table-exists? db "version")
    (query-exec db
                (string-append
                 "CREATE TABLE version "
                 " (version INTEGER)")))
  (unless (table-exists? db "trades")
    (query-exec db
                (string-append
                "CREATE TABLE trades "
                " (id TEXT PRIMARY KEY, item1 TEXT, item2 TEXT, "
                "FOREIGN KEY (item1) REFERENCES books (id), FOREIGN KEY (item2) REFERENCES books (id))")))
  (migrate db)                                  
  l)



(define (unique-id-for-books db)
  (query-exec db
              "PRAGMA foreign_keys=OFF")
  (call-with-transaction
   db
   (lambda () 
     (query-exec db
                 "CREATE TABLE new_books (id TEXT PRIMARY KEY, title TEXT, self_link TEXT, isbn TEXT)")
     (query-exec db
                 "CREATE TABLE new_authors (bid TEXT, name TEXT, FOREIGN KEY(bid) REFERENCES books(id))") 
     (for-each 
      (lambda (row)
        (match row
          [(vector id title self_link isbn) 
           (let ((new-id (uuid-string)))
             (query-exec db
                         "INSERT INTO new_books (id, title, self_link, isbn) VALUES(?, ?, ?, ?)" new-id title self_link isbn)
             (for-each
              (lambda (name)
                (query-exec db
                            "INSERT into new_authors (bid, name) VALUES(?, ?)" new-id name))
              (query-list db 
                          "SELECT authors.name FROM authors where authors.bid = ?" id)))]))
      (query-rows db "SELECT * FROM books"))
     (query-exec db "DROP TABLE books")
     (query-exec db "DROP TABLE authors")
     (query-exec db "ALTER TABLE new_books RENAME TO books")
     (query-exec db "ALTER TABLE new_authors RENAME TO authors")))
  (query-exec db "PRAGMA foreign_keys=ON"))         

(define migrations `((0 . ,unique-id-for-books)))

(define (migrate db)
  (define current-version 
    (query-maybe-row db 
                     "SELECT version FROM version ORDER BY version DESC LIMIT 1"))
  (define migrations-to-run           
    (if 
     current-version 
     (take-after 
      (lambda (migration)
        (equal? (car migration) current-version))
      migrations)
     migrations))
  (for-each (lambda (m) 
              ((cdr m) db)) migrations-to-run)
  (define most-recent 
      (with-handlers ([exn:fail:contract? (lambda (e) #false)])
        (car(last migrations-to-run))))
  (when most-recent  
   (query-exec db
   "INSERT INTO version(version) VALUES(?)" most-recent)))    
  
(define (take-after func l)
  (cond 
    [(empty? l) '()]
    [(func (first l)) (rest l)]
    [else 
     (take-after func (rest l))]))

(provide initialize-db!)       