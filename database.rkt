#lang racket
(require uuid)
(require db)
(require "book.rkt")

;; database implementation
; initialize-db! : path? -> library?
; Sets up a  database (if it doesn't exist)
(define (initialize-db! password host)
  (define db 
    (postgresql-connect
      #:database "swap"
      #:user "postgres"
      #:password password
      #:port 5432
      #:server host))
  (define l (library db))
  (unless (table-exists? db "books")
    (query-exec db
                  "CREATE TABLE books (id TEXT PRIMARY KEY, title TEXT, self_link TEXT, isbn TEXT)"))
  (unless (table-exists? db "authors")
    (query-exec db
                "CREATE TABLE authors (bid TEXT REFERENCES books(id), name TEXT)"))
  (unless (table-exists? db "version")
    (query-exec db
                (string-append
                 "CREATE TABLE version "
                 " (version INTEGER)")))
  (unless (table-exists? db "trades")
    (query-exec db
                (string-append
                "CREATE TABLE trades "
                " (id TEXT PRIMARY KEY, item1 TEXT REFERENCES books (id), item2 TEXT REFERENCES books (id))")))
  (migrate db)                                  
  l)

(define (unique-id-for-books db)
  (println "skipping unique-id-for-books"))         

(define (state-for-trades db)
  (query-exec db
    (string-append "ALTER TABLE trades ADD COLUMN state TEXT NOT NULL DEFAULT 'initiated' " 
      "CONSTRAINT valid_states CHECK (state = 'initiated' OR state = 'accepted' OR state = 'completed') ")))

(define (rename-trades-columns db)
  (call-with-transaction db (lambda () 
        (query-exec db
          "ALTER TABLE trades RENAME COLUMN item1 TO side1")
        (query-exec db
          "ALTER TABLE trades RENAME COLUMN item2 TO side2"))))

(define migrations `(
  (0 . ,unique-id-for-books)
  (1 . ,state-for-trades)
  (2 . ,rename-trades-columns)))

(define (migrate db)
  (define current-version 
    (match (query-maybe-row db 
                     "SELECT version FROM version ORDER BY version DESC LIMIT 1")
      [(vector v) v]
      [x x]))
  (define migrations-to-run           
    (if 
     current-version 
     (take-after 
      (lambda (migration)
        (equal? (car migration) current-version))
      migrations)
     migrations))
    (printf "running migrations: ~a\n" migrations-to-run) 
  (for-each (lambda (m) 
              ((cdr m) db)) migrations-to-run)
  (define most-recent 
      (with-handlers ([exn:fail:contract? (lambda (e) #false)])
        (car(last migrations-to-run))))
  (when most-recent
   (query-exec db
   "INSERT INTO version (version) VALUES ($1)" most-recent)))   
  
(define (take-after func l)
  (cond 
    [(empty? l) '()]
    [(func (first l)) (rest l)]
    [else 
     (take-after func (rest l))]))

(provide initialize-db!)       