#lang racket
(require "../book.rkt")
(require web-server/dispatch)
(require web-server/servlet)
(require web-server/http)
(require uuid)
(require json)

;; book  -> jsexpr?
(define (book->jsexpr b)
  (match b
  [(book title authors self-link isbn id)
     (hash 'title title
           'authors authors
           'self-link self-link
           'isbn isbn
           'id id)]))  

(define (jsexpr->book json)
  (with-handlers ((list? identity)
                  (exn:fail:contract? 
                   (lambda (e) (list 'unexpected-json (format "expect object with title, authors, self-link, isbn, and id: ~a" json)))))
    (book
     (match (hash-ref json 'title)
       [(? string? title) title]
       [ x (raise (list 'not-string (format "expected string: ~a" x)))])
     (match (hash-ref json 'authors)
       [(? (lambda (x)
                (and (list? x)
                      (andmap uuid-string? x))) authors) authors]
       [ x (raise (list 'not-list-of-uuid-strings (format "expected list of uuid-strings: ~a" x)))])
     (match (hash-ref json 'self-link)
       [(? string? self-link) self-link]
       [ x (raise (list 'not-string (format "expected string: ~a" x)))])
     (match (hash-ref json 'isbn)
       [(? string? isbn) isbn]
       [ x (raise (list 'not-string (format "expected string: ~a" x)))])
     (match (hash-ref json 'id)
       [(? uuid-string? id) id]
       [ x (raise (list 'not-uuid-string (format "expected uuid-string: ~a" x)))]))))


;; number bytes string -> reponse?
(define (response/error code message-bytes error-message)
  (response/full
   code message-bytes (current-seconds) #"application/json"
   '()
   (list (jsexpr->bytes (hash 'message error-message )))))

;; library -> request? -> any 
;; library -> dispatcher/c        
(define (API library)
  (define-values (books-dispatch books-url)
    (dispatch-rules
     [("books" (string-arg)) #:method "get" (get-book! library)]))     
  books-dispatch)

;; library -> request String -> any
(define (get-book! library)
  (lambda (request book-id)
    (match (uuid-string? book-id)
      [#false (response/error 400 #"Bad Request" "invalid book-id")]
      [#true
       (match (find-library-book library book-id)
         [#false (response/error 404 #"Not Found" "not found")]
         [book (response/jsexpr (book->jsexpr book))])])))





(provide API)
