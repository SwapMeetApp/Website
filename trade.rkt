#lang racket
(require "book.rkt")
(require db)
(require uuid)
(require rackunit)

;; A [Trade-of X] is a Structure
;; (trade X X)
;; we want this to be trade.v2 eventually

;; A Side-of-Trade is a Structure
;; (side-of-trade user [NEL-of Book])

;; A Trade.v2 is a Structure
;; (trade.v2 side-of-trade side-of-trade)

(struct trade [side1 side2])

;; json -> [trade-of uuid]
(define (parse-trade json)
 (with-handlers ((exn:fail:contract? 
  (lambda (e) (raise-user-error "expect object with side1 and side2" json))))
  (trade
   (match (hash-ref json 'side1)
    [(? uuid-string? side1) side1]
    [ x (raise-user-error "expected uuid" x)])
   (match (hash-ref json 'side2)
    [(? uuid-string? side2) side2]
    [ x (raise-user-error "expected uuid" x)])))) 

;; puts the trade into the database
;; [Trade-of uuid] -> (void)
(define (library-insert-trade! trade library)
 (let ((conn (library-db library)))
    (call-with-transaction
     conn 
     (lambda ()
       (query-exec conn
                   "INSERT INTO trades (id, item1, item2,) VALUES (?, ?, ?); "
                   (uuid-string) (trade-side1 trade) (trade-side2 trade))))))

;; String library -> trade
(define (library-find-trade trade-id library)
  (let ((conn (library-db library)))
  (match
   (query-row conn
    "SELECT trades.item1, trades.item2 from trades WHERE ? = trades.id" trade-id)
    [(vector id item1 item2) 
      (trade (find-library-book library item1) (find-library-book library item2))])))

   