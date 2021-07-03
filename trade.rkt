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

;; json -> [Or [trade-of uuid] (List Symbol String)]
(define (parse-trade json)
 (with-handlers ((exn:fail:contract? 
  (lambda (e) (list 'unexpected-json (format "expect object with side1 and side2: ~a" json)))))
  (trade
   (match (hash-ref json 'side1)
    [(? uuid-string? side1) side1]
    [ x (list 'not-uuid (format "expected uuid: ~a" x))])
   (match (hash-ref json 'side2)
    [(? uuid-string? side2) side2]
    [ x (list 'not-uuid (format "expected uuid: ~a" x))])))) 

;; this is CRUD for a trade
;; puts the trade into the database
;; [Trade-of uuid] -> (void)
(define (library-insert-trade! trade library)
 (let ((conn (library-db library)))
    (call-with-transaction
     conn 
     (lambda ()
       (query-exec conn
                   "INSERT INTO trades (id, item1, item2) VALUES (?, ?, ?); "
                   (uuid-string) (trade-side1 trade) (trade-side2 trade))))))

;; String library -> [Maybe trade] 
(define (library-find-trade trade-id library)
  (let ((conn (library-db library)))
  (match
   (query-maybe-row conn
    "SELECT trades.item1, trades.item2 from trades WHERE ? = trades.id" trade-id)
    [(vector id item1 item2) 
      (trade (find-library-book library item1) (find-library-book library item2))]
    [ _ #false])))

;; String trade library -> void
(define (library-update-trade trade-id trade library)
  (let ((conn (library-db library)))
  (query-exec conn
              "UPDATE trades SET item1 = ?, item2 = ? WHERE ? = trades.id"
              (trade-side1 trade) (trade-side2 trade) trade-id)))

;; String library -> void
(define (library-delete-trade trade-id library)
  (let ((conn (library-db library)))
  (query-exec conn
              "DELETE FROM trades WHERE ? = trades.id"
              trade-id)))

(provide (all-defined-out))