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
;; [Trade-of uuid] -> [Maybe uuid-string?]
(define (library-insert-trade! trade library)
  (let ((conn (library-db library)))
    (match (query-maybe-row conn
                            "INSERT INTO trades (id, item1, item2) VALUES ($1, $2, $3) RETURNING id"
                            (uuid-string) (trade-side1 trade) (trade-side2 trade))
      [#false #false]
      [(vector id) id])))


;; String library -> [Maybe trade] 
(define (library-find-trade! trade-id library)
  (let ((conn (library-db library)))
    (match (query-maybe-row conn
                            "SELECT trades.item1, trades.item2 from trades WHERE $1 = trades.id" trade-id)
      [(vector item1 item2) 
       (trade item1 item2)]
      [ _ #false])))

;; String trade library -> [Maybe uuid-string?]
(define (library-update-trade! trade-id trade library)
  (let ((conn (library-db library)))
    (match (query-maybe-row conn
                            "UPDATE trades SET item1 = $1, item2 = $2 WHERE $3 = trades.id RETURNING id"
                            (trade-side1 trade) (trade-side2 trade) trade-id)
      [#false #false]
      [(vector id) id])))

;; String library -> [Maybe uuid-string?]
(define (library-delete-trade! trade-id library)
  (let ((conn (library-db library)))
    (match (query-maybe-row conn
                            "DELETE FROM trades WHERE $1 = trades.id RETURNING id"
                            trade-id)
      [#false #false]
      [(vector id) id])))              

(provide (all-defined-out))
