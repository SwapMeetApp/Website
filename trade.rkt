#lang racket
(require "book.rkt")
(require db)
(require uuid)
(require rackunit)

;; A [Trade-of X] is a Structure
;; (trade X X trade-state?)
;; we want this to be trade.v2 eventually

;; A Side-of-Trade is a Structure
;; (side-of-trade user [NEL-of Book])

;; A Trade.v2 is a Structure
;; (trade.v2 side-of-trade side-of-trade)

(struct trade [side1 side2 state])

(define (trade-state? x)
  (match x
    ["initiated" #true]
    ["accepted" #true]
    ["completed" #true]
    [x #false]))

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
       [ x (list 'not-uuid (format "expected uuid: ~a" x))])
     (match (hash-ref json 'state)
       [(? trade-state? state) state]
       [ x (list 'not-trade-state (format "expected trade-state: ~a" x))]))))

;; a trade-search is a structure: 
;;  (trade-search [Maybe uuid-string?][Maybe uuid-string?][Maybe trade-state?]
(struct trade-search [side1 side2 state])

(define (params->trade-search params)
  (let(
      (side1 (match (assq 'side1 params)
                [#f #f]
                [(cons _ s) s]))
      (side2 (match (assq 'side2 params)
                [#f #f]
                [(cons _ s) s]))
      (state (match (assq 'state params)
                [#f #f]
                [(cons _ s) s])))
  (match (list side1 side2 state)
    [(list #f #f #f) "must provide at least one of side1, side2, or state"]
    [(list (? uuid-string? s1) (? uuid-string? s2) (? trade-state? st))
        (trade-search s1 s2 st)]
    [(list (? uuid-string? s1) #f (? trade-state? st))
        (trade-search s1 #f st)]
    [(list (? uuid-string? s1) (? uuid-string? s2) #f)
        (trade-search s1 s2 #f)]
    [(list (? uuid-string? s1) #f #f)
        (trade-search s1 #f #f)]
    [(list #f (? uuid-string? s2) (? trade-state? st))
      (trade-search #f s2 st)]
    [(list #f #f (? trade-state? st))
      (trade-search #f #f st)]
    [(list #f (? uuid-string? s2) #f)
      (trade-search #f s2 #f)])))

;; this is CRUD for a trade
;; puts the trade into the database
;; [Trade-of uuid] -> [Maybe uuid-string?]
(define (library-insert-trade! trade library)
  (let ((conn (library-db library)))
    (match (query-maybe-row conn
                            "INSERT INTO trades (id, side1, side2) VALUES ($1, $2, $3) RETURNING id"
                            (uuid-string) (trade-side1 trade) (trade-side2 trade))
      [#false #false]
      [(vector id) id])))


;; String library -> [Maybe trade] 
(define (library-find-trade! trade-id library)
  (let ((conn (library-db library)))
    (match (query-maybe-row conn
                            "SELECT trades.side1, trades.side2, trades.state from trades WHERE $1 = trades.id" trade-id)
      [(vector side1 side2 state) 
       (trade side1 side2 state)]
      [ _ #false])))

;; String trade library -> [Maybe uuid-string?]
(define (library-update-trade! trade-id trade library)
  (let ((conn (library-db library)))
    (match (query-maybe-row conn
                            "UPDATE trades SET side1 = $1, side2 = $2, state = $3 WHERE $4 = trades.id RETURNING id"
                            (trade-side1 trade) (trade-side2 trade) (trade-state trade) trade-id)
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

;;trade-search library -> [List-of trade]
(define (library-search-trades! search library)
  (let* ((conn (library-db library))
         (terms (list (trade-search-side1 search)
                      (trade-search-side2 search)
                      (trade-search-state search)))
         (terms2 `((side1  ,(trade-search-side1 search))
                  (side2  ,(trade-search-side2 search))
                  (state  ,(trade-search-state search))))
         (terms3 (filter (lambda (x)
                        (match x
                        [(list _ #f) #f]
                        [x #t])) terms2))
         (indexes (second (foldl (lambda (x acc)
                            (list (+ 1 (first acc)) (append (second acc) (list (first acc))))) '(1 ()) terms3))) 
         (q (string-append "SELECT * FROM trades WHERE "
                                          (string-join 
                                          (map (lambda (i x) 
                                            (match x 
                                            [(list col str) (format "$~a = ~a" i col)]))
                                            indexes terms3)
                                            " AND ")))
          (row->trade (lambda (r)
            (match r
              [(vector id side1 side2 state) (trade side1 side2 state)]))))                                                         
   (map row->trade (apply query-rows conn q 
                     (filter (lambda (x) (not (eq? #f x))) terms)))))
 
(provide (all-defined-out))
