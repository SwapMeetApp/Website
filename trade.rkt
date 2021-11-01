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

(define (jsexpr->trade-search json)
  (with-handlers ((exn:fail?
                   (lambda (e) (exn-message e))))
  (let(
      (side1 (match (hash-ref json 'side1 (lambda () #f))
                [#f #f]
                [(? uuid-string? s) s]
                [X (error "side1 should be uuid, got " X)]))
      (side2 (match (hash-ref json 'side2 (lambda () #f))
                [#f #f]
                [(? uuid-string? s) s]
                [X (error "side2 should be uuid, got " X)]))
      (state (match (hash-ref json 'state (lambda () #f))
                [#f #f]
                [(? trade-state? s) s]
                [X (error "state should be a valid trade state, got " X)])))
  (match (list side1 side2 state)
    [(list #f #f #f) "must provide at least one of side1, side2, or state"]
    [(list s1 s2 st)
        (trade-search s1 s2 st)]
    [(list s1 #f st)
        (trade-search s1 #f st)]
    [(list s1 s2 #f)
        (trade-search s1 s2 #f)]
    [(list s1 #f #f)
        (trade-search s1 #f #f)]
    [(list #f s2 st)
      (trade-search #f s2 st)]
    [(list #f #f st)
      (trade-search #f #f st)]
    [(list #f s2 #f)
      (trade-search #f s2 #f)]))))

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
         (terms `(,@(match (trade-search-side1 search)
                      [#f '()]
                      [x `((side1 ,x))])
                  ,@(match (trade-search-side2 search)
                      [#f '()]
                      [x `((side2 ,x))])
                  ,@(match (trade-search-state search)
                      [#f '()]
                      [x `((state ,x))])))
         (q (string-append 
              "SELECT * FROM trades WHERE "
              (string-join (map (lambda (i x) 
                                  (match x 
                                    [(list col str) (format "$~a = ~a" i col)]))
                                (range 1 (+ 1 (length terms)))
                                terms)
                            " AND ")))
          (row->trade (lambda (r)
            (match r
              [(vector id side1 side2 state) (trade side1 side2 state)]))))                                                                 
   (map row->trade (apply query-rows conn q 
                     (map second terms)))))
 
(provide (all-defined-out))
