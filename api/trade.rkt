#lang racket
(require "../trade.rkt")
(require web-server/dispatch)
(require web-server/servlet)
(require uuid)
(require json)


;; trade -> jsexpr?
(define (trade->jsexpr t)
    (match t
     [(trade side1 side2)
      (hash 'side1 side1
            'side2 side2)]))  

;; library -> request? -> any 
;; library -> dispatcher/c        
(define (API library)
 (define-values (trade-dispatch trade-url)
    (dispatch-rules
      [("trade" (string-arg)) #:method "get" (get-trade library)]))
 trade-dispatch)

;; request String -> any
(define (get-trade library)
 (lambda (request trade-id)
;; FIX ME. handle non-uuid strings
    ;(uuid-string? trade-id)
      (response/jsexpr
       (match (library-find-trade trade-id library)
        [#false "not found"]
        [X (trade->jsexpr X)]))))

                       
(provide API)