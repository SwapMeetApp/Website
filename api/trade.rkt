#lang racket
(require "../trade.rkt")
(require web-server/dispatch)
(require web-server/servlet)
(require web-server/http)
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
      [("trade" (string-arg)) #:method "get" (get-trade library)]
      [("trade") #:method "post" (create-trade library)]))
      ;; methods rest conventions
 trade-dispatch)

;; library -> request String -> any
(define (get-trade library)
 (lambda (request trade-id)
;; FIX ME. handle non-uuid strings
    ;(uuid-string? trade-id)
       (match (library-find-trade trade-id library)
        [#false (response/full
 404 #"Not Found" (current-seconds) #"application/json"
 '()
 (list (jsexpr->bytes (hash 'message "not found"))))]
        [X (response/jsexpr (trade->jsexpr X))])))

;; library -> request -> any
(define (create-trade library)
  (lambda (request)
    (match (request-post-data/raw request)
    [#false "fix me"]
    [body (response/jsexpr "to do")])))
     ; (match (parse-trade(bytes->jsexpr body))
                       
(provide API)