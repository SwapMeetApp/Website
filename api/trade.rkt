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
    [(trade side1 side2 state)
     (hash 'side1 side1
           'side2 side2
           'state state)]))  

;; number bytes string -> reponse?
(define (response/error code message-bytes error-message)
  (response/full
   code message-bytes (current-seconds) #"application/json"
   '()
   (list (jsexpr->bytes (hash 'message error-message )))))

;; library -> request? -> any 
;; library -> dispatcher/c        
(define (API library)
  (define-values (trade-dispatch trade-url)
    (dispatch-rules
     [("trade") #:method "post" (create-trade! library)]
     [("trade" (string-arg)) #:method "get" (get-trade! library)]
     [("trade" (string-arg)) #:method "put" (update-trade! library)]
     [("trade" (string-arg)) #:method "delete" (delete-trade! library)]
     [("trades") #:method "get" (find-trades! library)]))
  trade-dispatch)

;; library -> request -> response
(define (create-trade! library)
  (lambda (request)
    (match (request-post-data/raw request)
      [#false (response/error 400 #"Bad Request" "empty body")]
      [body
       (let ((json (bytes->jsexpr body)))
         (if 
          (jsexpr? json) 
          (match (parse-trade json)
            [(list _ (? string? error-message)) (response/error 400 #"Bad Request" error-message)]
            [trade 
             (match (library-insert-trade! trade library)
               [#false (response/error 500 #"Internal Server Error" "Unknown error creating trade")]
               [id (response/jsexpr id)])])
          (response/error 400 #"Bad Request" "invalid json")))])))

;; library -> request String -> any
(define (get-trade! library)
  (lambda (request trade-id)
    (match (uuid-string? trade-id)
      [#false (response/error 400 #"Bad Request" "invalid trade-id")]
      [#true
       (match (library-find-trade! trade-id library)
         [#false (response/error 404 #"Not Found" "not found")]
         [trade (response/jsexpr (trade->jsexpr trade))])])))

;; library -> request String -> any
(define (update-trade! library)
  (lambda (request trade-id)
    (match (uuid-string? trade-id)
      [#false (response/error 400 #"Bad Request" "invalid trade-id")]
      [#true
       (match (request-post-data/raw request)
         [#false (response/error 400 #"Bad Request" "empty body")]
         [body
           (let ((json (bytes->jsexpr body)))
             (if
               (jsexpr? json)
               (match (parse-trade json)
                 [(list _ (? string? error-message)) (response/error 400 #"Bad Request" error-message)]
                 [trade
                   (match (library-update-trade! trade-id trade library)
                     [#false (response/error 404 #"Not Found" "not found")]
                     [id (response/jsexpr id)])])
               (response/error 400 #"Bad Request" "invalid json")))])])))

;; library -> request String -> any
(define (delete-trade! library)
  (lambda (request trade-id)
    (match (uuid-string? trade-id)
      [#false (response/error 400 #"Bad Request" "invalid trade-id")]
      [#true
       (match (library-delete-trade! trade-id library)
	 [#false (response/error 404 #"Not Found" "not found")]
	 [id (response/jsexpr id)])])))


;; complete find-trades hook up with library-search-trade
;; add powershell script
;; possible refactor library-search-trades
(define (find-trades! library)
  (lambda (request)
    (let* ((params (url-query(request-uri request))))
      (match (params->trade-search params)
        [(? string? error) (response/error 400 #"Bad Request" error)]
        [search ;; handle errors from library search trades
          (match (map (trade->jsexpr (library-search-trades! search library)))
            [(? list? x) (response/jsexpr x)]
            [ _ (response/error 400 #"Bad Request" "invalid json")])]))))
           
    


(provide API)
