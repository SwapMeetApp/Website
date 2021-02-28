#lang racket
(require net/rfc6455)

(struct chatserver (connections) #:mutable)

(define CHAT (chatserver '()))

(define (handle-websockets conn req)
 (set-chatserver-connections! CHAT (cons conn (chatserver-connections CHAT)))
  (thread (lambda () 
            (let loop ()
             (sync (handle-evt (ws-recv-evt conn)
                               (lambda (message)
                                (if (eof-object? message)
                                  (set-chatserver-connections! CHAT (remove conn (chatserver-connections CHAT)))
                                  (begin (broadcast message) (loop))))))))))

(define (broadcast message)
  (for-each (lambda (c) (unless (ws-conn-closed? c) (ws-send! c message)) 
            (chatserver-connections CHAT))))

(provide handle-websockets)