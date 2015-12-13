#lang racket
(require "compiler.rkt")

;; The `serve' function is revised to run the loop
;; in a thread, and it returns a function to shut down
;; down the server.

(define (serve port-no)
  (define listener (tcp-listen port-no 5 #t))
  (define (loop)
    (accept-and-handle listener)
      (loop))
  (define t (thread loop))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))

;; The rest is the same as before.

(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (handle in out)
  (close-input-port in)
  (close-output-port out))

(define (handle in out)
  ;; Discard the request header (up to blank line):
  (comp in out)
  ;(define thing (read-line in))
  ;(displayln thing)
  ;(displayln thing out)
  ;(comp in out)
  ;; Send reply:
  ;(display "HTTP/1.0 200 Okay\r\n" out)
  ;(display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  ;(display "<html><body>Hello, world!</body></html>" out)
  )

(define (launch-server port)
  (serve port))
  