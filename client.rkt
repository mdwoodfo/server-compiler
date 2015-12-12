#lang racket

(displayln "Enter a file name to compile:")
(define infile (read-line))

(define (compile)
  (define-values (ip op) (tcp-connect "localhost" 2112))

  (with-input-from-file infile
    (lambda ()
      (do ((inp (read-line) (read-line)))
        ((eof-object? inp))
        (displayln inp op))))

  (newline op)
  
  (flush-output op)

  (current-input-port ip)
  
  (do ((inp (read-line) (read-line)))
        ((eof-object? inp))
        (displayln inp))
  )
(compile)