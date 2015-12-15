#lang racket

(define (compile)
  (define-values (ip op) (tcp-connect "localhost" 2112))

  (display "Enter a file name to compile: ")
  (define source (read-line))
  (define object (string-append source ".obj"))

  (with-input-from-file source
    (lambda ()
      (do ((inp (read-line) (read-line)))
        ((eof-object? inp)
          (flush-output op)
          (close-output-port op))
        (displayln inp op))))

  (with-output-to-file object 
    (lambda ()
      (do ((inp (read-line ip) (read-line ip)))
        ((eof-object? inp))
        (displayln inp)))
    #:exists 'replace))

(compile)

