#lang racket

(displayln "Enter a file name to compile:")
(define infile (read-line))  ; "source.cm"

(define (compile)
  (define-values (ip op) (tcp-connect "localhost" 2112))

  (with-input-from-file infile
    (lambda ()
      (do ((inp (read-line) (read-line)))
        ((eof-object? inp))
        (displayln inp op))))

  (newline op)
  
  (flush-output op)

  (define object
    (string-append
     (substring infile 0 (- (string-length infile) 3))
     ".obj"
    ))
  
  (current-input-port ip)
  (current-output-port (open-output-file object #:mode 'text #:exists 'replace))
  
  (do ((inp (read-line) (read-line)))
        ((eof-object? inp))
        (displayln inp))
  )

(compile)
