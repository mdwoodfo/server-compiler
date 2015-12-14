#lang racket
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUM VAR FNCT))
(define-empty-tokens op-tokens (newline = OP CP OB CB + - * / ^ EOF NEG TYPE))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  ;; (:/ 0 9) would not work because the lexer does not understand numbers.
  ;; But (:/ #\0 #\9) is ok.
  (digit (:/ "0" "9")))

(define calcl
  (lexer
   [(eof) 'EOF]
   ;; recursively call the lexer on the remaining input after a tab or space.
   ;; Returning the result of that operation.  
   ;; This effectively skips all whitespace.
   [(:or #\tab #\space) (calcl input-port)]
   ;; (token-newline) returns 'newline
   [#\newline (token-newline)]
   ;; Since (token-=) returns '=, just return the symbol directly
   [(:or "=" "+" "-" "*" "/" "^") (string->symbol lexeme)]
   ["(" 'OP]
   [")" 'CP]
   ["{" 'OB]
   ["}" 'CB]
   [(:or "int" "void") 'TYPE]
   ["sin" (token-FNCT sin)]
   [(:+ (:or lower-letter upper-letter)) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]))

(define calcp
  (parser
   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))
   (precs (right =)
          (left - +)
          (left * /)
          (left NEG)
          (right ^))
   (grammar
    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(exp) $1])

    (fun-declaration [(TYPE VAR OP params CP OB localdec stmtlst CB) (printf "funct")])
    (params [(TYPE) (printf "params")])
    (localdec [(localdec VAR = exp) (printf "")]
              [() (printf "")])
    (stmtlst [() (printf "stmtlst")])
    
    (exp
     [(fun-declaration) (printf "funct")]
     [(params)(printf "params")]
     [(localdec)(printf "localdecfda ")]
     [(stmtlst)(printf "stmtlst")]
     [(NUM) (let-values ([(lo hi hexLo hexHi) (int->16bit $1)])
                  ;; Assembly Code
                  ;; Push number onto stack
;                  (printf "LDA #$~a~n" hexHi)
;                  (printf "PHA~n")
;                  (printf "LDA #$~a~n" hexLo)
;                  (printf "PHA~n")
                  (printf "A9 ~a 48 A9 ~a 48 " hexHi hexLo)
                  $1)
                ]
     [(VAR) (begin
              ;; Assembly Code
              ;; Retrieve variable value from Symbol Table
              ;; Push value onto Stack
;                  (printf "LDX #$~a~n" (8bit->hex (symbol->var-lookup $1)))
;                  (printf "INX~n")
;                  (printf "INX~n")
;                  (printf "LDA *$~a,X~n" (8bit->hex VARS))
;                  (printf "PHA~n")
;                  (printf "DEX~n")
;                  (printf "LDA *$~a,X~n" (8bit->hex VARS))
;                  (printf "PHA~n")
              (printf "A2 ~a E8 E8 B5 ~a 48 CA B5 ~a 48 " 
                      (8bit->hex (symbol->var-lookup $1))
                      (8bit->hex VARS)
                      (8bit->hex VARS))
              (hash-ref vars $1 (lambda () 0)))]
     [(VAR = exp) (let-values ([(lo hi hexLo hexHi) (int->16bit $3)])
                        ;; Assembly Code
                        ;; Pull right-hand expression off Stack
;                        (printf "PLA~n")
;                        (printf "STA *$~a~n" (8bit->hex WORK1LO))
;                        (printf "PLA~n")
;                        (printf "STA *$~a~n" (8bit->hex WORK1HI))
                    (printf "68 85 ~a 68 85 ~a " 
                            (8bit->hex WORK1LO)
                            (8bit->hex WORK1HI))
                        ;; Lookup variable location in Symbol Table
                        ;; Store expression value in variable
                        ;; Push value onto Stack
;                        (printf "LDX #$~a~n" (8bit->hex (symbol->var-lookup $1)))
;                        (printf "INX~n")
;                        (printf "INX~n")
;                        (printf "LDA *$~a~n" (8bit->hex WORK1HI))
;                        (printf "STA *$~a,X~n" (8bit->hex VARS))
;                        (printf "PHA~n")
;                        (printf "DEX~n")
;                        (printf "LDA *$~a~n" (8bit->hex WORK1LO))
;                        (printf "STA *$~a,X~n" (8bit->hex VARS))
;                        (printf "PHA~n")
                    (printf "A2 ~a E8 E8 A5 ~a 95 ~a 48 CA A5 ~a 95 ~a 48 " 
                            (8bit->hex (symbol->var-lookup $1))
                            (8bit->hex WORK1HI)
                            (8bit->hex VARS)
                            (8bit->hex WORK1LO)
                            (8bit->hex VARS))
                    (hash-set! vars $1 $3)
                    $3)]
     [(FNCT OP exp CP) ($1 $3)]
     [(exp + exp) (let-values ([(lo hi hexLo hexHi) (int->16bit ADD)])
                        ;; Assembly Code
                        ;; Pull Right-hand expression off Stack
                        ;; Store in Addend workspace
;                        (printf "PLA~n")
;                        (printf "STA *$~a~n" (8bit->hex MATH2LO))
;                        (printf "PLA~n")
;                        (printf "STA *$~a~n" (8bit->hex MATH2HI))
                    (printf "68 85 ~a 68 85 ~a " 
                            (8bit->hex MATH2LO)
                            (8bit->hex MATH2HI))
                        ;; Pull Left-hand expression off Stack
                        ;; Store in Augend workspace
;                        (printf "PLA~n")
;                        (printf "STA *$~a~n" (8bit->hex MATH1LO))
;                        (printf "PLA~n")
;                        (printf "STA *$~a~n" (8bit->hex MATH1HI))
                    (printf "68 85 ~a 68 85 ~a " 
                            (8bit->hex MATH1LO)
                            (8bit->hex MATH1HI))
                        ;; Call ADD subroutine
;                        (printf "JSR $~a~a~n" hexHi hexLo)
                    (printf "20 ~a ~a " hexLo hexHi) ;; <==== byte reversal!!!
                        ;; Push sum onto Stack
                    (printf "A5 ~a 48 A5 ~a 48 " 
                            (8bit->hex MATH3HI)
                            (8bit->hex MATH3LO))
                    (+ $1 $3))
                  ]
     [(exp - exp) (let-values ([(lo hi hexLo hexHi) (int->16bit SUBTRACT)])
                        ;; Assembly Code
                        ;; Pull Right-hand expression off Stack
                        ;; Store in Minuend workspace
;                        (printf "PLA~n")
                    (printf "68 ")
;                        (printf "STA *$~a~n" (8bit->hex MATH2LO))
                    (printf "85 ~a " (8bit->hex MATH2LO))
;                        (printf "PLA~n")
                    (printf "68 ")
;                        (printf "STA *$~a~n" (8bit->hex MATH2HI))
                    (printf "85 ~a " (8bit->hex MATH2HI))
                        ;; Pull Left-hand expression off Stack
                        ;; Store in Subtrahend workspace
;                        (printf "PLA~n")
                        (printf "68 ")
;                        (printf "STA *$~a~n" (8bit->hex MATH1LO))
                        (printf "85 ~a " (8bit->hex MATH1LO))
;                        (printf "PLA~n")
                        (printf "68 ")
;                        (printf "STA *$~a~n" (8bit->hex MATH1HI))
                        (printf "85 ~a " (8bit->hex MATH1HI))
                        ;; Call SUBTRACT subroutine
;                        (printf "JSR $~a~a~n" hexHi hexLo)
                        (printf "20 ~a ~a " hexLo hexHi) ;; <--- byte reversal!!
                        ;; Push difference onto Stack
;                        (printf "LDA *$~a~n" (8bit->hex MATH3HI))
                        (printf "A5 ~a " (8bit->hex MATH3HI))
;                        (printf "PHA~n")
                        (printf "48 ")
;                        (printf "LDA *$~a~n" (8bit->hex MATH3LO))
                        (printf "A5 ~a " (8bit->hex MATH3LO))
;                        (printf "PHA~n")
                        (printf "48 ")
                        (- $1 $3))
                      ]
         [(exp * exp) (* $1 $3)]
         [(exp / exp) (/ $1 $3)]
         [(- exp) (prec NEG) (- $2)]
         [(OP exp CP) $2]))))