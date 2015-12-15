#lang racket
;; NOTE: THIS DOES NOT YET HAVE THE FULL C- GRAMMAR!!!!!
;;       THIS IS AN EVOLUTION OF THE CALCULATOR CODE.

;; Import the parser and lexer generators.
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide comp)

(define (printflush st . x)
  (apply printf st x)
  (flush-output))

;; -------------------
;; Zero Page Locations
;; -------------------
;; Math Variables
(define MATH1LO #x10) ;; augend, subtrahend, multiplier, dividend
(define MATH1HI #x11) 
(define MATH2LO #x12) ;; addend, minuend, multiplicand, divisor
(define MATH2HI #x13)
(define MATH3LO #x14) ;; sum, difference, product, quotient
(define MATH3HI #x15)
(define MATH4LO #x16)
(define MATH4HI #x17)
(define MATH5LO #x18)
(define MATH5HI #x19)
(define MATH6LO #x1A)
(define MATH6HI #x1B)
(define MATH7LO #x1C)
(define MATH7HI #x1D)
(define MATH8LO #x1E)
(define MATH8HI #x1F)

;; Work Space
(define WORK1LO #x20)
(define WORK1HI #x21)

;; Symbol Table
(define VARS #x30) ;$30 - $7F

;; Memory Mapped I/O
(define IOFROB #xFD)
(define IOINTL #xFE)
(define IOINTH #xFF)

;; ---------------
;; ROM Subroutines
;; ---------------
(define ADD #xF000)
(define SUBTRACT #xF00E)

;(define-tokens value-tokens (NUM VAR FNCT))
;(define-empty-tokens op-tokens (newline = OP CP OB CB + - * / ^ EOF NEG TYPE))

;; A hash table to store variable values in for the calculator
(define vars (make-hash))

(define-tokens value-tokens (NUM VAR FNCT))
(define-empty-tokens op-tokens (newline = OP CP OB CB OSB CSB + - * / ^ SEMI COM
                                        IF ELSE WHILE RETURN
                                        LTE LT GT GTE EE EQ NE EOF NEG INT VOID))

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
   [(:or #\tab #\space #\newline) (calcl input-port)]
   ;; (token-newline) returns 'newline
   [#\newline (token-newline)]
   ;; Since (token-=) returns '=, just return the symbol directly
   [(:or "+" "-" "*" "/" "^" ) (string->symbol lexeme)]
   ["(" 'OP]
   [")" 'CP]
   ["{" 'OB]
   ["}" 'CB]
   ["[" 'OSB]
   ["]" 'CSB]
   [";" 'SEMI]
   ["," 'COM]
   ["if" 'IF]
   ["<=" 'LTE]
   ["<" 'LT]
   [">" 'GT]
   [">=" 'GTE]
   ["==" 'EE]
   ["!=" 'NE]
   ["=" 'EQ]
   ["else" 'ELSE]
   ["while" 'WHILE]
   ["return" 'RETURN]
   ["int" 'INT]
   ["void" 'VOID]
   ["sin" (token-FNCT sin)]
   [(:+ (:or lower-letter upper-letter)) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]))

(define cminus
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
           [(program) $1])
    (arg-list [(arg-list COM exp) (printflush "")]
              [(exp) (printflush "")])
    (args [(arg-list) (printflush "")]
          [() (printflush "")])
    (call [(VAR OP args CP) (printflush "")])
    (factor [(OP exp CP) $2]
            [(var) (begin (printflush "") $1)]
            [(call) (printflush "")]
            [(NUM) (let-values ([(lo hi hexLo hexHi) (int->16bit $1)])
                     ;; Assembly Code
                     ;; Push number onto stack
                     ;                  (printflush "LDA #$~a~n" hexHi)
                     ;                  (printflush "PHA~n")
                     ;                  (printflush "LDA #$~a~n" hexLo)
                     ;                  (printflush "PHA~n")
                     (printflush "A9 ~a 48 A9 ~a 48 " hexHi hexLo)
                     $1)
                   ])
    (muldiv [(multiplication-exp) (begin (printflush "") $1)]
            [(division-exp) (begin (printflush "") $1)]
            [(factor) (begin (printflush "") $1)])
    (multiplication-exp [(multiplication-exp * factor) (begin (printflush "") (* $1 $3))]
                        [(mulexp) (begin (printflush "") $1)])
    (division-exp [(division-exp * factor) (begin (printflush "") (/ $1 $3))]
                  [(divexp) (begin (printflush "") $1)])
    (mulexp [(factor * factor) (let-values ([(lo hi hexLo hexHi) (int->16bit SUBTRACT)])
                                 ;                        (printflush "PLA~n")
                                 (printflush "68 ")
                                 ;                        (printflush "STA *$~a~n" (8bit->hex MATH2LO))
                                 (printflush "85 ~a " (8bit->hex MATH2LO))
                                 ;                        (printflush "PLA~n")
                                 (printflush "68 ")
                                 ;                        (printflush "STA *$~a~n" (8bit->hex MATH2HI))
                                 (printflush "85 ~a " (8bit->hex MATH2HI))
                                 ;; Pull Left-hand expression off Stack
                                 ;; Store in Subtrahend workspace
                                 ;                        (printflush "PLA~n")
                                 (printflush "68 ")
                                 ;                        (printflush "STA *$~a~n" (8bit->hex MATH1LO))
                                 (printflush "85 ~a " (8bit->hex MATH1LO))
                                 ;                        (printflush "PLA~n")
                                 (printflush "68 ")
                                 ;                        (printflush "STA *$~a~n" (8bit->hex MATH1HI))
                                 (printflush "85 ~a " (8bit->hex MATH1HI))
                                 (printflush "A9 00 85 ~a 85 ~a A2 10 "
                                             (8bit->hex MATH4LO) (8bit->hex MATH4HI))
                                 (printflush "46 ~a 66 ~a 90 0B A5 ~a 18 65 ~a 85 ~a A5 ~a 65 ~a "
                                             (8bit->hex MATH2HI)
                                             (8bit->hex MATH2LO)
                                             (8bit->hex MATH4LO)
                                             (8bit->hex MATH1LO)
                                             (8bit->hex MATH4LO)
                                             (8bit->hex MATH4HI)
                                             (8bit->hex MATH1HI))
                                 (printflush "6A 85 ~a 66 ~a 66 ~a 66 ~a CA D0 E3 "
                                             (8bit->hex MATH4HI)
                                             (8bit->hex MATH4LO)
                                             (8bit->hex MATH3HI)
                                             (8bit->hex MATH3LO))
                                 ;; Push product onto Stack
                                 (printflush "A5 ~a 48 A5 ~a 48 " 
                                             (8bit->hex MATH3HI)
                                             (8bit->hex MATH3LO))
                                 ;(8bit->hex MATH4HI)
                                 ;(8bit->hex MATH4LO))
                                 (* $1 $3))])
    (divexp [(factor / factor) (let-values ([(lo hi hexLo hexHi) (int->16bit SUBTRACT)])
                                 (printflush "A9 00 85 ~a 85 ~a A2 10 "
                                             (8bit->hex MATH3LO) (8bit->hex MATH3HI))
                                 (printflush "A9 00 85 ~a 85 ~a A2 10 "
                                             (8bit->hex MATH2LO) (8bit->hex MATH2HI))
                                 (printflush "A9 00 85 ~a 85 ~a A2 10 "
                                             (8bit->hex MATH1LO) (8bit->hex MATH1HI))
                                 (printflush "A9 00 85 ~a 85 ~a A2 10 "
                                             (8bit->hex WORK1LO) (8bit->hex WORK1HI))
                                 ;                        (printflush "PLA~n")
                                 (printflush "68 ")
                                 ;                        (printflush "STA *$~a~n" (8bit->hex MATH2LO))
                                 (printflush "85 ~a " (8bit->hex MATH2LO))
                                 ;                        (printflush "PLA~n")
                                 (printflush "68 ")
                                 ;                        (printflush "STA *$~a~n" (8bit->hex MATH2HI))
                                 (printflush "85 ~a " (8bit->hex MATH2HI))
                                 ;; Pull Left-hand expression off Stack
                                 ;; Store in Subtrahend workspace
                                 ;                        (printflush "PLA~n")
                                 (printflush "68 ")
                                 ;                        (printflush "STA *$~a~n" (8bit->hex MATH1LO))
                                 (printflush "85 ~a " (8bit->hex MATH1LO))
                                 ;                        (printflush "PLA~n")
                                 (printflush "68 ")
                                 ;                        (printflush "STA *$~a~n" (8bit->hex MATH1HI))
                                 (printflush "85 ~a " (8bit->hex MATH1HI))
                                 (printflush "C8 06 ~a 26 ~a 90 F9 66 ~a 66 ~a "
                                             (8bit->hex MATH2LO)
                                             (8bit->hex MATH2HI)
                                             (8bit->hex MATH2HI)
                                             (8bit->hex MATH2LO))
                                 (printflush "88 06 ~a 26 ~a 38 A5 ~a E5 ~a 90 14 "
                                             (8bit->hex MATH3LO)
                                             (8bit->hex MATH3HI)
                                             (8bit->hex MATH1HI)
                                             (8bit->hex MATH2HI))
                                 (printflush "85 ~a A5 ~a E5 ~a 90 0C "
                                             (8bit->hex WORK1LO)
                                             (8bit->hex MATH1LO)
                                             (8bit->hex MATH2LO))
                                 (printflush "85 ~a A5 ~a 85 ~a A9 01 05 ~a 85 ~a "
                                             (8bit->hex MATH1LO)
                                             (8bit->hex WORK1LO)
                                             (8bit->hex MATH1HI)
                                             (8bit->hex MATH3LO)
                                             (8bit->hex MATH3LO))
                                 (printflush "46 ~a 66 ~a C0 01 10 D8 "
                                             (8bit->hex MATH2HI)
                                             (8bit->hex MATH2LO))
                                 ;; Push product onto Stack
                                 (printflush "A5 ~a 48 A5 ~a 48 " 
                                             (8bit->hex MATH3HI)
                                             (8bit->hex MATH3LO))
                                 (quotient $1 $3))])
    (term [(muldiv) (begin (printflush "") $1)])
    (subexp [(term - term) (let-values ([(lo hi hexLo hexHi) (int->16bit SUBTRACT)])
                             ;; Assembly Code
                             ;; Pull Right-hand expression off Stack
                             ;; Store in Minuend workspace
                             ;                        (printflush "PLA~n")
                             (printflush "68 ")
                             ;                        (printflush "STA *$~a~n" (8bit->hex MATH2LO))
                             (printflush "85 ~a " (8bit->hex MATH2LO))
                             ;                        (printflush "PLA~n")
                             (printflush "68 ")
                             ;                        (printflush "STA *$~a~n" (8bit->hex MATH2HI))
                             (printflush "85 ~a " (8bit->hex MATH2HI))
                             ;; Pull Left-hand expression off Stack
                             ;; Store in Subtrahend workspace
                             ;                        (printflush "PLA~n")
                             (printflush "68 ")
                             ;                        (printflush "STA *$~a~n" (8bit->hex MATH1LO))
                             (printflush "85 ~a " (8bit->hex MATH1LO))
                             ;                        (printflush "PLA~n")
                             (printflush "68 ")
                             ;                        (printflush "STA *$~a~n" (8bit->hex MATH1HI))
                             (printflush "85 ~a " (8bit->hex MATH1HI))
                             ;; Call SUBTRACT subroutine
                             ;                        (printflush "JSR $~a~a~n" hexHi hexLo)
                             (printflush "20 ~a ~a " hexLo hexHi) ;; <--- byte reversal!!
                             ;; Push difference onto Stack
                             ;                        (printflush "LDA *$~a~n" (8bit->hex MATH3HI))
                             (printflush "A5 ~a " (8bit->hex MATH3HI))
                             ;                        (printflush "PHA~n")
                             (printflush "48 ")
                             ;                        (printflush "LDA *$~a~n" (8bit->hex MATH3LO))
                             (printflush "A5 ~a " (8bit->hex MATH3LO))
                             ;                        (printflush "PHA~n")
                             (printflush "48 ")
                             (- $1 $3))])
    (subtraction-exp [(subtraction-exp - term) (begin (printflush "") (- $1 $3))]
                     [(subexp) (begin (printflush "") $1)])
    (addexp [(term + term) (let-values ([(lo hi hexLo hexHi) (int->16bit ADD)])
                             ;; Assembly Code
                             ;; Pull Right-hand expression off Stack
                             ;; Store in Addend workspace
                             ;                        (printflush "PLA~n")
                             ;                        (printflush "STA *$~a~n" (8bit->hex MATH2LO))
                             ;                        (printflush "PLA~n")
                             ;                        (printflush "STA *$~a~n" (8bit->hex MATH2HI))
                             (printflush "68 85 ~a 68 85 ~a " 
                                         (8bit->hex MATH2LO)
                                         (8bit->hex MATH2HI))
                             ;; Pull Left-hand expression off Stack
                             ;; Store in Augend workspace
                             ;                        (printflush "PLA~n")
                             ;                        (printflush "STA *$~a~n" (8bit->hex MATH1LO))
                             ;                        (printflush "PLA~n")
                             ;                        (printflush "STA *$~a~n" (8bit->hex MATH1HI))
                             (printflush "68 85 ~a 68 85 ~a " 
                                         (8bit->hex MATH1LO)
                                         (8bit->hex MATH1HI))
                             ;; Call ADD subroutine
                             ;                        (printflush "JSR $~a~a~n" hexHi hexLo)
                             (printflush "20 ~a ~a " hexLo hexHi) ;; <==== byte reversal!!!
                             ;; Push sum onto Stack
                             (printflush "A5 ~a 48 A5 ~a 48 " 
                                         (8bit->hex MATH3HI)
                                         (8bit->hex MATH3LO))
                             (+ $1 $3))])
    (addition-exp [(addexp) $1]
                  [(addition-exp + term) (+ $1 $3)])
    (addsub [(addition-exp) $1]
            [(subtraction-exp) $1]
            [(term) $1])
    (relop [(LTE) (printflush "")]
           [(LT) (printflush "")]
           [(GT) (printflush "")]
           [(GTE) (printflush "")]
           [(EE) (printflush "")]
           [(NE) (printflush "")])
    (simple-exp [(addsub relop addsub) (printflush "")]
                [(addsub) $1])
    (var [(VAR) (begin
                  ;; Assembly Code
                  ;; Retrieve variable value from Symbol Table
                  ;; Push value onto Stack
                  ;                  (printflush "LDX #$~a~n" (8bit->hex (symbol->var-lookup $1)))
                  ;                  (printflush "INX~n")
                  ;                  (printflush "INX~n")
                  ;                  (printflush "LDA *$~a,X~n" (8bit->hex VARS))
                  ;                  (printflush "PHA~n")
                  ;                  (printflush "DEX~n")
                  ;                  (printflush "LDA *$~a,X~n" (8bit->hex VARS))
                  ;                  (printflush "PHA~n")
                  (printflush "A2 ~a E8 E8 B5 ~a 48 CA B5 ~a 48 " 
                              (8bit->hex (symbol->var-lookup $1))
                              (8bit->hex VARS)
                              (8bit->hex VARS))
                  (hash-ref vars $1 -1))]
         [(VAR OSB exp CSB) (printflush "")])
    (exp [(VAR EQ exp) (let-values ([(lo hi hexLo hexHi) (int->16bit $3)])
                         ;; Assembly Code
                         ;; Pull right-hand expression off Stack
                         ;                        (printflush "PLA~n")
                         ;                        (printflush "STA *$~a~n" (8bit->hex WORK1LO))
                         ;                        (printflush "PLA~n")
                         ;                        (printflush "STA *$~a~n" (8bit->hex WORK1HI))
                         (printflush "68 85 ~a 68 85 ~a " 
                                     (8bit->hex WORK1LO)
                                     (8bit->hex WORK1HI))
                         ;; Lookup variable location in Symbol Table
                         ;; Store expression value in variable
                         ;; Push value onto Stack
                         ;                        (printflush "LDX #$~a~n" (8bit->hex (symbol->var-lookup $1)))
                         ;                        (printflush "INX~n")
                         ;                        (printflush "INX~n")
                         ;                        (printflush "LDA *$~a~n" (8bit->hex WORK1HI))
                         ;                        (printflush "STA *$~a,X~n" (8bit->hex VARS))
                         ;                        (printflush "PHA~n")
                         ;                        (printflush "DEX~n")
                         ;                        (printflush "LDA *$~a~n" (8bit->hex WORK1LO))
                         ;                        (printflush "STA *$~a,X~n" (8bit->hex VARS))
                         ;                        (printflush "PHA~n")
                         (printflush "A2 ~a E8 E8 A5 ~a 95 ~a 48 CA A5 ~a 95 ~a 48 " 
                                     (8bit->hex (symbol->var-lookup $1))
                                     (8bit->hex WORK1HI)
                                     (8bit->hex VARS)
                                     (8bit->hex WORK1LO)
                                     (8bit->hex VARS))
                         (hash-set! vars $1 $3)
                         $3)
                       ]
         [(simple-exp) $1])
    (return-stmt [(RETURN SEMI) (printflush "")]
                 [(RETURN exp SEMI) (printflush "")])
    (iteration-stmt [(WHILE OP exp CP stmt) (printflush "")])
    ;(selection-stmt [(IF OP exp CP stmt) (printflush "")]
    ;                [(IF OP exp CP stmt ELSE stmt) (printflush "")])
    (exp-stmt [(exp SEMI) (printflush "")]
              [(SEMI) (printflush "")])
    (stmt [(exp-stmt) (printflush "")]
          [(compound-stmt) (printflush "")]
          ;      [(selection-stmt) (printflush "")]
          [(iteration-stmt) (printflush "")]
          [(return-stmt) (printflush "")])
    (stmt-list [(stmt-list stmt) (printflush "")]
               [() (printflush "")])
    (local-dec [(local-dec var-dec) (printflush "")]
               [() (printflush "")])
    (compound-stmt [(OB local-dec stmt-list CB) (printflush "")])
    (param [(type VAR) (printflush "")]
           [(type VAR OSB CSB) (printflush "")])
    (param-list [(param-list COM param) (printflush "")]
                [(param) (printflush "")])
    (params [(param-list) (printflush "")]
            [(VOID) (printflush "")])
    (fun-dec [(type VAR OP params CP compound-stmt) (printflush "")])
    (type [(INT) (printflush "")]
          [(VOID) (printflush "")])
    (var-dec [(type VAR SEMI) (begin (hash-set! vars $2 0) (printflush "") $1)]
             [(type VAR OSB NUM CSB) (printflush "")])
    (declaration [(var-dec) (printflush "")]
                 [(fun-dec) (printflush "")])
    (declaration-list [(declaration-list declaration) (printflush "")]
                      [(declaration) (printflush "")])
    (program [(declaration-list) (printflush "")])
    )))

;; -------------------------------
;; DECIMAL / BINARY NUMBER FORMATS
;; -------------------------------
;; Convert an integer to the 6502 Lo/Hi-byte 16-bit Integer format.
;; Returns bytes and integers and hex strings.
(define (int->16bit arg)
  (let ((i (bitwise-and arg #xFFFF))
        (lo 0)
        (hi 0))
    (when (< i 0) 
      ;; Use Two's Compliment for negative numbers
      (set! i (+ (bitwise-xor i #xFFFF) 1)))
    (set! lo (bitwise-and i #x00FF))
    (set! hi (/ (bitwise-and i #xFF00) 256))
    (values lo hi 
            (8bit->hex lo)
            (8bit->hex hi))))

;; Convert 6502 Lo/Hi-bytes to an integer.
(define (16bit->int lo hi)
  (let ((i 0))
    (cond ((> (bitwise-and hi #x80) 0) 
           ;; Use Two's Compliment for negative numbers
           (set! i (- (+ (* (bitwise-xor hi #xFF) 256) (bitwise-xor lo #xFF) 1))))
          (#t (set! i (+ (* hi 256) lo))))
    i))

;; Converts an 8-bit integer (< 256) to a 2-digit hex string.
(define (8bit->hex number)
  (let ((padded (string-upcase (string-append "0" (number->string number 16)))))
    (substring padded (- (string-length padded) 2))))

;; Do the math locally to calculate index of variable in Symbol Table
(define (symbol->var-lookup var)
  (*
   (- (bytes-ref (string->bytes/utf-8 (string-upcase (symbol->string var))) 0)
      65)
   3))

;; run the calculator on the given input-port       
(define (comp ip op)
  ;  (display "*=$D000")
  (current-output-port op)
  (display "D000") ;; NOT LITTLE-ENDIAN!!!!!
  (newline)
  (port-count-lines! ip)
  (letrec ((one-line
            (lambda ()
              (let ((result (cminus (lambda () (calcl ip)))))
                (when result
                  ;; Assembly Code
                  ;; EOL pull last value off stack and print result
                  ;                  (printflush "PLA~n")
                  ;                  (printflush "STA *$~a~n" (8bit->hex IOINTL))
                  ;                  (printflush "PLA~n")
                  ;                  (printflush "STA *$~a~n" (8bit->hex IOINTH))
                  ;                  (printflush "STA *$~a~n" (8bit->hex IOFROB))
                  (printflush "68 85 ~a 68 85 ~a 85 ~a " 
                              (8bit->hex IOINTL) 
                              (8bit->hex IOINTH)
                              (8bit->hex IOFROB))
                  ;; Scheme result
                  ;; (printflush "~a\n" result)
                  (one-line))))))
    (one-line))
  ;  (display "HLT") ;; Hex 02
  (display "02") ;; Hex 02
  (newline)
  ;  (display ".end")
  (display "FFFFFF") ;; Hex 02
  ;  (newline)
  (flush-output op)
  )

;; Some examples
;(comp (open-input-string "a=1\nb=1234\na + (b - 20)"))

(define (compile-file source object)
  (comp (open-input-file source)
        (open-output-file object #:mode 'text #:exists 'replace)))
