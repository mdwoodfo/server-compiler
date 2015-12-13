#lang racket
;; NOTE: THIS DOES NOT YET HAVE THE FULL C- GRAMMAR!!!!!
;;       THIS IS AN EVOLUTION OF THE CALCULATOR CODE.

;; Import the parser and lexer generators.
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide comp)

;; -------------------
;; Zero Page Locations
;; -------------------
;; Math Variables
(define MATH1LO #x10) ;; augend, subtrahend
(define MATH1HI #x11) 
(define MATH2LO #x12) ;; addend, minuend
(define MATH2HI #x13)
(define MATH3LO #x14) ;; sum, difference
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
                                        LTE LT GT GTE EE NE EOF NEG INT VOID))

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
   [(:or "=" "+" "-" "*" "/" "^" ) (string->symbol lexeme)]
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
    (arg-list [(arg-list COM exp) (printf "arglist->arg-list, exp\n")]
              [(exp) (printf "arg-list->exp\n")])
    (args [(arg-list) (printf "args->arg-list\n")]
          [() (printf "args->EMPTY\n")])
    (call [(VAR OP args CP) (printf "call->VAR(args)\n")])
    (factor [(OP exp CP) (printf "factor->(exp)\n")]
            [(var) (printf "factor->var\n")]
            [(call) (printf "factor->call\n")]
            [(NUM) (printf "factor->NUM\n")])
    (mulop [(*) (printf "mulop->*\n")]
           [(/) (printf "mulop->/\n")])
    (term [(term mulop factor) (printf "term->term-mulop-factor\n")]
          [(factor) (printf "term->factor\n")])
    (addop [(+) (printf "addop->+\n")]
           [(-) (printf "addop->-\n")])
    (add-exp [(add-exp addop term) (printf "addexp->add-exp-addop-term\n")]
             [(term) (printf "add-exp->term\n")])
    (subexp [(term - term)(printf "subexp->term - term\n")])
    (subtraction-exp [(subtraction-exp + subexp)(printf "subtraction-exp->subtraction-exp + subexp\n")]
                  [(subexp)(printf "subtraction-exp->subexp\n")])
    (addexp [(term + term)(printf "addexp->term + term\n")])
    (addition-exp [(addition-exp + addexp)(printf "addition-exp->addition-exp + addexp\n")]
                  [(addexp)(printf "addition-exp->addexp\n")])
    (addsub [(addition-exp)(printf "addsub->addition-exp\n")]
            [(subtraction-exp)(printf "addsub->subtraction-exp\n")]
            [(term)(printf "addsub->term\n")])
    (relop [(LTE) (printf "")]
           [(LT) (printf "")]
           [(GT) (printf "")]
           [(GTE) (printf "")]
           [(EE) (printf "")]
           [(NE) (printf "")])
    (simple-exp [(addsub relop addsub) (printf "simple-exp->addsub_relop_addsub\n")]
                [(addsub) (printf "simple-exp->addsub\n")])
    (var [(VAR) (begin
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
         [(VAR OSB exp CSB) (printf "var->VAR(exp)\n")])
    (exp [(var = exp) (printf "exp->var = exp\n$1=~a $3=~a\n" $1 $3) ;(let-values ([(lo hi hexLo hexHi) (int->16bit $3)])
                        ;; Assembly Code
                        ;; Pull right-hand expression off Stack
;                        (printf "PLA~n")
;                        (printf "STA *$~a~n" (8bit->hex WORK1LO))
;                        (printf "PLA~n")
;                        (printf "STA *$~a~n" (8bit->hex WORK1HI))
                    ;(printf "68 85 ~a 68 85 ~a " 
                    ;        (8bit->hex WORK1LO)
                    ;        (8bit->hex WORK1HI))
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
                    ;(printf "A2 ~a E8 E8 A5 ~a 95 ~a 48 CA A5 ~a 95 ~a 48 " 
                    ;        (8bit->hex (symbol->var-lookup $1))
                    ;        (8bit->hex WORK1HI)
                    ;        (8bit->hex VARS)
                    ;        (8bit->hex WORK1LO)
                    ;        (8bit->hex VARS))
                    ;(hash-set! vars $1 $3)
                    ;$3)
                      ]
         [(simple-exp) (printf "exp->simpleexp\n")])
    (return-stmt [(RETURN SEMI) (printf "")]
                 [(RETURN exp SEMI) (printf "")])
    (iteration-stmt [(WHILE OP exp CP stmt) (printf "")])
    ;(selection-stmt [(IF OP exp CP stmt) (printf "")]
    ;                [(IF OP exp CP stmt ELSE stmt) (printf "")])
    (exp-stmt [(exp SEMI) (printf "exp-stmt->exp;\n")]
              [(SEMI) (printf "exp-stmt->;\n")])
    (stmt [(exp-stmt) (printf "stmt->expstmt\n")]
          [(compound-stmt) (printf "stmt->compoundstmt\n")]
    ;      [(selection-stmt) (printf "")]
          [(iteration-stmt) (printf "")]
          [(return-stmt) (printf "")])
    (stmt-list [(stmt-list stmt) (printf "stmtlist->stmtlist-stmt\n")]
               [() (printf "stmtlist->EMPTY\n")])
    (local-dec [(local-dec var-dec) (printf "local-dec->local-dec_var-dec\n")]
               [() (printf "local-dec->EMPTY\n")])
    (compound-stmt [(OB local-dec stmt-list CB) (printf "compound-stmt->{local-dec_stmt-list}\n")])
    (param [(type VAR) (printf "param->type-VAR\n")]
           [(type VAR OSB CSB) (printf "param->type-VAR[]\n")])
    (param-list [(param-list COM param) (printf "param-list->param-list,param\n")]
                [(param) (printf "param-list->param\n")])
    (params [(param-list) (printf "params->param-list\n")]
            [(VOID) (printf "params->void\n")])
    (fun-dec [(type VAR OP params CP compound-stmt) (printf "fun-dec->type_VAR(params)_compount-stmt\n")])
    (type [(INT) (printf "type->INT\n")]
          [(VOID) (printf "type->VOID\n")])
    (var-dec [(type VAR SEMI) (printf "var-dec->type_VAR_SEMI\n")]
             [(type VAR OSB NUM CSB) (printf "var-dec->type_VAR[NUM]\n")])
    (declaration [(var-dec) (printf "declaration->var-dec\n")]
                 [(fun-dec) (printf "declaration->fun-dec\n")])
    (declaration-list [(declaration-list declaration) (printf "declaration-list->declaration-list_declaration\n")]
                      [(declaration) (printf "declaration-list->declaration\n")])
    (program [(declaration-list) (printf "prog->dec-list\n")])
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
;                  (printf "PLA~n")
;                  (printf "STA *$~a~n" (8bit->hex IOINTL))
;                  (printf "PLA~n")
;                  (printf "STA *$~a~n" (8bit->hex IOINTH))
;                  (printf "STA *$~a~n" (8bit->hex IOFROB))
                  (printf "68 85 ~a 68 85 ~a 85 ~a " 
                          (8bit->hex IOINTL) 
                          (8bit->hex IOINTH)
                          (8bit->hex IOFROB))
                  ;; Scheme result
                  ;; (printf "~a\n" result)
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
