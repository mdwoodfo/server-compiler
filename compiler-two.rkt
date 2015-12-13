#lang racket
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

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
   [(:or #\tab #\space) (calcl input-port)]
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
    (arg-list [(arg-list COM exp) '()]
              [(exp) '()])
    (args [(arg-list) '()]
          [() '()])
    (call [(VAR OP args CP) '()])
    (factor [(OP exp CP) '()]
            [(var) '()]
            [(call) '()]
            [(NUM) '()])
    (mulop [(*) '()]
           [(/) '()])
    (term [(term mulop factor) '()]
          [(factor) '()])
    (addop [(+) '()]
           [(-) '()])
    (add-exp [(add-exp addop term) '()]
             [(term) '()])
    (relop [(LTE) '()]
           [(LT) '()]
           [(GT) '()]
           [(GTE) '()]
           [(EE) '()]
           [(NE) '()])
    (simple-exp [(add-exp relop add-exp) '()]
                [(add-exp) '()])
    (var [(VAR) '()]
         [(VAR OSB exp CSB) '()])
    (exp [(var = exp) '()]
         [(simple-exp) '()])
    (return-stmt [(RETURN SEMI) '()]
                 [(RETURN exp SEMI) '()])
    (iteration-stmt [(WHILE OP exp CP stmt) '()])
    (selection-stmt [(IF OP exp CP stmt) '()]
                    [(IF OP exp CP stmt ELSE stmt) '()])
    (exp-stmt [(exp SEMI) '()]
              [(SEMI) '()])
    (stmt [(exp-stmt) '()]
          [(compound-stmt) '()]
          [(selection-stmt) '()]
          [(iteration-stmt) '()]
          [(return-stmt) '()])
    (stmt-list [(stmt-list stmt) '()]
               [() '()])
    (local-dec [(local-dec var-dec) '()]
               [() '()])
    (compound-stmt [(OB local-dec stmt-list CB) '()])
    (param [(type VAR) '()]
           [(type VAR OSB CSB) '()])
    (param-list [(param-list COM param) '()]
                [(param) '()])
    (params [(param-list) '()]
            [(VOID) '()])
    (fun-dec [(type VAR OP params CP compound-stmt) '()])
    (type [(INT) '()]
          [(VOID) '()])
    (var-dec [(type VAR SEMI) '()]
             [(type VAR OSB NUM CSB) '()])
    (declaration [(var-dec) '()]
                 [(fun-dec) '()])
    (declaration-list [(declaration-list declaration) '()]
                      [(declaration) '()])
    (program [(declaration-list) '()])
    )))