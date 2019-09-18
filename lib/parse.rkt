;; based on the example calc.rkt in parser-tools

#lang racket

(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "lunar.rkt")

(provide leval)

(define-tokens value-tokens (NUM))
(define-empty-tokens op-tokens (newline EOF OP CP + * ! ^))

(define-lex-abbrevs
 (digit (:/ #\0 #\9)))

(define lex
  (lexer
   [(eof) 'EOF]
   [(:or #\tab #\space) (lex input-port)]
   [#\newline (token-newline)]
   [(:or "+" "*" "!" "^") (string->symbol lexeme)]
   ["(" 'OP]
   [")" 'CP]
   [(:+ digit) (token-NUM (string->number lexeme))]))

(define parse
  (parser
   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error (λ (a b c) (void)))
   (precs (left +)
          (left *)
          (left ^)
          (left !))
   (grammar
    (start [() #f]
           [(error start) $2]
           [(exp) $1])
    (exp [(NUM) $1]
         [(exp + exp) (ladd $1 $3)]
         [(exp * exp) (lmult $1 $3)]
         [(exp ^ exp) ($1)]
         [(exp !) (prec !) (lfactorial $1)]
         [(OP exp CP) $2]))))

(define (leval expr)
  (parse (λ () (lex expr))))
