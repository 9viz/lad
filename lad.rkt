;; lad - lunar (dismal) arithmetic repl thingy

#lang racket

(define (prepend-zeros num-zeros lst)
  (append (make-list num-zeros 0) lst))

(define extract-digits
  (lambda (num [digits '()])
  (cond
    [(= num 0) (reverse digits)]
    [else
     (set! digits (append digits
                          [list (modulo num 10)]))
     (extract-digits (quotient num 10)
                     digits)])))

(define (ladd n1 n2)
  (let ([n1s (extract-digits n1)]
        [n2s (extract-digits n2)]
    (let ([ln1 (length n1s)]
          [ln2 (length n2s)])
      (cond
        [(> ln1 ln2) (set! n2s
                            (prepend-zeros (- ln1 ln2) n2s))]
        [(< ln1 ln2) (set! n1s
                           (prepend-zeros (- ln2 ln1) n1s))])
      (map (lambda (num1 num2) (max num1 num2))
           n1s n2s))))
