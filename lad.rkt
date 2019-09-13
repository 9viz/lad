;; lad - lunar (dismal) arithmetic repl thingy

#lang racket

(define (prepend-zeros num-zeros lst)
  (append (make-list num-zeros 0) lst))

(define (handle-lists lst1 lst2)
  (let ([n1 (length lst1)]
        [n2 (length lst2)])
    (let ([diff (- n1 n2)])
      (cond
        [(positive? diff) (values lst1 (prepend-zeros diff lst2))]
        [else (values (prepend-zeros (- diff) lst1) lst2)]))))

(define (list->num lst)
  (foldl
   (lambda (val dig)
     (+ (* dig 10) val))
   0
   lst))

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
        [n2s (extract-digits n2)])
    (set!-values (n1s n2s) (handle-lists n1s n2s))
    (list->num (map (lambda (num1 num2) (max num1 num2))
                    n1s n2s))))
