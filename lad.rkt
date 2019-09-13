;; lad - lunar (dismal) arithmetic repl thingy

#lang racket

(define (list->num lst)
  (foldl
   (lambda (val dig) [+ (* dig 10) val])
   0
   lst))

(define extract-digits
  (lambda (num [digits '()])
    (cond
      [(= num 0) (reverse digits)]
      [else
       (extract-digits (quotient num 10)
                       (append digits [list (modulo num 10)]))])))

(define (ladd/prepend-zeros num-digits lst)
  (append (make-list num-digits 0) lst))

(define (ladd/handle-nums-primitive lst1 lst2)
  (let ([diff (- (length lst1) (length lst2))])
    (cond
      [(positive? diff) (values lst1 (ladd/prepend-zeros diff lst2))]
      [else (values (ladd/prepend-zeros (- diff) lst1) lst2)])))

(define (ladd/handle-nums num1 num2)
  (let ([lst1 (extract-digits num1)]
        [lst2 (extract-digits num2)])
    (ladd/handle-nums-primitive lst1 lst2)))

(define (ladd n1 n2)
  (let-values ([(d1 d2) (ladd/handle-nums n1 n2)])
    (list->num (map max d1 d2))))

(define (lmult/append-zeros num-digits lst)
  (append lst (make-list num-digits 0)))

(define (lmult/mult-single ns n)
  (map (lambda (d) (min d n)) ns))

(define (lmult n1 n2)
  (let ([cnt 0]
        [fnl '()]
        [d1 (extract-digits n1)]
        [d2 (extract-digits n2)]
        [add (lambda (n1 n2)
               (let-values ([(n1 n2) (ladd/handle-nums-primitive n1 n2)])
                 (map max n1 n2)))])
    (for ([n (reverse d2)])
      (set! fnl (add fnl
                     (lmult/mult-single (lmult/append-zeros cnt d1) n)))
      (set! cnt (+ cnt 1)))
    (list->num fnl)))
