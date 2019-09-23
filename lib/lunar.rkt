#lang racket

(require "util.rkt" "add.rkt" "mult.rkt")

(provide ladd lmult lfactorial lexpt)

(define (ladd n1 n2)
  (let-values ([(d1 d2) (ladd/handle-nums n1 n2)])
    (list->num (map max d1 d2))))

(define (lmult n1 n2)
  (let ([cnt 0]
        [fnl '()]
        [d1 (extract-digits n1)]
        [d2 (extract-digits n2)]
        [add (λ (n1 n2)
               (let-values ([(n1 n2) (ladd/handle-nums-primitive n1 n2)])
                 (map max n1 n2)))])
    (for ([n (reverse d2)])
      (set! fnl (add fnl
                     (lmult/mult-single [lmult/append-zeros cnt d1] n)))
      (set! cnt (+ cnt 1)))
    (list->num fnl)))

(define (lfactorial n)
  (when [between? n 1 9]
    1)
  (let ([x 1])
    (for ([i (in-range 1 (+ n 1))])
      (set! x (lmult x i)))
    x))

(define (lexpt b e)
  (when [between? b 1 9]
    b)
  (let ([p 1])
    (for ([i (in-range 1 (+ e 1))])
      (set! p (lmult p b)))
    p))
