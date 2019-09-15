#lang racket

(require "util.rkt")

(provide (all-defined-out))

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
