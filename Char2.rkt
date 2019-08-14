#lang sicp
; 2.18, 注意cdr返回的为list，而car返回的为数子，而append的操作对象是两个list
(define (reverse x)
  (if (not (null? x))
      (append (reverse(cdr x)) (list(car x)))
      x))