#lang sicp
; 2.18, 注意cdr返回的为list，而car返回的为数子，而append的操作对象是两个list
(define (reverse x)
  (if (not (null? x))
      (append (reverse(cdr x)) (list(car x)))
      x))

; 2.19
(define (cc amount coin-values)
  ;定义了三个过程,scheme里面0和1不对应true和false，需要用#t和#f
  (define (no-more? coin-values)
    (null? coin-values))
  (define (except-first-denomination coin-values)
    (cdr coin-values))
  (define (first-denomination coin-values)
    (car coin-values))
   ;原题过程
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+
          (cc amount (except-first-denomination coin-values))
          (cc (- amount (first-denomination coin-values)) coin-values)))))

;2.21
(define (square x)
  (* x x))

(define (square-list1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list2(cdr items)))))

(define (square-list2 items)
  (map square items))

;2.27
(define (deep-reverse x)
  (if (not(pair? x))
      x
      (append (deep-reverse (cdr x)) (list (deep-reverse (car x))))))

;2.28 原来cond的else部分也要加括号QAQ
(define (fringe x)
  (cond ((null? x) x)
        ((not (pair? x)) (list x))
        ((append (fringe (car x)) (fringe (cdr x))))))