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

;2.30
(define (square-tree1 x)
  (cond ((null? x) x)
        ((pair? x) (cons (square-tree1 (car x)) (square-tree1 (cdr x))))
        ((square x))))

(define (square-tree2 x)
  (map (lambda(x)
         (if (pair? x)
             (square-tree2 x)
             (square x))) x))

;2.31 因为之前定义过tree-map了所以改成了new-tree-map
(define (new-tree-map proc tree)
  (map (lambda(x)
         (if (pair? x)
             (new-tree-map proc x)
             (proc x))) tree))
(define (square-tree tree)
  (new-tree-map square tree))

;2.32  把(car s）也定义一下就不用每次都调用了
(define (subsets s)
  (if (null? s)
      (list s)
      (let ((rest (subsets (cdr s))))
      (append rest
              (map (lambda (x) (cons (car s) x)) rest)))))

;2.33
(define (map1 p sequence)
  (if (null? sequence)
      sequence
      (cons (p (car sequence)) (map1 p (cdr sequence)))))

(define (append1 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-term) (+ (* this-coeff x) higher-term))
              0
              coefficient-sequence))

;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
             nil
             (cons (accumulate op init (map car seqs))
                   (accumulate-n op init (map cdr seqs)))))