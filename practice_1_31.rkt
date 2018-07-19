;;递归计算过程
(define (product term a next b) 
    (if (> a b) 
        1
        (* (term a) (product term (next a) next b))))


;;迭代计算过程
(define (product term a next b) 
    (define (iter a result) 
        (if (> a b) 
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))


;;阶乘
(define (factorial n) 
    (product (lambda (x) x) 
             1
             (lambda (x) (+ x 1))
             n))


;;pi/4
(define (pi-product n) 
    (define (term x) 
        (if (even? x)
        (/ (+ x 2) (+ x 1))
        (/ (+ x 1) (+ x 2))))
    (define (next x) (+ x 1))
    (* 1.0 (product term 1 next n)))
    
    
;;结果
;> (* 4 (pi-product 10000))
;3.1417497057380523
;> (* 4 (pi-product 50000))
;3.141624068416808
;> 
    
