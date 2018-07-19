;;求和的高阶函数
(define (sum term a next b) 
    (if (> a b) 
        0
        (+ (term a) (sum term (next a) next b))))
        

;;求函数f在a到b上的积分
(define (integral f a b dx) 
    (define (add-dx x) (+ x dx))
    (* dx (sum f (+ a (/ dx 2)) add-dx b)))
    
    
;;用辛普森规则求积分    
(define (Simpson-integral f a b n)
    (define h (/ (+ a b) n))
    (define (term x)
        (f (+ a (* x h))))
    (define (high-term y) 
        (if (even? y) 
            (* 2 (term y))
            (* 4 (term y))))
    (define (next z) 
        (+ z 1))
        
    (* (/ h 3)
       (+ (term 0) 
          (term n) 
          (sum high-term 1 next (- n 1)))))
          
          
;;立方函数
(define (cube x) (* x x x))

;;结果
;>  (Simpson-integral cube 0 1 10000)
;1/4
;> (integral cube 0 1 .00001)
;0.24999999998662864
;> 
