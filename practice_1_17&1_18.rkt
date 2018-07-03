;O(n)
(define (* a b) 
    (if (= b 0) 
        0
        (+ a (* a (- b 1)))))

;;假定的过程:double 求出一个数的两倍、halve 求一个偶数的一半

;O(log n)的递归计算过程
(define (fast-* a b)
    (cond ((= b 0) 0)
          ((even? b) (fast-* (double a) (halve b))) 
          (else (+ a (fast-* a (- b 1))))))

;O(log n)的迭代计算过程
(define (fast-* a b) 
    (fast-*-iter a b 0))

(define (fast-*-iter a b result) 
    (cond ((= b 0) result)
          ((even? b) (fast-*-iter (double a) (halve b) result))
          (else (fast-*-iter a (- b 1) (+ a result)))))
