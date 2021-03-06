;;练习1.12

(define (pasika n) 
    (define (pasika-n-list n)  ;求出第n层的数据返回一个表

        (define (no. llist k)  ;(no. list x)求值出表list的第x个元素 
            (cond ((null? llist) nil) 
                  ((= k 1) (car llist)) 
                  (else (no. (cdr llist) (- k 1)))))
    
        (define (nlist-iter a before-list)   ;返回第n层的第a个元素
            (if (or (= a 1) (= a n)) 
                1
                (+ (no. before-list (- a 1))
                (no. before-list a))))

        (define (pasika-n-list-iter s before-list)  
            (cond ((= s 0) nil)
                  (else (cons (nlist-iter s before-list)
                            (pasika-n-list-iter (- s 1) before-list)))))

            (cond ((= n 1) (list 1))
                  ((= n 2) (list 1 1))
                  (else (pasika-n-list-iter n (pasika-n-list (- n 1))))))


        
    (define (pasika-list a b)       ;返回一个包含第a层到第b层表的表
        (if (or (< a b) (= a b)) 
            (cons (pasika-n-list a) (pasika-list (+ 1 a) b))
            nil))
            
    

        (pasika-list 1 n))


