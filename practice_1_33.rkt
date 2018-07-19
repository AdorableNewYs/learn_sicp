(define (filtered-accumulate predicate combiner null-value term a next b) 
    (cond ((> a b) null-value) 
          ((predicate a) (combiner (term a) (filtered-accumulate predicate 
                                                                 combiner
                                                                 null-value
                                                                 term
                                                                 (next a)
                                                                 next
                                                                 b)))
          (else (filtered-accumulate predicate 
                                     combiner
                                     null-value
                                     term
                                     (next a)
                                     next
                                     b))))
                                     
                                     
;;有点丑。。双枪老太婆2333                                     

;;结果2+4+6+8+10
;> (filtered-accumulate even? + 0 (lambda (x) x) 1 (lambda (x) (+ x 1)) 10) 
;30
;> 
