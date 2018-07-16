;;计算平方
(define square
    (lambda (x) (* x x)))
  
(define (smallest-div a)
    (mid a 2))
;;练习1.23
(define (Next n)
  (if (= n 2)
      3
      (+ n 2)))
;;书上的find-divisor
(define (mid a b)
    (cond ((> (square b) a) a)
          ((= (remainder a b) 0) b)
          (else (mid a (Next b)))))
;;O(/n)
(define (prime? n)
    (= n (smallest-div n)))
;;1.22
(define (time-prime-test n) 
    (newline) 
    (display n) 
    (start-prime-test n (runtime)))

(define (start-prime-test n time) 
    (if (prime? n) (dis (- (runtime) time))))

(define (dis time) 
    (display " *** ")
    (display time))


(define (search-for-primes a b) 
    (cond ((even? a) (search-for-primes (+ a 1) b))
          ((> a b) (newline)
                   (display "Done")) 
          (else (time-prime-test a) 
                (search-for-primes (next a) b))))

(define (next x) 
    (+ x 2))




(define (expmod base exp m) 
    (cond ((= exp 0) 1) 
          ((even? exp) (remainder (square (expmod base (/ exp 2) m)) 
                                  m)) 
          (else (remainder (* base (expmod base (- exp 1) m)) 
                           m))))

(define (fermat-test n) 
    (define (try-it a) 
        (= (expmod a n n) a)) 
    (try-it (+ 1 (random (- n 1)))))
;;O(log n)
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))



;;1.28
(define (new-expmod base exp m) 
    (define (not-square-root? n) 
        (and (not (= n 1)) 
             (not (= n (- m 1))) 
             (= (remainder (square n) m) 1)))
    (cond ((= exp 0) 1) 
          ((not-square-root? a) 0)
          ((even? exp) (remainder (square (expmod base (/ exp 2) m)) 
                                  m))
          (else (remainder (* base (new-expmod base (- exp 1) m)) 
                           m))))

(define (Miller-Rabin-fermat-test n) 
    (define (try-it a) 
        (= (expmod a (- n 1) n) 1)) 
    (try-it (+ 1 (random (- n 1)))))


(define (new-fast-prime? n times)
  (cond ((= times 0) true)
        ((Miller-Rabin-fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
