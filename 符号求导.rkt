(variable? a)  
(same-variable? v1 v2) 
(sum? e)  
(addend e)  
(augend e)  
(make-sum v1 v2)  
(product? e)  
(multiplier e)  
(multiplicand e) 
(make-product v1 v2)
(exponentiation? e) 
(base e)
(exponent e)
(make-exponentiation b e)  

(define (deriv exp var) 
    (cond ((number? exp) 0)
          ((variable? exp)
           (if (same-variable? exp var) 1 0))
          ((sum? exp) (make-sum (deriv (addend exp) var)
                                (deriv (augend exp) var)))
          ((product? exp) (make-sum (make-product (multiplier exp)
                                                  (deriv (multiplicand exp) var))
                                    (make-product (multiplicand exp) 
                                                  (deriv (multiplier exp) var))))
          ((exponentiation? exp) (make-product (deriv (base exp) var) 
                                               (make-product (exponent exp) 
                                                             (make-exponentiation (base exp) 
                                                                                  (- (exponent exp) 1)))))
          (else 
            (error "unknown expression type -- DERIV" exp))))
    
(define (variable? x) 
  (symbol? x))

(define (same-variable? v1 v2) 
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? e n) 
  (and (number? e) (= e n)))

(define (make-sum v1 v2) 
  (cond ((=number? v1 0) v2)
        ((=number? v2 0) v1)
        ((and (number? v1) (number? v2)) (+ v1 v2))
        ((list? v2) (append (list '+ v1) v2))
        (else (list '+ v1 v2))))

(define (sum? e) 
  (and (pair? e) (eq? (car e) '+)))

(define (addend e) (cadr e))

(define (augend e) (if (= (cdddr e) '()) 
                       (caddr e)
                       (append (list '+) (cddr e))))

(define (make-product v1 v2) 
  (cond ((or (=number? v1 0) (=number? v2 0)) 0)
        ((=number? v1 1) v2)
        ((=number? v2 1) v1)
        ((and (number? v1) (number? v2)) (* v1 v2))
        ((list? v2) (append (list '* v1) v2))
        (else (list '* v1 v2))))

(define (product? e) 
  (and (pair? e) (eq? (car e) '*)))

(define (multiplier e) (cadr e))

(define (multiplicand e) (if (= (cdddr e) '())
                             (caddr e)
                             (append (list '*) (cddr e))))

(define (make-exponentiation b e) 
  (cond ((number? b) (expt b e))
        ((= e 0) 1) 
        ((= e 1) b) 
        (else (list '** b e))))
  
(define (exponentiation? e) 
  (and (pair? e) (eq? (car e) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))
