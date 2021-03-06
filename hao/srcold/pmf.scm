;; This is an attempt to implement an exact odd ratio test with
;; a quartic root solver. Scheme was chosen because of its native
;; support of rational numbers. But this piece of code contains
;; a bug (probably in the quartic root solver) so the output 
;; confidence interval is ridiculous.
 
(define (choose n k)
  (define (fac n)
    (define (*fac n acc)
      (if (zero? n)
	  acc
	  (*fac (- n 1) (* acc n))))
    (*fac n 1))
  (/ (fac n) (* (fac k) (fac (- n k)))))

(define (seq low up)
  (define (*seq low up res)
    (if (> low up)
	res
	(*seq low (- up 1) (cons up res))))
  (*seq low up (list)))


(define (pmf-denom x1 x2 n1 n2 z)
  (* (choose n1 x1) (choose n2 x2) (expt z x1)))
(define (norm-const m n1 n2 z up ell accum)
  (if (> ell up)
      accum
      (norm-const m n1 n2 z up (+ ell 1) (+ accum (pmf-denom ell (- m ell) n1 n2 z)))))
(define (pmf x1 x2 n1 n2 z)
  (let ((m (+ x1 x2)))
    (/ (pmf-denom x1 x2 n1 n2 z) (norm-const m n1 n2 z (min m n1) (max 0 (- m n2)) 0))))


(define (cdf x1 x2 n1 n2 z)
  (let* ((m (+ x1 x2))
	 (C (norm-const m n1 n2 z (min m n1) (max 0 (- m n2)) 0)))
    (/ (apply + (map (lambda (j)
			(pmf-denom j (- m j) n1 n2 z))
		      (seq (max 0 (- m n2)) x1)))
       C)))
(define (cdf-fast x1 x2 n1 n2 z)
  (let* ((m (+ x1 x2))
	 (d (norm-const m n1 n2 z x1         (max 0 (- m n2)) 0))
	 (C (norm-const m n1 n2 z (min m n1) (+ x1 1) d)))
    (/ d C)))

(define (printdist)
  (define (*printdist j up)
    (if (> j up)
	'nil
	(begin
	  (let ((x (pmf j 5 8 20 48/50)))
	    (print (exact->inexact x)) (newline))
	  (*printdist (+ j 1) up))))
  (*printdist 0 8))

(define div /)
(define (/ x y)
  (print x)     (print "  /  ")	(print y)    (print "  ==  ")    (print (div x y))    (newline)
  (div x y))
	

;; Previos value = 315/82 
;; (define alpha-1 5.02388618731488)
;; (define alpha-2 0.000982069117175256)
(define alpha-1 5.02388618731488)
(define alpha-2 5.02388618731488)

(define (var-quartic-1 y x1 x2 n1 n2)
  (let* ((m (+ x1 x2))
	 (Q (- (* (expt (- y x1 1/2) 2)
		 (+ (/ 1 y)
		    (/ 1 (- n1 y))
		    (/ 1 (- m y))
		    (/ 1 (+ (- n2 m) y))))
	      alpha-1)))
    (* Q (* 4 y (- y m) (- y n1) (+ y n2 (- m))))))
(define (var-quartic-2 y x1 x2 n1 n2)
  (let* ((m (+ x1 x2))
	 (Q (- (* (expt (- y x1 -1/2) 2)
		 (+ (/ 1 y)
		    (/ 1 (- n1 y))
		    (/ 1 (- m y))
		    (/ 1 (+ (- n2 m) y))))
	      alpha-2)))
    (* Q (* 4 y (- y m) (- y n1) (+ y n2 (- m))))))

(define (var-quartic-coef-1 x1 x2 n1 n2 degree)
  (let ((m (+ x1 x2)))
    (cond ((eqv? degree 0)      (+ (- (* (expt m 2) n1))
				   (* m n1 n2)
				   (* -4 (expt m 2) n1 x1)
				   (* 4 m n1 n2 x1)
				   (* -4 (expt m 2) n1 (expt x1 2))
				   (* 4 m n1 n2 (expt x1 2))))
	  ((eqv? degree 1)      (+ (* 2 m n1)
				   (* 4 (expt m 2) n1)
				   (* 4 alpha-1 (expt m 2) n1)
				   (* -4 m n1 n2)
				   (* -4 alpha-1 m n1 n2)
				   (* 8 m n1 x1)
				   (* 8 (expt m 2) n1 x1)
				   (* -8 m n1 n2 x1)
				   (* 8 m n1 (expt x1 2))))
	  ((eqv? degree 2)      (+ (* -4 alpha-1 (expt m 2))
				   (- n1)
				   (* -8 m n1)
				   (* -8 alpha-1 m n1)
				   (* -4 (expt m 2) n1)
				   (- n2)
				   (* 4 alpha-1 m n2)
				   (* 4 alpha-1 n1 n2)
				   (* 4 m n1 n2)
				   (* -4 n1 x1)
				   (* -16 m n1 x1)
				   (* -4 n2 x1)
				   (* -4 n1 (expt x1 2))
				   (* -4 n2 (expt x1 2))))
	  ((eqv? degree 3)      (+ (* 8 alpha-1 m)
				   (* 4 n1)
				   (* 4 alpha-1 n1)
				   (* 8 m n1)
				   (* 4 n2)
				   (* -4 alpha-1 n2)
				   (* 8 n1 x1)
				   (* 8 n2 x1)))
	  ((eqv? degree 4)      (+ (* -4 alpha-1)
				   (* -4 n1)
				   (* -4 n2)))) ))
(define (var-quartic-coef-2 x1 x2 n1 n2 degree)
  (let ((m (+ x1 x2)))
    (cond ((eqv? degree 0)      (+ (- (* (expt m 2) n1))
				   (* m n1 n2)
				   (* 4 (expt m 2) n1 x1)
				   (* -4 m n1 n2 x1)
				   (* -4 (expt m 2) n1 (expt x1 2))
				   (* 4 m n1 n2 (expt x1 2))))
	  ((eqv? degree 1)      (+ (* 2 m n1)
				   (* -4 (expt m 2) n1)
				   (* 4 alpha-2 (expt m 2) n1)
				   (* 4 m n1 n2)
				   (* -4 alpha-2 m n1 n2)
				   (* -8 m n1 x1)
				   (* 8 (expt m 2) n1 x1)
				   (* -8 m n1 n2 x1)
				   (* 8 m n1 (expt x1 2))))
	  ((eqv? degree 2)      (+ (* -4 alpha-2 (expt m 2))
				   (- n1)
				   (* 8 m n1)
				   (* -8 alpha-2 m n1)
				   (* -4 (expt m 2) n1)
				   (- n2)
				   (* 4 alpha-2 m n2)
				   (* 4 alpha-2 n1 n2)
				   (* 4 m n1 n2)
				   (* 4 n1 x1)
				   (* -16 m n1 x1)
				   (* 4 n2 x1)
				   (* -4 n1 (expt x1 2))
				   (* -4 n2 (expt x1 2))))
	  ((eqv? degree 3)      (+ (* 8 alpha-2 m)
				   (* -4 n1)
				   (* 4 alpha-2 n1)
				   (* 8 m n1)
				   (* -4 n2)
				   (* -4 alpha-2 n2)
				   (* 8 n1 x1)
				   (* 8 n2 x1)))
	  ((eqv? degree 4)      (+ (* -4 alpha-2)
				   (* -4 n1)
				   (* -4 n2))))))

;; Derivative
(define (var-quartic-deriv fn y x1 x2 n1 n2)
  (let ((r1 (fn x1 x2 n1 n2 1))
	(r2 (fn x1 x2 n1 n2 2))
	(r3 (fn x1 x2 n1 n2 3))
	(r4 (fn x1 x2 n1 n2 4)))
    (+ r1 (* 2 r2 y) (* 3 r3 (expt y 2)) (* 4 r4 (expt y 3)))))
(define (var-quartic-deriv-1 y x1 x2 n1 n2)  (var-quartic-deriv var-quartic-coef-1 y x1 x2 n1 n2))
(define (var-quartic-deriv-2 y x1 x2 n1 n2)  (var-quartic-deriv var-quartic-coef-2 y x1 x2 n1 n2))  

;; Check if the coefficients are correct
;; (+ (* (var-quartic-coef-1 39 1283 50 3428 0) 1)
;;    (* (var-quartic-coef-1 39 1283 50 3428 1) 4)
;;    (* (var-quartic-coef-1 39 1283 50 3428 2) 16)
;;    (* (var-quartic-coef-1 39 1283 50 3428 3) 64)
;;    (* (var-quartic-coef-1 39 1283 50 3428 4) 256))
;; (var-quartic-1 4 39 1283 50 3428)
;; (+ (* (var-quartic-coef-2 39 1283 50 3428 0) 1)
;;    (* (var-quartic-coef-2 39 1283 50 3428 1) 4)
;;    (* (var-quartic-coef-2 39 1283 50 3428 2) 16)
;;    (* (var-quartic-coef-2 39 1283 50 3428 3) 64)
;;    (* (var-quartic-coef-2 39 1283 50 3428 4) 256))
;; (var-quartic-2 4 39 1283 50 3428)

(define (aberth-4-init a1 r0)
  (let ((centre (/ a1 4)))
    (list (+ centre r0)
	  (+ centre (* r0 (make-rectangular (cos (* 3.14159 0.27)) (sin (* 3.14159 0.27)))))
	  (+ centre (* r0 (make-rectangular (cos (* 3.14159 0.49)) (sin (* 3.14159 0.49)))))
	  (+ centre (* r0 (make-rectangular (cos (* 3.14159 0.71)) (sin (* 3.14159 0.71))))))))

(define (max-root-mod fn x1 x2 n1 n2)
  (let ((r0 (fn x1 x2 n1 n2 0))
	(r1 (fn x1 x2 n1 n2 1))
	(r2 (fn x1 x2 n1 n2 2))
	(r3 (fn x1 x2 n1 n2 3))
	(r4 (fn x1 x2 n1 n2 4)))
    (+ 1 (max (abs (/ r0 r4))
	      (abs (/ r1 r4))
	      (abs (/ r2 r4))
	      (abs (/ r3 r4))))))
(define (max-root-mod-1 x1 x2 n1 n2) (max-root-mod var-quartic-coef-1 x1 x2 n1 n2))
(define (max-root-mod-2 x1 x2 n1 n2) (max-root-mod var-quartic-coef-2 x1 x2 n1 n2))
(define (nth list n)
  (if (= n 1)
      (car list)
      (nth (cdr list) (- n 1))))

(define (aberth-4 p dp init)
  (define (nxt-approx z)
    (map (lambda (k)
	   (let ((zk (nth z k)))
	     (let ((p/dp (/ (p zk) (dp zk)))
		   (S (fold (lambda (j A)
				(if (eqv? j k) A (+ A (/ 1 (- zk (nth z j))))))
			    0 (seq 1 4))))
	       (- zk (/ p/dp (- 1 (* p/dp S)))))))
	 (seq 1 4)))
  (define (main-loop z n)
    (if (eqv? n 0)
	z
	(let ((z-new (nxt-approx z)))
	  ;; (map (lambda (x)
	  ;; 	 (print (exact->inexact x)) (print "  ")) z-new)
	  ;; (newline)
	  ;; (map (lambda (x)
	  ;; 	 (print (exact->inexact (p x))) (print "  ")) z-new)
	  ;; (newline)
	  (main-loop z-new (- n 1)))))
  (let ((res (main-loop init 100)))
    (print "--------\n")
    (map (lambda (x) (print (exact->inexact x))      (print "  ")) res)    (newline)
    (map (lambda (x) (print (exact->inexact (p  x))) (print "  ")) res)    (newline)
    (map (lambda (x) (print (exact->inexact (dp x))) (print "  ")) res)    (newline)
    (print "********\n")
    res))

(define (extreme-realroot compare roots)
  (define (*find roots cur)
    (if (null? roots)
	cur
	(if (and (< (imag-part (car roots)) 1e-6)
		 (compare (real-part (car roots)) cur))
	    (*find (cdr roots) (real-part (car roots)))
	    (*find (cdr roots) cur))))
  (*find roots (if (or (eq? compare <) (eq? compare <=)) +inf.0 -inf.0)))


(define (confint-xmode x1 x2 n1 n2)
  (let ((xe-big (extreme-realroot > (aberth-4 (lambda (y) (var-quartic-1 y x1 x2 n1 n2))
					      (lambda (y) (var-quartic-deriv-1 y x1 x2 n1 n2))
					      (aberth-4-init (var-quartic-coef-1 x1 x2 n1 n2 3)
							     (max-root-mod-1 x1 x2 n1 n2)))))
	(xe-small (extreme-realroot < (aberth-4 (lambda (y) (var-quartic-2 y x1 x2 n1 n2))
						(lambda (y) (var-quartic-deriv-2 y x1 x2 n1 n2))
						(aberth-4-init (var-quartic-coef-2 x1 x2 n1 n2 3)
							       (max-root-mod-2 x1 x2 n1 n2))))))
    (list xe-small xe-big)))
(define (xmode-to-z x x1 x2 n1 n2)
  (let ((m (+ x1 x2)))
    (/ (* x (+ (- m) x n2)) (* (- x n1) (- x m)))))
(define (confint-z x1 x2 n1 n2)
  (map (lambda (y) (xmode-to-z y x1 x2 n1 n2)) (confint-xmode x1 x2 n1 n2)))

(let* ((x1 28) (n1 50)
      (x2 48) (n2 78)
      (res (confint-xmode x1 x2 n1 n2)))
  (print (var-quartic-2 (car res) x1 x2 n1 n2))   (print "  ")
  (print (var-quartic-1 (cadr res) x1 x2 n1 n2))  (newline)
  (print (car res))                               (print "  ")
  (print (cadr res))                              (newline))


(let* ((x1 28) (n1 50)
       (x2 48) (n2 78))
  (print 604.8553240558413) (print "   ") (print (var-quartic-1 604.8553240558413 x1 x2 n1 n2)) (newline)
  (print 603.23) (print "   ") (print (var-quartic-1 603.23 x1 x2 n1 n2)) (newline)
  (print 400) (print "   ") (print (var-quartic-1 400 x1 x2 n1 n2)) (newline))

;; (printdist)

;;(define (cdf-approx x1 x2 n1 n2 z)
  
