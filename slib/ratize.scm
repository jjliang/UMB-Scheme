;;;; "ratize.scm" Convert number to rational number

(define (rational:simplest x y)
  (define (sr x y) (let ((fx (floor x)) (fy (floor y)))
		    (cond ((not (< fx x)) fx)
			  ((= fx fy) (+ fx (/ (sr (/ (- y fy)) (/ (- x fx))))))
			  (else (+ 1 fx)))))
  (cond ((< y x) (rational:simplest y x))
	((not (< x y)) (if (rational? x) x (slib:error)))
	((positive? x) (sr x y))
	((negative? y) (- (sr (- y) (- x))))
	(else (if (and (exact? x) (exact? y)) 0 0.0))))
(define (rationalize x e) (rational:simplest (- x e) (+ x e)))
