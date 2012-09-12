;; "primes.scm", test and generate prime numbers.
; Written by Michael H Coffin (mhc@edsdrd.eds.com)
;
; This code is in the public domain.

;Date: Thu, 23 Feb 1995 07:47:49 +0500
;From: mhc@edsdrd.eds.com (Michael H Coffin)
;;
;; Test numbers for primality using Rabin-Miller Monte-Carlo
;; primality test.
;;
;; Public functions:
;;
;;    (primes start count . iter)
;;
;;    (probably-prime? p . iter)
;;
;;
;; Please contact the author if you have problems or suggestions:
;;
;;      Mike Coffin
;;      1196 Whispering Knoll
;;      Rochester Hills, Mi. 48306
;;
;;      mhc@edsdrd.eds.com
;;

(require 'random)

;; The default number of times to perform the Rabin-Miller test.  The
;; probability of a composite number passing the Rabin-Miller test for
;; primality with this many random numbers is at most
;; 1/(4^primes:iterations).  The default yields about 1e-9.
;;
(define primes:iter 15)

;; Is n probably prime?
;;
(define (primes:probably-prime? n . iter)
  (let ((iter (if (null? iter) primes:iter (car iter))))
    (primes:prob-pr? n iter)))


;; Return a list of the first `number' odd probable primes less
;; than `start'.
;;
(define (primes:primes start number . iter)
  (let ((iter (if (null? iter) primes:iter (car iter))))
    (do ((candidate (if (odd? start) start (- start 1))
		    (- candidate 2))
	 (count 0)
	 (result '())
	 )
	((or (< candidate 3) (>= count number)) result)
      (if (primes:prob-pr? candidate iter)
	  (begin
	    (set! count (1+ count))
	    (set! result (cons candidate result)))
	  ))))


;; Is n probably prime?  First we check for divisibility by small
;; primes; if it passes that, and it's less than the maximum small
;; prime squared, we try Rabin-Miller.
;;
(define (primes:prob-pr? n count)
  (and (not (primes:dbsp? n))
       (or (< n (* primes:max-small-prime primes:max-small-prime))
	   (primes:rm-prime? n count))))


;; Is `n' Divisible By a Small Prime?
;;
(define (primes:dbsp? n)
  (let ((limit (min (sqrt n) primes:max-small-prime))
	(divisible #f)
	)
    (do ((i 0 (1+ i)))
	((let* ((divisor (array-ref primes:small-primes i)))
	   (set! divisible (= (modulo n divisor) 0))
	   (or divisible (>= divisor limit)))
	 divisible)
      )))


;; Does `n' pass the R.-M. primality test for `m' random numbers?
;;
(define (primes:rm-prime? n m)
  (do ((i 0 (1+ i))
       (x (+ 2 (random (- n 2)))))
      ((or (= i m) (primes:rm-composite? n x))
       (= i m))))


;; Does `x' prove `n' composite using Rabin-Miller?
;;
(define (primes:rm-composite? n x)
  (let ((f (primes:extract2s (- n 1))))
    (primes:rm-comp? n (cdr f) (car f) x)))


;; Is `n' (where n-1 = 2^k * q) proven composite by `x'?
;;
(define (primes:rm-comp? n q k x)
  (let ((y (primes:expt-mod x q n)))
    (if (= y 1)
	#f
	(let loop ((j 0) (y y))
	  (cond ((= j k) #t)
		((= y (- n 1)) #f)
		((= y 1) #t)
		(else (loop (1+ j) (primes:expt-mod y 2 n)))
		)))))


;; Extract factors of 2; that is, factor x as 2^k * q
;; and return (k . q)
;;
(define (primes:extract2s x)
  (do ((k 0 (1+ k))
       (q x (quotient q 2)))
      ((odd? q) (cons k q))
    ))


;; Raise `base' to the power `exp' modulo `modulus' Could use the
;; modulo package, but we only need this function (and besides, this
;; implementation is quite a bit faster).
;;
(define (primes:expt-mod base exp modulus)
  (do ((y 1)
       (k exp (quotient k 2))
       (z base (modulo (* z z) modulus)))
      ((= k 0) y)
    (if (odd? k)
	(set! y (modulo (* y z) modulus)))
    ))

;; This table seems big enough so that making it larger really
;; doesn't have much effect.
;;
(define primes:max-small-prime 997)

(define primes:small-primes
  #(  2   3   5   7  11  13  17  19  23  29
     31  37  41  43  47  53  59  61  67  71
     73  79  83  89  97 101 103 107 109 113
    127 131 137 139 149 151 157 163 167 173
    179 181 191 193 197 199 211 223 227 229
    233 239 241 251 257 263 269 271 277 281
    283 293 307 311 313 317 331 337 347 349
    353 359 367 373 379 383 389 397 401 409
    419 421 431 433 439 443 449 457 461 463
    467 479 487 491 499 503 509 521 523 541
    547 557 563 569 571 577 587 593 599 601
    607 613 617 619 631 641 643 647 653 659
    661 673 677 683 691 701 709 719 727 733
    739 743 751 757 761 769 773 787 797 809
    811 821 823 827 829 839 853 857 859 863
    877 881 883 887 907 911 919 929 937 941
    947 953 967 971 977 983 991 997 ))

(define primes primes:primes)
(define probably-prime? primes:probably-prime?)

(provide 'primes)
