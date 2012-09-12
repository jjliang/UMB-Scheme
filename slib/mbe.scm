;;;; "mbe.scm" "Macro by Example" (Eugene Kohlbecker, r4rs)
;;; From: Dorai Sitaram, dorai@cs.rice.edu, 1991, revised Sept. 3, 1992,
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;; revised Dec. 6, 1993 to r4rs syntax (if not semantics).
;;; revised Mar. 2 1994 for SLIB (jaffer@ai.mit.edu).

;;; A vanilla implementation of Macro-by-Example (Eugene
;;; Kohlbecker, r4rs).  This file requires defmacro.

(require 'common-list-functions)	;nconc, some, every
;(require 'rev2-procedures)		;append! alternate for nconc
(require 'rev4-optional-procedures)	;list-tail

;;; A vanilla implementation of a hygiene filter for define-syntax

;(define hyg:tag-generic
;  (lambda (e kk tmps) e))

;;; if you don't want the hygiene filter, comment out the following
;;; s-exp and uncomment the previous one.

(define hyg:tag-generic
  (lambda (e kk tmps)
    (if (pair? e)
	(let ((a (car e)))
	  (case a
	    ((quote) `(quote ,(hyg:tag-vanilla (cadr e) kk tmps)))
	    ((if begin)
	     `(,a ,@(map (lambda (e1) (hyg:tag-generic e1 kk tmps))
			 (cdr e))))
	    ((set! define)
	     `(,a ,(hyg:tag-vanilla (cadr e) kk tmps)
		  ,@(map (lambda (e1) (hyg:tag-generic e1 kk tmps))
			 (cddr e))))
	    ((lambda) (hyg:tag-lambda (cdr e) kk tmps))
	    ((letrec) (hyg:tag-letrec (cdr e) kk tmps))
	    ((let) (hyg:tag-let (cdr e) kk tmps))
	    ((let*) (hyg:tag-let-star (cdr e) kk tmps))
	    ((do) (hyg:tag-do (cdr e) kk tmps))
	    ((case)
	     `(case ,(hyg:tag-generic (cadr e) kk tmps)
		,@(map
		   (lambda (cl)
		     `(,(hyg:tag-vanilla (car cl) kk tmps)
		       ,@(map
			  (lambda (e1)
			    (hyg:tag-generic e1 kk tmps))
			  (cdr cl))))
		   (cddr e))))
	    ((cond)
	     `(cond ,@(map
		       (lambda (cl)
			 (map (lambda (e1)
				(hyg:tag-generic e1 kk tmps))
			      cl))
		       (cdr e))))
	    (else (map (lambda (e1)
			 (hyg:tag-generic e1 kk tmps))
		       e))))
	(hyg:tag-vanilla e kk tmps))))

(define hyg:tag-vanilla
  (lambda (e kk tmps)
    (cond ((symbol? e)
	   (cond ((memq e kk) e)
		 ((assq e tmps) => cdr)
		 (else e)))
	  ((pair? e)
	   (cons (hyg:tag-vanilla (car e) kk tmps)
		 (hyg:tag-vanilla (cdr e) kk tmps)))
	  (else e))))

(define hyg:tag-lambda
  (lambda (e kk tmps)
    (let* ((bvv (car e))
	   (tmps2 (append
		   (map (lambda (v) (cons v (gentemp)))
			(hyg:flatten bvv))
		   tmps)))
      `(lambda
	   ,(hyg:tag-vanilla bvv kk tmps2)
	 ,@(map
	    (lambda (e1)
	      (hyg:tag-generic e1 kk tmps2))
	    (cdr e))))))

(define hyg:flatten
  (lambda (e)
    (let loop ((e e) (r '()))
      (cond ((pair? e) (loop (car e)
			     (loop (cdr e) r)))
	    ((null? e) r)
	    (else (cons e r))))))

(define hyg:tag-letrec
  (lambda (e kk tmps)
    (let* ((varvals (car e))
	   (tmps2 (append
		   (map (lambda (v) (cons v (gentemp)))
			(map car varvals))
		   tmps)))
      `(letrec ,(map
		 (lambda (varval)
		   `(,(hyg:tag-vanilla (car varval)
				       kk tmps2)
		     ,(hyg:tag-generic (cadr varval)
				       kk tmps2)))
		 varvals)
	 ,@(map (lambda (e1)
		  (hyg:tag-generic e1 kk tmps2))
		(cdr e))))))

(define hyg:tag-let
  (lambda (e kk tmps)
    (let* ((tt (if (symbol? (car e)) (cons (car e) (gentemp)) '()))
	   (e (if (null? tt) e (cdr e)))
	   (tmps (if (null? tt) tmps (append (list tt) tmps))))
      (let* ((varvals (car e))
	     (tmps2 (append (map (lambda (v) (cons v (gentemp)))
				 (map car varvals))
			    tmps)))
	`(let
	   ,@(if (null? tt) '() `(,(hyg:tag-vanilla (car tt) 
						    kk
						    tmps)))
	   ,(let loop ((varvals varvals)
		       (i (length varvals)))
	      (if (null? varvals) '()
		  (let ((varval (car varvals))
			(tmps3 (list-tail tmps2 i)))
		    (cons `(,(hyg:tag-vanilla (car varval)
					      kk tmps2)
			    ,(hyg:tag-generic (cadr varval)
					      kk tmps3))
			  (loop (cdr varvals) (- i 1))))))
	   ,@(map
	      (lambda (e1)
		(hyg:tag-generic e1 kk tmps2))
	      (cdr e)))))))

(define hyg:tag-do
  (lambda (e kk tmps)
    (let* ((varinistps (car e))
	   (tmps2 (append (map (lambda (v) (cons v (gentemp)))
			       (map car varinistps))
			  tmps)))
      `(do
	   ,(let loop ((varinistps varinistps)
		       (i (length varinistps)))
	      (if (null? varinistps) '()
		  (let ((varinistp (car varinistps))
			(tmps3 (list-tail tmps2 i)))
		    (cons `(,(hyg:tag-vanilla (car varinistp)
					      kk tmps2)
			    ,(hyg:tag-generic (cadr varinistp)
					      kk tmps3)
			    ,@(hyg:tag-generic (cddr varinistp)
					       kk tmps2))
			  (loop (cdr varinistps) (- i 1))))))
	   ,(map (lambda (e1)
		   (hyg:tag-generic e1 kk tmps2)) (cadr e))
	 ,@(map
	    (lambda (e1)
	      (hyg:tag-generic e1 kk tmps2))
	    (cddr e))))))

(define hyg:tag-let-star
  (lambda (e kk tmps)
    (let* ((varvals (car e))
	   (tmps2 (append (reverse (map (lambda (v) (cons v (gentemp)))
					(map car varvals)))
			  tmps)))
      `(let*
	   ,(let loop ((varvals varvals)
		       (i (- (length varvals) 1)))
	      (if (null? varvals) '()
		  (let ((varval (car varvals))
			(tmps3 (list-tail tmps2 i)))
		    (cons `(,(hyg:tag-vanilla (car varval)
					      kk tmps3)
			    ,(hyg:tag-generic (cadr varval)
					      kk (cdr tmps3)))
			  (loop (cdr varvals) (- i 1))))))
	 ,@(map
	    (lambda (e1)
	      (hyg:tag-generic e1 kk tmps2))
	    (cdr e))))))

;;;; End of hygiene filter.

;;; finds the leftmost index of list l where something equal to x
;;; occurs
(define mbe:position
  (lambda (x l)
    (let loop ((l l) (i 0))
      (cond ((not (pair? l)) #f)
	    ((equal? (car l) x) i)
	    (else (loop (cdr l) (+ i 1)))))))

;;; tests if expression e matches pattern p where k is the list of
;;; keywords
(define mbe:matches-pattern?
  (lambda (p e k)
    (cond ((mbe:ellipsis? p)
	   (and (or (null? e) (pair? e))
		(let* ((p-head (car p))
		       (p-tail (cddr p))
		       (e-head=e-tail (mbe:split-at-ellipsis e p-tail)))
		  (and e-head=e-tail
		       (let ((e-head (car e-head=e-tail))
			     (e-tail (cdr e-head=e-tail)))
			 (and (comlist:every
			       (lambda (x) (mbe:matches-pattern? p-head x k))
			       e-head)
			      (mbe:matches-pattern? p-tail e-tail k)))))))
	  ((pair? p)
	   (and (pair? e)
		(mbe:matches-pattern? (car p) (car e) k)
		(mbe:matches-pattern? (cdr p) (cdr e) k)))
	  ((symbol? p) (if (memq p k) (eq? p e) #t))
	  (else (equal? p e)))))

;;; gets the bindings of pattern variables of pattern p for
;;; expression e;
;;; k is the list of keywords
(define mbe:get-bindings
  (lambda (p e k)
    (cond ((mbe:ellipsis? p)
	   (let* ((p-head (car p))
		  (p-tail (cddr p))
		  (e-head=e-tail (mbe:split-at-ellipsis e p-tail))
		  (e-head (car e-head=e-tail))
		  (e-tail (cdr e-head=e-tail)))
	     (cons (cons (mbe:get-ellipsis-nestings p-head k)
		     (map (lambda (x) (mbe:get-bindings p-head x k))
			  e-head))
	       (mbe:get-bindings p-tail e-tail k))))
	  ((pair? p)
	   (append (mbe:get-bindings (car p) (car e) k)
	     (mbe:get-bindings (cdr p) (cdr e) k)))
	  ((symbol? p)
	   (if (memq p k) '() (list (cons p e))))
	  (else '()))))

;;; expands pattern p using environment r;
;;; k is the list of keywords
(define mbe:expand-pattern
  (lambda (p r k)
    (cond ((mbe:ellipsis? p)
	   (append (let* ((p-head (car p))
			  (nestings (mbe:get-ellipsis-nestings p-head k))
			  (rr (mbe:ellipsis-sub-envs nestings r)))
		     (map (lambda (r1)
			    (mbe:expand-pattern p-head (append r1 r) k))
			  rr))
	     (mbe:expand-pattern (cddr p) r k)))
	  ((pair? p)
	   (cons (mbe:expand-pattern (car p) r k)
	     (mbe:expand-pattern (cdr p) r k)))
	  ((symbol? p)
	   (if (memq p k) p
	     (let ((x (assq p r)))
	       (if x (cdr x) p))))
	  (else p))))

;;; returns a list that nests a pattern variable as deeply as it
;;; is ellipsed
(define mbe:get-ellipsis-nestings
  (lambda (p k)
    (let sub ((p p))
      (cond ((mbe:ellipsis? p) (cons (sub (car p)) (sub (cddr p))))
	    ((pair? p) (append (sub (car p)) (sub (cdr p))))
	    ((symbol? p) (if (memq p k) '() (list p)))
	    (else '())))))

;;; finds the subenvironments in r corresponding to the ellipsed
;;; variables in nestings
(define mbe:ellipsis-sub-envs
  (lambda (nestings r)
    (comlist:some (lambda (c)
		    (if (mbe:contained-in? nestings (car c)) (cdr c) #f))
		  r)))

;;; checks if nestings v and y have an intersection
(define mbe:contained-in?
  (lambda (v y)
    (if (or (symbol? v) (symbol? y)) (eq? v y)
	(comlist:some (lambda (v_i)
			(comlist:some (lambda (y_j)
					(mbe:contained-in? v_i y_j))
				      y))
		      v))))

;;; split expression e so that its second half matches with
;;; pattern p-tail
(define mbe:split-at-ellipsis
  (lambda (e p-tail)
    (if (null? p-tail) (cons e '())
      (let ((i (mbe:position (car p-tail) e)))
	(if i (cons (butlast e (- (length e) i))
		    (list-tail e i))
	    (slib:error 'mbe:split-at-ellipsis 'bad-arg))))))

;;; tests if x is an ellipsing pattern, i.e., of the form
;;; (blah ... . blah2)
(define mbe:ellipsis?
  (lambda (x)
    (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '...))))

;define-syntax

(defmacro define-syntax (macro-name syn-rules)
  (if (or (not (pair? syn-rules))
	  (not (eq? (car syn-rules) 'syntax-rules)))
      (slib:error 'define-syntax 'not-an-r4rs-high-level-macro
	     macro-name syn-rules)
      (let ((keywords (cons macro-name (cadr syn-rules)))
	    (clauses (cddr syn-rules)))
	`(defmacro ,macro-name macro-arg
	   (let ((macro-arg (cons ',macro-name macro-arg))
		 (keywords ',keywords))
	     (cond ,@(map
		      (lambda (clause)
			(let ((in-pattern (car clause))
			      (out-pattern (cadr clause)))
			  `((mbe:matches-pattern? ',in-pattern macro-arg
						  keywords)
			    (hyg:tag-generic
			     (mbe:expand-pattern
			      ',out-pattern
			      (mbe:get-bindings ',in-pattern macro-arg
						keywords)
			      keywords)
			     (nconc
			      (hyg:flatten ',in-pattern)
			      keywords)
			     '()))))
		      clauses)
		   (else (slib:error ',macro-name 'no-matching-clause
				',clauses))))))))
;eof
