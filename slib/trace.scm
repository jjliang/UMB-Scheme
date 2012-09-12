;;;; "trace.scm" Utility macros for tracing in Scheme.
;;; Copyright (C) 1991, 1992, 1993, 1994 Aubrey Jaffer.
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

(require 'debug)			;for the qp printer.
(define debug:indent 0)

(define debug:tracef
  (let ((null? null?)			;These bindings are so that
	(not not)			;tracef will not trace parts
	(car car) (cdr cdr)		;of itself.
	(eq? eq?) (+ +) (zero? zero?) (modulo modulo)
	(apply apply) (display display) (qpn debug:qpn))
    (lambda (function . optname)
      (set! debug:indent 0)
      (let ((name (if (null? optname) function (car optname))))
	(lambda args
	  (cond ((and (not (null? args))
		      (eq? (car args) 'debug:untrace-object)
		      (null? (cdr args)))
		 function)
		(else
		 (do ((i debug:indent (+ -1 i))) ((zero? i)) (display #\ ))
		 (apply qpn "CALLED" name args)
		 (set! debug:indent (modulo (+ 1 debug:indent) 8))
		 (let ((ans (apply function args)))
		   (set! debug:indent (modulo (+ -1 debug:indent) 8))
		   (do ((i debug:indent (+ -1 i))) ((zero? i)) (display #\ ))
		   (qpn "RETURNED" name ans)
		   ans))))))))

;;; the reason I use a symbol for debug:untrace-object is so
;;; that functions can still be untraced if this file is read in twice.

(define (debug:untracef function)
  (set! debug:indent 0)
  (function 'debug:untrace-object))

;;;;The trace: functions wrap around the debug: functions to provide
;;; niceties like keeping track of traced functions and dealing with
;;; redefinition.

(require 'alist)
(define trace:adder (alist-associator eq?))
(define trace:deler (alist-remover eq?))

(define *traced-procedures* '())
(define (trace:tracef fun sym)
  (cond ((not (procedure? fun))
	 (display "WARNING: not a procedure " (current-error-port))
	 (display sym (current-error-port))
	 (newline (current-error-port))
	 (set! *traced-procedures* (trace:deler *traced-procedures* sym))
	 fun)
	(else
	 (let ((p (assq sym *traced-procedures*)))
	   (cond ((and p (eq? (cdr p) fun))
		  fun)
		 (else
		  (let ((tfun (debug:tracef fun sym)))
		    (set! *traced-procedures*
			  (trace:adder *traced-procedures* sym tfun))
		    tfun)))))))

(define (trace:untracef fun sym)
  (let ((p (assq sym *traced-procedures*)))
    (set! *traced-procedures* (trace:deler *traced-procedures* sym))
    (cond ((not (procedure? fun)) fun)
	  ((not p) fun)
	  ((eq? (cdr p) fun)
	   (debug:untracef fun))
	  (else fun))))

(define tracef debug:tracef)
(define untracef debug:untracef)

;;;; Finally, the macros trace and untrace

(defmacro trace xs
  (if (null? xs)
      `(begin ,@(map (lambda (x) `(set! ,x (trace:tracef ,x ',x)))
		     (map car *traced-procedures*))
	      (map car *traced-procedures*))
      `(begin ,@(map (lambda (x) `(set! ,x (trace:tracef ,x ',x))) xs))))
(defmacro untrace xs
  (if (null? xs)
      (slib:eval
       `(begin ,@(map (lambda (x) `(set! ,x (trace:untracef ,x ',x)))
		      (map car *traced-procedures*))
	       '',(map car *traced-procedures*)))
      `(begin ,@(map (lambda (x) `(set! ,x (trace:untracef ,x ',x))) xs))))
