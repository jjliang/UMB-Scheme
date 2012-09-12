; "fluidlet.scm", FLUID-LET for Scheme
; Copyright (c) 1992, Dorai Sitaram (dorai@cs.rice.edu)
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

(require 'rev4-optional-procedures)
(require 'common-list-functions)
(require 'dynamic-wind)
(require 'macro)

(define list-set! (lambda (s i v) (set-car! (list-tail s i) v)))

(define-syntax fluid-let
  (syntax-rules ()
    ((fluid-let ((x v) ...) . body)
     (let ((%x-names (list 'x ...))
	   (%x-values (list x ...))
	   (%fluid-x-values (list v ...)))
       (dynamic-wind
	 (lambda ()
           (set! x (list-ref %fluid-x-values
		       (comlist:position 'x %x-names)))
           ...)
	 (lambda () . body)
	 (lambda ()
           (let ((%x-position (comlist:position 'x %x-names)))
             (list-set! %fluid-x-values %x-position x)
	     (set! x (list-ref %x-values %x-position)))
	   ...))))))

;--- end of file
