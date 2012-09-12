;;"yasyn.scm" YASOS in terms of "object.scm"
;;;From: whumeniu@datap.ca (Wade Humeniuk)

(require 'object)

(define yasos:instance?     object?)
;; Removed (define yasos:make-instance 'bogus)  ;;
;; Removed (define-syntax YASOS:INSTANCE-DISPATCHER  ;; alias so compiler can inline for speed
;;   (syntax-rules () ((yasos:instance-dispatcher inst) (cdr inst))))
;; DEFINE-OPERATION

(define-syntax DEFINE-OPERATION
  (syntax-rules ()
    ((define-operation (<name> <inst> <arg> ...) <exp1> <exp2> ...)
     ;;=>
     (define <name> (make-generic-method
		     (lambda (<inst> <arg> ...) <exp1> <exp2> ...))))

    ((define-operation (<name> <inst> <arg> ...) ) ;; no body
     ;;=>
     (define-operation (<name> <inst> <arg> ...)
       (slib:error "Operation not handled"
		   '<name>
		   (format #f (if (yasos:instance? <inst>) "#<INSTANCE>" "~s")
			   <inst>))))))

;; DEFINE-PREDICATE

(define-syntax DEFINE-PREDICATE
  (syntax-rules ()
    ((define-predicate <name>)
     ;;=>
     (define <name> (make-generic-predicate)))))

;; OBJECT

(define-syntax OBJECT
  (syntax-rules ()
    ((object ((<name> <self> <arg> ...) <exp1> <exp2> ...) ...)
    ;;=>
     (let ((self (make-object)))
       (make-method! self <name> (lambda (<self> <arg> ...) <exp1> <exp2> ...))
       ...
       self))))

;; OBJECT with MULTIPLE INHERITANCE  {First Found Rule}

(define-syntax OBJECT-WITH-ANCESTORS
  (syntax-rules ()
    ((object-with-ancestors ( (<ancestor1> <init1>) ... ) 
			    ((<name> <self> <arg> ...) <exp1> <exp2> ...) ...)
    ;;=>
     (let* ((<ancestor1> <init1>)
	    ...
	    (self (make-object <ancestor1> ...)))
       (make-method! self <name> (lambda (<self> <arg> ...) <exp1> <exp2> ...))
       ...
       self))))
       
;; OPERATE-AS  {a.k.a. send-to-super}

; used in operations/methods

(define-syntax OPERATE-AS
  (syntax-rules ()
   ((operate-as <component> <op> <composit> <arg> ...) ;; What is <composit> ???
   ;;=>
    ((get-method <component> <op>) <composit> <arg> ...))))



;; SET & SETTER


(define-syntax SET
  (syntax-rules ()
    ((set (<access> <index> ...) <newval>)
     ((yasos:setter <access>) <index> ... <newval>)
    )
    ((set <var> <newval>)
     (set! <var> <newval>)
    )
) )


(define yasos:add-setter	'bogus)
(define yasos:remove-setter-for 'bogus)

(define YASOS:SETTER
  (let ( (known-setters (list (cons car set-car!)
			      (cons cdr set-cdr!)
			      (cons vector-ref vector-set!)
			      (cons string-ref string-set!))
	 )
	 (added-setters '())
       )

    (set! YASOS:ADD-SETTER
      (lambda (getter setter)
	(set! added-setters (cons (cons getter setter) added-setters)))
    )
    (set! YASOS:REMOVE-SETTER-FOR
      (lambda (getter)
	(cond
	  ((null? added-setters)
	   (slib:error "REMOVE-SETTER-FOR: Unknown getter" getter)
	  )
	  ((eq? getter (caar added-setters))
	   (set! added-setters (cdr added-setters))
	  )
	  (else
	    (let loop ((x added-setters) (y (cdr added-setters)))
	      (cond
		((null? y) (slib:error "REMOVE-SETTER-FOR: Unknown getter"
				       getter))
		((eq? getter (caar y)) (set-cdr! x (cdr y)))
		(else (loop (cdr x) (cdr y)))
	  ) ) )
     ) ) )

    (letrec ( (self
		 (lambda (proc-or-operation)
		   (cond ((assq proc-or-operation known-setters) => cdr)
			 ((assq proc-or-operation added-setters) => cdr)
			 (else (proc-or-operation self))) )
	    ) )
      self)
) )



(define (YASOS:MAKE-ACCESS-OPERATION <name>)
  (letrec ( (setter-dispatch
	       (lambda (inst . args)
		   (cond
		     ((and (yasos:instance? inst)
			   (get-method inst setter-dispatch))
		       => (lambda (method) (apply method (cons inst args)))
		     )
		     (else #f)))
	    )
	    (self
	       (lambda (inst . args)
		  (cond
		     ((eq? inst yasos:setter) setter-dispatch) ; for (setter self)
		     ((and (yasos:instance? inst)
			   (get-method inst self))
		      => (lambda (method) (apply method (cons inst args)))
		     )
		     (else (slib:error "Operation not handled" <name> inst))
		)  )
	    )
	  )

	  self
) )

(define-syntax DEFINE-ACCESS-OPERATION
  (syntax-rules ()
    ((define-access-operation <name>)
     ;=>
     (define <name> (yasos:make-access-operation '<name>))
) ) )



;;---------------------
;; general operations
;;---------------------

(define-operation (YASOS:PRINT obj port)
  (format port
	  ;; if an instance does not have a PRINT operation..
	  (if (yasos:instance? obj) "#<INSTANCE>" "~s")
	  obj
) )

(define-operation (YASOS:SIZE obj)
  ;; default behavior
  (cond
    ((vector? obj) (vector-length obj))
    ((list?   obj) (length obj))
    ((pair?   obj) 2)
    ((string? obj) (string-length obj))
    ((char?   obj) 1)
    (else
      (slib:error "Operation not supported: size" obj))
) )

(require 'format)

;;; exports:

(define print yasos:print)		; print also in debug.scm
(define size yasos:size)
(define add-setter yasos:add-setter)
(define remove-setter-for yasos:remove-setter-for)
(define setter yasos:setter)

(provide 'oop)				;in case we were loaded this way.
(provide 'yasos)
