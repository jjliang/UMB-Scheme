;;;; Implementation of VICINITY and MODULES for Scheme
;Copyright (C) 1991, 1992, 1993, 1994 Aubrey Jaffer
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

(define *SLIB-VERSION* "2a3")

;;; Standardize msdos -> ms-dos.
(define software-type
  (cond ((eq? 'msdos (software-type))
	 (lambda () 'ms-dos))
	(else software-type)))

(define (user-vicinity)
  (case (software-type)
    ((VMS)	"[.]")
    (else	"")))

(define program-vicinity
  (let ((*vicinity-suffix*
	 (case (software-type)
	   ((NOSVE)	'(#\: #\.))
	   ((AMIGA)	'(#\: #\/))
	   ((UNIX)	'(#\/))
	   ((VMS)	'(#\: #\]))
	   ((MS-DOS WINDOWS ATARIST OS/2)	'(#\\))
	   ((MACOS THINKC)	'(#\:)))))
    (lambda ()
      (let loop ((i (- (string-length *load-pathname*) 1)))
	(cond ((negative? i) "")
	      ((memv (string-ref *load-pathname* i)
		     *vicinity-suffix*)
	       (substring *load-pathname* 0 (+ i 1)))
	      (else (loop (- i 1))))))))

(define sub-vicinity
  (case (software-type)
    ((VMS)
     (lambda
      (vic name)
      (let ((l (string-length vic)))
	(if (or (zero? (string-length vic))
		(not (char=? #\] (string-ref vic (- l 1)))))
	    (string-append vic "[" name "]")
	    (string-append (substring vic 0 (- l 1))
			   "." name "]")))))
    (else
     (let ((*vicinity-suffix*
	    (case (software-type)
	      ((NOSVE) ".")
	      ((UNIX AMIGA) "/")
	      ((MACOS THINKC) ":")
	      ((MS-DOS WINDOWS ATARIST OS/2) "\\"))))
       (lambda (vic name)
	 (string-append vic name *vicinity-suffix*))))))

(define (make-vicinity <pathname>) <pathname>)

(define *catalog*
  (map
   (lambda (p)
     (if (symbol? (cdr p)) p
	 (cons
	  (car p)
	  (if (pair? (cdr p))
	      (cons 
	       (cadr p)
	       (in-vicinity (library-vicinity) (cddr p)))
	      (in-vicinity (library-vicinity) (cdr p))))))
   '(
     (rev4-optional-procedures	.	"sc4opt")
     (rev2-procedures		.	"sc2")
     (multiarg/and-		.	"mularg")
     (multiarg-apply		.	"mulapply")
     (rationalize		.	"ratize")
     (transcript		.	"trnscrpt")
     (with-file			.	"withfile")
     (dynamic-wind		.	"dynwind")
     (dynamic			.	"dynamic")
     (fluid-let		macro	.	"fluidlet")
     (alist			.	"alist")
     (hash			.	"hash")
     (sierpinski		.	"sierpinski")
     (soundex			.	"soundex")
     (hash-table		.	"hashtab")
     (logical			.	"logical")
     (random			.	"random")
     (random-inexact		.	"randinex")
     (modular			.	"modular")
     (primes			.	"primes")
     (factor			.	"factor")
     (charplot			.	"charplot")
     (sort			.	"sort")
     (common-list-functions	.	"comlist")
     (tree			.	"tree")
     (format			.	"format")
     (format-inexact		.	"formatfl")
     (generic-write		.	"genwrite")
     (pretty-print		.	"pp")
     (pprint-file		.	"ppfile")
     (object->string		.	"obj2str")
     (string-case		.	"strcase")
     (stdio			.	"stdio")
     (scanf			.	"scanf")
     (line-i/o			.	"lineio")
     (string-port		.	"strport")
     (getopt			.	"getopt")
     (debug			.	"debug")
     (trace	defmacro	.	"trace")
;     (eval			.	"eval")
     (record			.	"record")
     (promise			.	"promise")
     (synchk			.	"synchk")
     (defmacroexpand		.	"defmacex")
     (macro-by-example	defmacro	.	"mbe")
     (syntax-case		.	"scainit")
     (syntactic-closures	.	"scmacro")
     (macros-that-work		.	"macwork")
     (macro			.	macros-that-work)
     (object			.	"object")
     (record-object		.	"recobj")
     (yasos		macro	.	"yasyn")
     (oop			.	yasos)
     (collect		macro	.	"collect")
     (struct	defmacro	.	"struct")
     (structure	syntax-case	.	"structure")
     (values			.	"values")
     (queue			.	"queue")
     (priority-queue		.	"priorque")
     (array			.	"array")
     (array-for-each		.	"arraymap")
     (repl			.	"repl")
     (process			.	"process")
     (test			.	"test")
     (red-black-tree		.	"rbtree")
     (chapter-order		.	"chap")
     (posix-time		.	"time")
     (common-lisp-time		.	"cltime")
     (relational-database	.	"rdms")
     (database-utilities	.	"dbutil")
     (alist-table		.	"alistab")
     (parameters		.	"paramlst")
     (read-command		.	"comparse")
     (batch			.	"batch")
     )))

(set! *catalog*
      (append (list
	       (cons 'schelog
		     (in-vicinity (sub-vicinity (library-vicinity) "schelog")
				  "schelog"))
	       (cons 'portable-scheme-debugger
		     (in-vicinity (sub-vicinity (library-vicinity) "psd")
				  "psd-slib")))
	      *catalog*))

;(define *load-pathname* #f) ;;; bill
(define *load-pathname* "")

(define (slib:pathnameize-load *old-load*)
  (lambda (<pathname> . extra)
    (let ((old-load-pathname *load-pathname*))
      (set! *load-pathname* <pathname>)
      (apply *old-load* (cons <pathname> extra))
      (require:provide <pathname>)
      (set! *load-pathname* old-load-pathname))))

(set! slib:load-source
      (slib:pathnameize-load slib:load-source))
(set! slib:load
      (slib:pathnameize-load slib:load))

;;;; MODULES

(define *modules* '())

(define (require:provided? feature)
  (if (symbol? feature)
      (if (memq feature *features*) #t
	  (let ((path (cdr (or (assq feature *catalog*) '(#f . #f)))))
	    (and path (member path *modules*) #t)))
      (and (member feature *modules*) #t)))

(define (require:feature->path feature)
  (if (symbol? feature)
      (if (memq feature *features*) #t
	  (let ((path (cdr (or (assq feature *catalog*) '(#f . #f)))))
	    (cond ((symbol? path) (require:feature->path path))
		  ((member (if (pair? path) (cdr path) path) *modules*)
		   #t)
		  (else path))))
      (if (member feature *modules*) #t
	  feature)))

(define (require:require feature)
  (let ((path (require:feature->path feature)))
    (cond ((and (not path) (string? feature) (file-exists? feature))
	   (set! path feature)))
    (cond ((eq? path #t) #t)
	  ((not path)
	   ;;(newline) (display ";required feature not supported: ")
	   ;;(display feature) (newline)
	   (slib:error ";required feature not supported: " feature))
	  ((not (pair? path))		;simple name
	   (slib:load path)
	   (require:provide feature))
	  (else				;special loads
	   (require (car path))
	   (apply (case (car path)
		    ((macro) macro:load)
		    ((syntactic-closures) synclo:load)
		    ((syntax-case) syncase:load)
		    ((macros-that-work) macwork:load)
		    ((macro-by-example) defmacro:load)
		    ((defmacro) defmacro:load)
		    ((source) slib:load-source)
		    ((compiled) slib:load-compiled))
		  (if (list? path) (cdr path) (list (cdr path))))
	   (require:provide feature)))))

(define (require:provide feature)
  (if (symbol? feature)
      (if (not (memq feature *features*))
	  (set! *features* (cons feature *features*)))
      (if (not (member feature *modules*))
	  (set! *modules* (cons feature *modules*)))))

(require:provide 'vicinity)

(define provide require:provide)
(define provided? require:provided?)
(define require require:require)

;;; Supported by all implementations
(provide 'eval)
(provide 'defmacro)

(if (and (string->number "0.0") (inexact? (string->number "0.0")))
    (provide 'inexact))
(if (rational? (string->number "1/19")) (provide 'rational))
(if (real? (string->number "0.0")) (provide 'real))
(if (complex? (string->number "1+i")) (provide 'complex))
(let ((n (string->number "9999999999999999999999999999999")))
  (if (and n (exact? n)) (provide 'bignum)))

(define current-time
  (if (provided? 'current-time) current-time
      (let ((c 0))
	(lambda () (set! c (+ c 1)) c))))
(define difftime (if (provided? 'current-time) difftime -))
(define offset-time (if (provided? 'current-time) offset-time +))

(define report:print
  (lambda args
    (for-each (lambda (x) (write x) (display #\ )) args)
    (newline)))

(define slib:report
  (let ((slib:report (lambda () (slib:report-version) (slib:report-locations))))
    (lambda args
      (cond ((null? args) (slib:report))
	    ((not (string? (car args)))
	     (slib:report-version) (slib:report-locations #t))
	    ((require:provided? 'transcript)
	     (transcript-on (car args))
	     (slib:report)
	     (transcript-off))
	    ((require:provided? 'with-file)
	     (with-output-to-file (car args) slib:report))
	    (else (slib:report))))))

(define slib:report-version
  (lambda ()
    (report:print
     'SLIB *SLIB-VERSION* 'on (scheme-implementation-type)
     (scheme-implementation-version) 'on (software-type))))

(define slib:report-locations
  (let ((features *features*) (catalog *catalog*))
    (lambda args
      (report:print '(IMPLEMENTATION-VICINITY) 'is (implementation-vicinity))
      (report:print '(LIBRARY-VICINITY) 'is (library-vicinity))
      (report:print '(SCHEME-FILE-SUFFIX) 'is (scheme-file-suffix))
      (cond (*load-pathname*
	     (report:print '*LOAD-PATHNAME* 'is *load-pathname*)))
      (cond ((not (null? *modules*))
	     (report:print 'Loaded '*MODULES* 'are: *modules*)))
      (let* ((i (+ -1 5)))
	(cond ((eq? (car features) (car *features*)))
	      (else (report:print 'loaded '*FEATURES* ':) (display slib:tab)))
	(for-each
	 (lambda (x)
	   (cond ((eq? (car features) x)
		  (if (not (eq? (car features) (car *features*))) (newline))
		  (report:print 'Implementation '*FEATURES* ':)
		  (display slib:tab) (set! i (+ -1 5)))
		 ((zero? i) (newline) (display slib:tab) (set! i (+ -1 5)))
		 ((not (= (+ -1 5) i)) (display #\ )))
	   (write x) (set! i (+ -1 i)))
	 *features*))
      (newline)
      (let* ((i #t))
	(cond ((not (eq? (car catalog) (car *catalog*)))
	       (report:print 'Additional '*CATALOG* ':)))
	(cond ((or (pair? args) (not (eq? (car catalog) (car *catalog*))))
	       (for-each
		(lambda (x)
		  (cond ((eq? (car catalog) x)
			 (report:print 'Implementation '*CATALOG* ':)
			 (set! i (pair? args))
			 (cond (i)
			       (else (display slib:tab) (report:print x)
				     (display slib:tab) (report:print '...)))))
		  (cond (i (display slib:tab) (report:print x))))
		*catalog*))
	      (else (report:print 'Implementation '*CATALOG* ':)
		    (display slib:tab) (report:print (car *catalog*))
		    (display slib:tab) (report:print '...))))
      (newline))))

(let ((sit (scheme-implementation-version)))
  (cond ((zero? (string-length sit)))
	((or (not (string? sit)) (char=? #\? (string-ref sit 0)))
	 (newline)
	 (slib:report-version)
	 (report:print 'edit (scheme-implementation-type) ".init"
		       'to 'set '(scheme-implementation-version))
	 (report:print '(IMPLEMENTATION-VICINITY) 'is (implementation-vicinity))
	 (report:print 'type '(slib:report) 'for 'configuration)
	 (newline))))
