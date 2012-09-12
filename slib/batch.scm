;;; "batch.scm" Group and execute commands on various systems.
;Copyright (C) 1994, 1995 Aubrey Jaffer
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

(require 'line-i/o)			;Just for write-line
(require 'parameters)
(require 'database-utilities)

;;(define (batch parms op . args) ??)

(define (batch:port parms)
  (car (parameter-list-ref parms 'batch-port)))

(define (batch:dialect parms)		; was batch-family
  (car (parameter-list-ref parms 'batch-dialect)))

(define (batch:line-length-limit parms)
  (let ((bl (parameter-list-ref parms 'batch-line-length-limit)))
    (cond (bl (car bl))
	  (else (case (batch:dialect parms)
		  ((unix) 1024)
		  ((dos) 128)
		  ((vms) 1024)
		  ((system) 1024)
		  ((*unknown*) -1))))))

(define (batch-line parms str)
  (let ((bp (parameter-list-ref parms 'batch-port))
	(ln (batch:line-length-limit parms)))
    (cond ((not bp) (slib:error 'batch-line "missing batch-port parameter"
				parms))
	  ((>= (string-length str) ln) #f)
	  (else (write-line str (car bp)) #t))))

;;; add a Scheme batch-dialect?

(define (batch:system parms . strings)
  (define port (batch:port parms))
  (set! strings (batch:flatten strings))
  (case (batch:dialect parms)
    ((unix) (batch-line parms (apply string-join " " strings)))
    ((dos) (batch-line parms (apply string-join " " strings)))
    ((vms) (batch-line parms (apply string-join " " "$" strings)))
    ((system) (write `(system ,(apply string-join " " strings)) port)
	      (newline port)
	      (system (apply string-join " " strings)))
    ((*unknown*) (write `(system ,(apply string-join " " strings)) port)
		 (newline port)
		 #f)))

(define (batch:run-script parms . strings)
  (case (batch:dialect parms strings)
    ((unix) (batch:system parms strings name))
    ((dos) (batch:system parms strings name))
    ((vms) (batch:system parms (cons #\@ strings)))
    ((system) (batch:system parms strings name))
    ((*unknown*) (batch:system parms strings name)
		 #f)))

(define (batch:comment parms . lines)
  (define port (batch:port parms))
  (set! lines (batch:flatten lines))
  (case (batch:dialect parms)
    ((unix) (every (lambda (line)
		     (batch-line parms (string-append "# " line)))
		   lines))
    ((dos) (every (lambda (line)
		    (batch-line parms
				(string-append
				 "rem" (if (equal? "  " line) ".") line)))
		  lines))
    ((vms) (every (lambda (line)
		    (batch-line parms (string-append "$! " line)))
		  lines))
    ((system) (every (lambda (line)
		       (batch-line parms (string-append "; " line)))
		     lines))
    ((*unknown*) (for-each (lambda (line)
			     (batch-line parms (string-append ";;; " line))
			     (newline port))
			   lines)
		 #f)))

(define (batch:lines->file parms file . lines)
  (define port (batch:port parms))
  (set! lines (batch:flatten lines))
  (case (batch:dialect parms)
    ((unix) (batch-line parms (string-append "rm -f " file))
	    (every
	     (lambda (string)
	       (batch-line parms (string-append "echo '" string "'>>" file)))
	     lines))
    ((dos) (batch-line parms (string-append "DEL " file))
	   (every
	    (lambda (string)
	      (batch-line parms
			  (string-append "ECHO" (if (equal? "" string) "." " ")
					 string ">>" file)))
	    lines))
    ((vms) (and (batch-line parms (string-append "$DELETE " file))
		(batch-line parms (string-append "$CREATE " file))
		(batch-line parms (string-append "$DECK"))
		(every (lambda (string) (batch-line parms string))
		       lines)
		(batch-line parms (string-append "$EOD"))))
    ((system) (write `(delete-file ,file) port) (newline port)
	      (delete-file file)
	      (pretty-print `(call-with-output-file ,file
			       (lambda (fp)
				 (for-each
				  (lambda (string) (write-line string fp))
				  ',lines)))
			    port)
	      (call-with-output-file file
		(lambda (fp) (for-each (lambda (string) (write-line string fp))
				       lines)))
	      #t)
    ((*unknown*) (write `(delete-file ,file) port) (newline port)
		 (pretty-print
		  `(call-with-output-file ,file
		     (lambda (fp)
		       (for-each
			(lambda (string)
			  (write-line string fp))
			,lines)))
		  port)
		 #f)))

(define (batch:delete-file parms file)
  (define port (batch:port parms))
  (case (batch:dialect parms)
    ((unix) (batch-line parms (string-append "rm -f " file))
	    #t)
    ((dos) (batch-line parms (string-append "DEL " file))
	   #t)
    ((vms) (batch-line parms (string-append "$DELETE " file))
	   #t)
    ((system) (write `(delete-file ,file) port) (newline port)
	      (delete-file file))	; SLIB provides
    ((*unknown*) (write `(delete-file ,file) port) (newline port)
		 #f)))

(define (batch:rename-file parms old-name new-name)
  (define port (batch:port parms))
  (case (batch:dialect parms)
    ((unix) (batch-line parms (string-join " " "mv -f" old-name new-name)))
    ((dos) (batch-line parms (string-join " " "MOVE" "/Y" old-name new-name)))
    ((vms) (batch-line parms (string-join " " "$RENAME" old-name new-name)))
    ((system) (batch:extender 'rename-file batch:rename-file))
    ((*unknown*) (write `(rename-file ,old-name ,new-name) port)
		 (newline port)
		 #f)))

(define (batch:call-with-output-script parms name proc)
  (case (batch:dialect parms)
    ((unix) ((cond ((string? name)
		    (lambda (proc)
		      (let ((ans (call-with-output-file name proc)))
			(system (string-append "chmod +x " name))
			ans)))
		   ((output-port? name) (lambda (proc) (proc name)))
		   (else  (lambda (proc) (proc (current-output-port)))))
	     (lambda (port)
	       (write-line "#!/bin/sh" port)
	       (cond
		((and (string? name) (provided? 'bignum))
		 (require 'posix-time)
		 (write-line
		  (string-append
		   "# \""  name "\" build script created "
		   (ctime (current-time)))
		  port)))
	       (proc port))))

    ((dos) ((cond ((string? name)
		   (lambda (proc)
		     (call-with-output-file (string-append name ".bat") proc)))
		  ((output-port? name) (lambda (proc) (proc name)))
		  (else  (lambda (proc) (proc (current-output-port)))))
	    (lambda (port)
	      (cond
	       ((and (string? name) (provided? 'bignum))
		(require 'posix-time)
		(write-line
		 (string-append
		  "rem " name
		  " build script created "
		  (ctime (current-time)))
		 port)))
	      (proc port))))

    ((vms) ((cond ((string? name)
		   (lambda (proc)
		     (call-with-output-file (string-append name ".COM") proc)))
		  ((output-port? name) (lambda (proc) (proc name)))
		  (else  (lambda (proc) (proc (current-output-port)))))
	    (lambda (port)
	      (cond
	       ((and (string? name) (provided? 'bignum))
		(require 'posix-time)
		;;(write-line
		;; "$DEFINE/USER SYS$OUTPUT BUILD.LOG" port)
		(write-line
		 (string-append
		  "$! " name
		  " build script created "
		  (ctime (current-time)))
		 port)))
	      (proc port))))

    ((system) ((cond ((string? name)
		      (lambda (proc)
			(let ((ans (call-with-output-file name
				     (lambda (port) (proc name)))))
			  (system (string-append "chmod +x " name))
			  ans)))
		     ((output-port? name) (lambda (proc) (proc name)))
		     (else  (lambda (proc) (proc (current-output-port)))))
	       (lambda (port)
		 (cond
		  ((and (string? name) (provided? 'bignum))
		   (require 'posix-time)
		   (write-line
		    (string-append
		     ";;; \""  name
		     "\" build script created " (ctime (current-time)))
		    port)))
		 (proc port))))

    ((*unknown*) ((cond ((string? name)
			 (lambda (proc)
			   (let ((ans (call-with-output-file name
					(lambda (port) (proc name)))))
			     (system (string-append "chmod +x " name))
			     ans)))
			((output-port? name) (lambda (proc) (proc name)))
			(else  (lambda (proc) (proc (current-output-port)))))
		  (lambda (port)
		    (cond
		     ((and (string? name) (provided? 'bignum))
		      (require 'posix-time)
		      (write-line
		       (string-append
			";;; \""  name
			"\" build script created " (ctime (current-time)))
		       port)))
		    (proc port)))
		 #f)))

;;; This little ditty figures out how to use a Scheme extension or
;;; SYSTEM to execute a command that is not available in the batch
;;; mode chosen.

(define (batch:extender NAME BATCHER)
  (lambda (parms . args)
    (define port (batch:port parms))
    (cond
     ((provided? 'i/o-extensions)	; SCM specific
      (write `(,NAME ,@args) port)
      (newline port)
      (apply (slib:eval NAME) args))
     (else
      (let ((pl (make-parameter-list (map car parms))))
	(adjoin-parameters!
	 pl (cons 'batch-dialect (os->batch-dialect
				  (parameter-list-ref parms 'platform))))
	(system
	 (call-with-output-string
	  (lambda (port)
	    (batch:call-with-output-script
	     port
	     (lambda (batch-port)
	       (define new-parms (copy-tree pl))
	       (adjoin-parameters! new-parms (list 'batch-port batch-port))
	       (apply BATCHER new-parms args)))))))))))

(define (replace-suffix str old new)
  (define (cs str)
    (let* ((len (string-length str))
	   (re (- len (string-length old))))
      (cond ((string-ci=? old (substring str re len))
	     (string-append (substring str 0 re) new))
	    (else
	     (slib:error 'replace-suffix "suffix doens't match:"
			 old str)))))
  (if (string? str) (cs str) (map cs str)))

(define (string-join joiner . args)
  (if (null? args) ""
      (apply string-append
	     (car args)
	     (map (lambda (s) (string-append joiner s)) (cdr args)))))

(define (batch:flatten strings)
  (apply
   append (map
	   (lambda (obj)
	     (cond ((eq? "" obj) '())
		   ((string? obj) (list obj))
		   ((eq? #f obj) '())
		   ((null? obj) '())
		   ((list? obj) (batch:flatten obj))
		   (else (slib:error 'batch:flatten "unexpected type"
				     obj "in" strings))))
	   strings)))

(define batch:platform (software-type))
(cond ((and (eq? 'unix batch:platform) (provided? 'system))
       (let ((file-name (tmpnam)))
	 (system (string-append "uname > " file-name))
	 (set! batch:platform (call-with-input-file file-name read))
	 (delete-file file-name))))

(define batch:database #f)
(define (os->batch-dialect os)
  ((((batch:database 'open-table) 'operating-system #f)
    'get 'os-family) os))

(define (batch:initialize! database)
  (set! batch:database database)
  (define-tables database

    '(batch-dialect
      ((family atom))
      ()
      ((unix)
       (dos)
       (vms)
       (system)
       (*unknown*)))

    '(operating-system
      ((name symbol))
      ((os-family batch-dialect))
      (;;(3b1		*unknown*)
       (acorn		*unknown*)
       (aix		unix)
       (alliant		*unknown*)
       (amiga		*unknown*)
       (apollo		unix)
       (apple2		*unknown*)
       (arm		*unknown*)
       (atari.st	*unknown*)
       (cdc		*unknown*)
       (celerity	*unknown*)
       (concurrent	*unknown*)
       (convex		*unknown*)
       (encore		*unknown*)
       (harris		*unknown*)
       (hp-ux		unix)
       (hp48		*unknown*)
       (isis		*unknown*)
       (linux		unix)
       (mac		*unknown*)
       (masscomp	unix)
       (ms-dos		dos)
       (mips		*unknown*)
       (ncr		*unknown*)
       (newton		*unknown*)
       (next		unix)
       (novell		*unknown*)
       (os/2		dos)
       (prime		*unknown*)
       (psion		*unknown*)
       (pyramid		*unknown*)
       (sequent		*unknown*)
       (sgi		*unknown*)
       (stratus		*unknown*)
       (sun-os		unix)
       (transputer	*unknown*)
       (unicos		unix)
       (unix		unix)
       (vms		vms)
       (*unknown*	*unknown*)
       )))

  ((database 'add-domain) '(operating-system operating-system #f symbol #f))
  )
