;;;;"scanf.scm" implemenation of formated input
;From: jjb@isye.gatech.edu (John Bartholdi)
;
; This code is in the public domain.

;;;;Heavily hacked by jaffer 4/94
;;;This implements a Scheme-oriented version of SCANF: returns a list of  
;;;objects read (rather than set!-ing values).

; Example:
;(sscanf "I want $100 for 3.14159 kilograms of ice cream."
;	 "%7s %*c %d %*s %*f %24s")
; => ("I want " 100 "kilograms of ice cream.")

(require 'string-port)

(define (scanf:scanf <format-string> . args)
  (scanf:fscanf (current-input-port) <format-string>))

(define (scanf:sscanf str <format-string> . args)
  (call-with-input-string
   str (lambda (p) (scanf:fscanf p <format-string>))))

(define (scanf:fscanf <input-port> <format-string> . args)
  (call-with-input-string
   <format-string>
   (lambda (ip1)
     (let loop1 ((items-read '()))
       ;; get the next format by reading between "%" characters:
       (let ((next-format
	      (scanf:read-separate-item (lambda (c) (char=? #\% c)) ip1)))
	 (cond
	  ((eof-object? next-format)
	   (reverse items-read))
	  (else				; interpret next format:
	   (call-with-input-string
	    next-format
	    (lambda (ip2)
	      (let loop2 ((field-width-chars '())
			  (report-field? #T))
		(let ((c (char-downcase (read-char ip2))))
		  (cond
		   ((char=? #\* c)
		    (loop2 field-width-chars #F))
		   ((char-numeric? c)
		    (loop2 (cons c field-width-chars) report-field?))
		   ((char=? #\c c)
		    (let ((next-item (read-char <input-port>)))
		      (if report-field?
			  (loop1 (cons next-item items-read))
			  (loop1 items-read))))
		   (else
		    (let ((next-item
			   (if (null? field-width-chars)
					; no fieldwidth given so
			       (scanf:read-separate-item char-whitespace?
							 <input-port>)
					; else read block of chars
			       (let ((field-width
				      (string->number
				       (list->string
					(reverse field-width-chars)))))
				 (scanf:read-n-chars field-width
						     <input-port>)))))
		      (case c
			((#\e #\f #\g #\i #\o #\d #\u #\x)
			 (call-with-input-string
			  next-item
			  (lambda (ip-str)
			    (let* ((num (scanf:read-separate-item
					 char-whitespace? ip-str)))
			      (set! num
				    (case c
				      ((#\e #\f #\g #\i) (string->number num))
				      ((#\o) (string->number num 8))
				      ((#\d #\u) (string->number num 10))
				      ((#\x) (string->number num 16))))
			      (if (number? num)
				  (if report-field?
				      (loop1 (cons num items-read))
				      (loop1 items-read))
				  (slib:error
				   'SCANF
				   "Number format (~s) does not match input (~s)"
				   next-format
				   next-item))))))
			((#\s)
			 (if report-field?
			     (loop1 (cons next-item items-read))
			     (loop1 items-read)))
			(else
			 (slib:error
			  'SCANF
			  "Unsupported format directive: ~a" c)))))))))))))))))

; Reads characters from <input-port> and returns them in a 
; string (excluding any character of which
;    (<separator?> c)
; returns #T).  Reads until reach either end of file or else 
; the first separator following a non-separator.  If at the 
; end of the file, returns eof-object.
;
; <separator?> is a function of one argument, a character, and 
; returns either #T or #F.

(define (scanf:read-separate-item <separator?> <input-port>)
  (let ((c (peek-char <input-port>)))
    (if (eof-object? c) c
	(let loop ((char-list '())
		   (found-valid-chars? #F))
	  (let ((c (read-char <input-port>)))
	    (cond ((eof-object? c)
		   (list->string (reverse char-list)))
		  ((<separator?> c)
		   (if found-valid-chars?
		       (list->string (reverse char-list))
		       (loop char-list #F)))
		  (else			; not a separator:
		   (loop (cons c char-list) #T))))))))

; Reads characters from a port until either <n> are read or
; eof-object? evaluates to #T, then returns all the characters read 
; in a string.

(define (scanf:read-n-chars <n> <input-port>)
  (let ((c (peek-char <input-port>)))
    (if (eof-object? c) c
	(let ((str (make-string <n>)))
	  (let loop ((count 0))
	    (cond
	     ((= <n> count) str)
	     ((eof-object? (peek-char <input-port>))
	      (substring str 0 count))
	     (else
	      (string-set! str count (read-char <input-port>))
	      (loop (+ 1 count)))))))))

(define scanf scanf:scanf)
(define sscanf scanf:sscanf)
(define fscanf scanf:fscanf)
