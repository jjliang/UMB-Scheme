;;; "alistab.scm" database tables using association lists (assoc)
; Copyright 1994 Aubrey Jaffer
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

;;; LLDB	is (filename . alist-table)
;;; HANDLE	is (table-name . TABLE)
;;; TABLE	is an alist of (Primary-key . ROW)
;;; ROW		is a list of non-primary VALUEs

(define alist-table
(let ((catalog-id 0)
      (resources '*base-resources*))

(define (make-base filename dim types)
  (list filename
	(list catalog-id)
	(list resources (list 'free-id 1))))

(define (open-base infile writable)
  (cons (if (input-port? infile) #f infile)
	((lambda (fun)
	   (if (input-port? infile)
	       (fun infile)
	       (call-with-input-file infile fun)))
	 read)))

(define (write-base lldb outfile)
  ((lambda (fun)
     (cond ((output-port? outfile) (fun outfile))
	   ((string? outfile) (call-with-output-file outfile fun))
	   (else #f)))
   (lambda (port)
     (display (string-append
	       ";;; \"" outfile "\" SLIB alist-table database	 -*-scheme-*-")
	      port)
     (newline port) (newline port)
     (display "(" port) (newline port)
     (for-each
      (lambda (table)
	(display " (" port)
	(write (car table) port) (newline port)
	(for-each
	 (lambda (row)
	   (display "  " port) (write row port) (newline port))
	 (cdr table))
	(display " )" port) (newline port))
      (cdr lldb))
     (display ")" port) (newline port)
;     (require 'pretty-print)
;     (pretty-print (cdr lldb) port)
     (set-car! lldb (if (string? outfile) outfile #f))
     #t)))

(define (sync-base lldb)
  (cond ((car lldb) (write-base lldb (car lldb)) #t)
	(else
;;;	 (display "sync-base: database filename not known")
	 #f)))

(define (close-base lldb)
  (cond ((car lldb) (write-base lldb (car lldb))
		    (set-cdr! lldb #f)
		    (set-car! lldb #f) #t)
	((cdr lldb) (set-cdr! lldb #f)
		    (set-car! lldb #f) #t)
	(else
;;;	 (display "close-base: database not open")
	 #f)))

(define (make-table lldb dim types)
  (let ((free-hand (open-table lldb resources 1 '(atom integer))))
    (and free-hand
	 (let* ((row (remover free-hand 'free-id))
		(id #f))
	   (cond (row
		  (set! id (car row))
		  ((make-putter 1 '(atom integer)) free-hand 'free-id
						   (list (+ 1 id)))
		  (set-cdr! lldb (cons (list id) (cdr lldb)))
		  id)
		 (else #f))))))

(define (open-table lldb base-id dim types)
  (assoc base-id (cdr lldb)))

(define (remover nalist key)
  (let ((alist (cdr nalist)))
    (cond ((null? alist) #f)
	  ((equal? key (caar alist))
	   (set-cdr! nalist (cdr alist))
	   (cdar alist))
	  ((null? (cdr alist)) #f)
	  ((equal? key (caadr alist))
	   (set! nalist (cdadr alist))
	   (set-cdr! alist (cddr alist))
	   nalist)
	  (else
	   (let l ((al (cdr alist)))
	     (cond ((null? (cdr al)) #f)
		   ((equal? key (caadr al))
		    (set! nalist (caadr al))
		    (set-cdr! al (cddr al))
		    nalist)
		   (else (l (cdr al)))))))))

(define (kill-table lldb base-id dim types)
  (and (remover lldb base-id) #t))

(define handle->base-id car)
(define handle->alist cdr)
(define set-handle-alist! set-cdr!)

(define (present? handle key)
  (assoc key (handle->alist handle)))

(define (make-putter prinum types)
  (lambda (handle ckey restcols)
    (let ((row (assoc ckey (handle->alist handle))))
      (cond (row (set-cdr! row restcols))
	    (else (set-handle-alist!
		   handle (cons (cons ckey restcols)
				(handle->alist handle))))))))

(define (make-getter prinum types)
  (lambda (handle ckey)
    (let ((row (assoc ckey (handle->alist handle))))
      (and row (cdr row)))))

(define (make-list-keyifier prinum types)
  (if (= 1 prinum) car list->vector))

(define (make-keyifier-1 type)
  identity)

(define (make-key->list prinum types)
  (cond ((= 1 prinum) list)
	(else vector->list)))

(define (make-key-extractor primary-limit column-type-list index)
  (if (= 1 primary-limit) identity
      (let ((i (+ -1 index)))
	(lambda (v) (vector-ref v i)))))

(define (for-each-key handle operation)
  (for-each (lambda (x) (operation (car x))) (handle->alist handle)))

(define (map-key handle operation)
  (map (lambda (x) (operation (car x))) (handle->alist handle)))

(define (ordered-for-each-key handle operation)
  (define (key->sortable k)
    (cond ((number? k) k)
	  ((string? k) k)
	  ((symbol? k) (symbol->string k))
	  ((vector? k) (map key->sortable (vector->list k)))
	  (else (slib:error "unsortable key" k))))
  ;; This routine assumes that the car of its operands are either
  ;; numbers or strings (or lists of those).
  (define (car-key-< x y)
    (key-< (car x) (car y)))
  (define (key-< x y)
    (cond ((and (number? x) (number? y)) (< x y))
	  ((number? x) #t)
	  ((number? y) #f)
	  ((string? x) (string<? x y))
	  ((key-< (car x) (car y)) #t)
	  ((key-< (car y) (car x)) #f)
	  (else (key-< (cdr x) (cdr y)))))
  (require 'sort)
  (for-each operation
	    (map cdr (sort! (map (lambda (p) (cons (key->sortable (car p))
						   (car p)))
				 (handle->alist handle))
			    car-key-<))))

(define (supported-type? type)
  (case type
    ((base-id atom integer boolean string symbol expression) #t)
    (else #f)))

(define (supported-key-type? type)
  (case type
    ((atom integer symbol string) #t)
    (else #f)))

  (lambda (operation-name)
    (case operation-name
      ((make-base) make-base)
      ((open-base) open-base)
      ((write-base) write-base)
      ((sync-base) sync-base)
      ((close-base) close-base)
      ((make-table) make-table)
      ((open-table) open-table)
      ((kill-table) kill-table)
      ((make-keyifier-1) make-keyifier-1)
      ((make-list-keyifier) make-list-keyifier)
      ((make-key->list) make-key->list)
      ((make-key-extractor) make-key-extractor)
      ((supported-type?) supported-type?)
      ((supported-key-type?) supported-key-type?)
      ((present?) present?)
      ((make-putter) make-putter)
      ((make-getter) make-getter)
      ((delete) remover)
      ((for-each-key) for-each-key)
      ((map-key) map-key)
      ((ordered-for-each-key) ordered-for-each-key)
      ((catalog-id) catalog-id)
      (else #f)
      ))
  ))
