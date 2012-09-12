;;; "dbutil.scm" relational-database-utilities
; Copyright 1994, 1995 Aubrey Jaffer
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

(require 'relational-database)

(define (db:base-type path)
  'alist-table)				; currently the only one.

(define (dbutil:wrap-command-interface rdb)
  (and rdb
       (let* ((rdms:commands ((rdb 'open-table) '*commands* #f))
	      (command:get
	       (and rdms:commands (rdms:commands 'get 'procedure))))
	 (and command:get
	      (letrec ((wdb (lambda (command)
			      (let ((com (command:get command)))
				(cond (com ((slib:eval com) wdb))
				      (else (rdb command)))))))
		(let ((init (wdb '*initialize*)))
		  (if (procedure? init) init wdb)))))))

(define (dbutil:open-database! path . arg)
  (let ((type (if (null? arg) (db:base-type path) (car arg))))
    (require type)
    (dbutil:wrap-command-interface
     (((make-relational-system (slib:eval type)) 'open-database)
      path #t))))

(define (dbutil:open-database path . arg)
  (let ((type (if (null? arg) (db:base-type path) (car arg))))
    (require type)
    (dbutil:wrap-command-interface
     (((make-relational-system (slib:eval type)) 'open-database)
      path #f))))

(define (dbutil:create-database path type)
  (require type)
  (let ((rdb (((make-relational-system (slib:eval type)) 'create-database)
	      path)))
    (dbutil:define-tables
     rdb
     '(parameter-arity
       ((name symbol))
       ((predicate? expression)
	(procedure expression))
       ((single (lambda (a) (and (pair? a) (null? (cdr a)))) car)
	(optional
	 (lambda (lambda (a) (or (null? a) (and (pair? a) (null? (cdr a))))))
	 identity)
	(boolean
	 (lambda (a) (or (null? a)
			 (and (pair? a) (null? (cdr a)) (boolean? (car a)))))
	 (lambda (a) (if (null? a) #f (car a))))
	(nary (lambda (a) #t) identity)
	(nary1 (lambda (a) (not (null? a))) identity))))
    (for-each (((rdb 'open-table) '*domains-data* #t) 'row:insert)
	      '((parameter-list *catalog-data* #f symbol #f)
		(parameter-name-translation *catalog-data* #f symbol #f)
		(parameter-arity parameter-arity #f symbol #f)))
    (dbutil:define-tables
     rdb
     '(*parameter-columns*
       *columns*
       *columns*
       ((1 #t index #f uint)
	(2 #f name #f symbol)
	(3 #f arity #f parameter-arity)
	(4 #f domain #f domain)
	(5 #f default #f expression)
	(6 #f expander #f expression)
	(7 #f documentation #f string)))
     '(no-parameters
       *parameter-columns*
       *parameter-columns*
       ())
     '(no-parameter-names
       ((name string))
       ((parameter-index uint))
       ())
     '(*commands*
       ((name symbol))
       ((parameters parameter-list)
	(parameter-names parameter-name-translation)
	(procedure expression)
	(documentation string))
       ((domain-checker
	 no-parameters
	 no-parameter-names
	 (lambda (rdb)
	   (let* ((ro:domains ((rdb 'open-table) '*domains-data* #f))
		  (ro:get-dir (ro:domains 'get 'domain-integrity-rule))
		  (ro:for-tab (ro:domains 'get 'foreign-table)))
	     (lambda (domain)
	       (let ((fkname (ro:for-tab domain))
		     (dir (slib:eval (ro:get-dir domain))))
		 (cond (fkname (let* ((fktab ((rdb 'open-table) fkname #f))
				      (p? (fktab 'get 1)))
				 (cond (dir (lambda (e) (and (dir e) (p? e))))
				       (else p?))))
		       (else dir))))))
	 "return procedure to check given domain name")

	(add-domain
	 no-parameters
	 no-parameter-names
	 (lambda (rdb)
	   (((rdb 'open-table) '*domains-data* #t) 'row:insert))
	 "given the row describing it, add a domain")

	(delete-domain
	 no-parameters
	 no-parameter-names
	 (lambda (rdb)
	   (((rdb 'open-table) '*domains-data* #t) 'row:remove))
	 "given its name, delete a domain"))))
    (dbutil:wrap-command-interface rdb)))

(define (make-command-server rdb command-table)
  (let* ((comtab ((rdb 'open-table) command-table #f))
	 (names (comtab 'column-names))
	 (row-ref (lambda (row name) (list-ref row (position name names))))
	 (comgetrow (comtab 'row:retrieve)))
    (lambda (comname command-callback)
      (let* ((command:row (comgetrow comname))
	     (parameter-table ((rdb 'open-table)
			       (row-ref command:row 'parameters) #f))
	     (parameter-names
	      ((rdb 'open-table) (row-ref command:row 'parameter-names) #f))
	     (comval ((slib:eval (row-ref command:row 'procedure)) rdb))
	     (options ((parameter-table 'get* 'name)))
	     (positions ((parameter-table 'get* 'index)))
	     (arities ((parameter-table 'get* 'arity)))
	     (defaults (map slib:eval ((parameter-table 'get* 'default))))
	     (domains ((parameter-table 'get* 'domain)))
	     (types (map (((rdb 'open-table) '*domains-data* #f) 'get 'type-id)
			 domains))
	     (dirs (map (rdb 'domain-checker) domains))
	     (aliases
	      (map list ((parameter-names 'get* 'name))
		   (map (parameter-table 'get 'name)
			((parameter-names 'get* 'parameter-index))))))
	(command-callback comname comval options positions
			  arities types defaults dirs aliases)))))

(define (dbutil:define-tables rdb . spec-list)
  (define new-tables '())
  (define dom:typ (((rdb 'open-table) '*domains-data* #f) 'get 4))
  (define create-table (rdb 'create-table))
  (define open-table (rdb 'open-table))
  (define table-exists? (rdb 'table-exists?))
  (define (check-domain dname)
    (cond ((dom:typ dname))
	  ((member dname new-tables)
	   (let* ((ftab (open-table
			 (string->symbol
			  (string-append "desc:" (symbol->string dname)))
			 #f)))
	     ((((rdb 'open-table) '*domains-data* #t) 'row:insert)
	      (list dname dname #f
		    (dom:typ ((ftab 'get 'domain-name) 1)) #f))))))
  (define (define-table name prikeys slots data)
    (cond
     ((table-exists? name)
      (let* ((tab (open-table name #t))
	     (row:update (tab 'row:update)))
	(for-each row:update data)))
     ((and (symbol? prikeys) (eq? prikeys slots))
      (cond ((not (table-exists? slots))
	     (slib:error "Table doesn't exist:" slots)))
      (set! new-tables (cons name new-tables))
      (let* ((tab (create-table name slots))
	     (row:insert (tab 'row:insert)))
	(for-each row:insert data)
	((tab 'close-table))))
     (else
      (let* ((descname
	      (string->symbol (string-append "desc:" (symbol->string name))))
	     (tab (create-table descname))
	     (row:insert (tab 'row:insert))
	     (j 0))
	(set! new-tables (cons name new-tables))
	(for-each (lambda (des)
		    (set! j (+ 1 j))
		    (check-domain (cadr des))
		    (row:insert (list j #t (car des)
				      (if (null? (cddr des)) #f (caddr des))
				      (cadr des))))
		  prikeys)
	(for-each (lambda (des)
		    (set! j (+ 1 j))
		    (check-domain (cadr des))
		    (row:insert (list j #f (car des)
				      (if (null? (cddr des)) #f (caddr des))
				      (cadr des))))
		  slots)
	((tab 'close-table))
	(set! tab (create-table name descname))
	(set! row:insert (tab 'row:insert))
	(for-each row:insert data)
	((tab 'close-table))))))
  (for-each (lambda (spec) (apply define-table spec)) spec-list))

(define create-database dbutil:create-database)
(define open-database! dbutil:open-database!)
(define open-database dbutil:open-database)
(define define-tables dbutil:define-tables)
