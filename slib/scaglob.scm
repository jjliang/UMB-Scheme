;;; "scaglob.scm" syntax-case initializations
;;; From: Harald Hanche-Olsen <hanche@imf.unit.no>

;;; This file was munged by a simple minded sed script since it left
;;; its original authors' hands.  See syncase.sh for the horrid details.

;;; init.ss
;;; Robert Hieb & Kent Dybvig
;;; 92/06/18

; These initializations are done here rather than "expand.ss" so that
; "expand.ss" can be loaded twice (for bootstrapping purposes).

(define expand-syntax #f)
(define syntax-dispatch #f)
(define generate-temporaries #f)
(define identifier? #f)
(define syntax-error #f)
(define syntax-object->datum #f)
(define bound-identifier=? #f)
(define free-identifier=? #f)
(define syncase:install-global-transformer #f)
(define implicit-identifier #f)
