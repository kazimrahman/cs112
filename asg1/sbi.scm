#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

;; FROM EXAMPLES/SYMBOLS.SCM
;; Define the three tables

;; Function table
(define *f-table* (make-hash))
(define (f-set! key value)
  (hash-set! *f-table* key value))
;; Label table
(define *l-table* (make-hash))

;; Variable table
(define *v-table* (make-hash))





(for-each
    (lambda (pair)
            (f-set! (car pair) (cadr pair)))
    `(

        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (div     ,(lambda (x y) (floor (/ x y))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (^       ,expt)
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,log)
        (sqrt    ,sqrt)
		(=       ,=)
		(<       ,<)
		(>       ,>)
		(<=      ,<=)
		(>=      ,>=)
		(<>      ,(lambda (a b) (not (= a b))))

     ))


;; print
(define (new-print expr)
  (when (not(null? expr))
    (map (lambda (token) (display (evalexpr token))) expr))
  (newline))
  
;;let  
(define (new-let arg)
  (hash-set! *v-table* (car arg)
     (evalexpr (cadr arg))))

 ;;if
(define (new-if arg)
  (eval-expr (cadr arg)))
 

;; dim
(define (new-dim arg)
  (when (= (length arg) 2)
    (when (not(null? arg))
      (hash-set! *v-table* (car arg) (cadr arg)
                 (make-vector (cadr arg)))))) 
  
;; parse for the labels in the statement
(define (label-scan program)
	(map (lambda (line)
		(when (and (>= (length line) 2) (symbol? (cadr line)))
			(when (not (and (null? line) (null? (cdr line))))
				(hash-set! *l-table* (cadr line)
					(x (car line)))) program)
		)
))
  
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)



(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)




(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)



(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program)   )))


;;
;; The function evalexpr outlines how to evaluate a list
;; recursively.
;; from examples/scheme/hashexample.scm
(define (evalexpr expr)
   	 
	(cond
		;check variable table for the expr from the list
		((hash-has-key? *v-table* expr)
		 (hash-ref *v-table* expr))
		;if not then check function table
		((hash-has-key? *f-table* expr)
		 (hash-ref *f-table* expr))
		((number? expr) (+ expr 0.0))
		((string? expr) expr)
		((pair? expr)   (apply (hash-ref *f-table* (car expr))
                                (map evalexpr (cdr expr))))
	)
)	


(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
               ;; replace write-program-by-line with interpreter function
               ;(show "whole list" program)
              (write-program-by-line sbprogfile program))))


(main (vector->list (current-command-line-arguments)))
