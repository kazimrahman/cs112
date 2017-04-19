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

;; Note to self:
;; car - first elem
;; cdr - second elem and all elem after
;; cadr - second elem
;; make-vector - size [v]
;; map - Applies proc to the elements of the lsts from the first
;; elements to the last.

;; let - you can't reference bindings previously defined in the
;; same let expression

;; let* - possible to refer to previous bindings in the same
;; let* expression
;; hash-has-key? - if the hash contains value for the given key
;; hash-ref - returns the value for key in hash
;; hash-set! - overwrites any existing mapping for key
;; apply - example: (apply + '(1 2 3)) = 6
;; procedure? - Returns #t if v is a procedure, #f otherwise.
;; symbols - immutable strings


;;;; Define the three tables ;;;;
;; Tables hold the states of items such as variables, operators, etc.

;; Function table: holds all functions, which includes operators.
;; Added code from symbols.scm from languages/scheme/examples/symbols.scm
(define *function-table* (make-hash))
(define (func-set! key value)
  (hash-set! *function-table* key value))
;; Label table: hold addresses of each line, one level up from
;; statements.
(define *label-table* (make-hash))

;; Variable-table: holds the value of all variables
(define *variable-table* (make-hash))

;; Initialize *function-table* here? (use for-each loop)
;; Took some code from languages/scheme/examples/symbols.scm
(for-each
 (lambda (pair)
   (func-set! (car pair) (cadr pair)))
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
   (^       ,expt)
   (+       ,+)
   (-       ,-)
   (*       ,*)
   (/       ,/)
   (abs     ,abs)
   (floor   ,floor)
   (=       ,=)
   (<       ,<)
   (>       ,>)
   (<=      ,<=)
   (>=      ,>=)
   (<>      ,(lambda (a b) (not (= a b))))
   (acos    ,acos)
   (asin    ,asin)
   (atan    ,atan)
   (cos     ,cos)
   (round   ,round)
   (sin     ,sin)
   (tan     ,tan)
   (ceil    ,ceiling)
   (exp     ,exp)
   (floor   ,floor)
   (log     ,log)
   (sqrt    ,sqrt)))

;; print
;; Statement -> '(' 'print' { Printable } ')'
;; Printable -> String | Expression
;; Each operand is printed in sequence, with a space
;; before expression values
;; newline is output at the end of print statement | print
;; statements are the only place Strings may occur in SBIR
(define (my-print arg)
  (unless (null? arg)
    (map (lambda (a) (display (eval-expr a))) arg))
  (newline))

;; let
;; Statement -> '(' 'let' Memory Expression ')'
;; Memory -> Array | Variable
;; Variable - value is stored into table
;; Array - the store message is sent to the vector
;; representing the array
(define (my-let arg)
  (hash-set! *variable-table* (car arg) (eval-expr (cadr arg))))

;; goto
;; Statement -> '(' 'goto' Label ')'
;; Control transfers to the statement referred to by the Label
;; An error occurs if Label is not defined
;; IMPLEMENTATION IS IN THE write-program-by-line

;; if
;; Statement -> '(' 'if' '(' Relop Expression Expression ')' Label ')'
;; Relop -> '=' | '<' | '>' | '<>' | '<=' | '>='
;; Two expressions are compared according to the given Relop
(define (my-if arg)
  (eval-expr (cadr arg)))



;; Input
;; Statement -> '(' 'input' Memory { Memory } ')'
;; Each value read into a Variable, the value is inserted into the
;; Symbol table under that variable's key.  For arrays, the array
;; must exist and the subscript not be out of bounds
;; Variable 'inputcount is inserted into the symbol table at the
;; of execution of this statement and initialized to the number of
;; of values successfully read in. Value of -1 is returned to indicate
;; EOF.
(define (my-input arg inputcount)
  (define (my-input2 arg inputcount)
    (cond
      ((null? arg)
       (hash-set! *variable-table* 'inputcount inputcount))
      (else
       (let ((read-inputs (read)))
         (cond
           ((eof-object? read-inputs)
            (hash-set! *variable-table* 'inputcount -1))
           (else
            (hash-set! *variable-table* (car arg) read-inputs)
            (set! inputcount (add1 inputcount))
            (hash-set! *variable-table* 'inputcount inputcount)
            (my-input2 (cdr arg) inputcount)))))))
  (my-input2 arg inputcount))



;;  Create a label scanner.
(define (get-labels program)
  (map (lambda (line)
         ;; Line must be greater than 2 number + statement
         (when (and (>= (length line) 2) (symbol? (cadr line)))
           ;; Not null for both the line and 2nd element+
           (unless (and (null? line) (null? (cdr line)))
             ;; Set label to the item(key) and linenum(value)
             (hash-set! *label-table* (cadr line)
                        (sub1 (car line)))))) program))

;; With the function table we need to evaluate the list
(define (eval-expr arg) ; Evaluates expressions.
  (cond
    ;; Check if its in variable table first
    ;; because there might be a value that overwrites
    ;; the func table.
    ((hash-has-key? *variable-table* arg)
     (hash-ref *variable-table* arg))
    ;; Check function table
    ((hash-has-key? *function-table* arg)
     (hash-ref *function-table* arg))
    ;; Check number and convert to float
    ((number? arg) (+ arg 0.0))
    ;; Check if string
    ((string? arg) arg)
    ;; If it’s a procedure then evaluate it
    ((pair? arg)
          (when (procedure? (hash-ref *function-table*  (car arg)))
             (apply (hash-ref *function-table*  (car arg))
                    (map eval-expr (cdr arg)))))))


(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))


;; Added arguments for passing statements and linenum
(define (write-program-by-line item program linenumber)
  ;; Check what type of statement it is…
  (define (line-determiner program linenumber)
    ;; Make sure the program is not empty
    (when (> (length program) linenumber)
      (let ((line (list-ref program linenumber)))
        (cond

          ;; If the line starts with number, then statement
          ((and (= (length line) 2) (pair? (cadr line)))
           (write-program-by-line (cadr line) program linenumber))

          ;; If the line starts with number, jump, then statement
          ((= (length line) 3)
           (write-program-by-line (caddr line) program linenumber))

          ;; Else continue with the next line
          (else (line-determiner program (add1 linenumber)))))))

  ;; If the first time we start
  (cond ((eqv? item "startprogram")
         (get-labels program)
         (line-determiner program 0))
        ;; Continue to see which function it is equal to
        (else
         (cond
           ;; Print
           ((equal? (car item) 'print)
            (if (null? (cdr item))
                (newline)
                (my-print (cdr item)))
            (line-determiner program (add1 linenumber)))

           ;; Let
           ((equal? (car item) 'let)
            (my-let (cdr item))
            (line-determiner program (add1 linenumber)))

           ;; If
           ((equal? (car item) 'if)
            (if (my-if item)
                (line-determiner program
                                 (hash-ref *label-table* (caddr item)))
                (line-determiner program (add1 linenumber))))

           ;; Dim
           ((equal? (car item) 'dim)
            (my-dim (cdr item))
            (line-determiner program (add1 linenumber)))

           ;; Goto
           ((equal? (car item) 'goto)
            (line-determiner program
                             (hash-ref *label-table* (cadr item))))

           ;; Input
           ((equal? (car item) 'input)
            (my-input (cdr item) 0)
            (line-determiner program (add1 linenumber)))
           ))
        )
  )

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line "startprogram" program 0))))

(main (vector->list (current-command-line-arguments)))
