#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;;
;; bcbaird
;; Brendan Baird
;; 10/17/17
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed. Lines of the file are stored and
;;    interpreted using hash tables in Scheme.
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

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define *function-table* (make-hash))
(define *label-table* (make-hash))
(define *variable-table* (make-hash))

(define (function-get key)
	(hash-ref *function-table* key))
(define (function-put! key value)
	(hash-set! *function-table* key value))
(define (function-has-key? key)
  (hash-has-key? *function-table* key))

(define (label-get key)
	(hash-ref *label-table* key))
(define (label-put! key value)
	(hash-set! *label-table* key value))

(define (variable-get key)
	(hash-ref *variable-table* key))
(define (variable-put! key value)
	(hash-set! *variable-table* key value))
(define (variable-has-key? key)
  (hash-has-key? *variable-table* key))

(for-each
	(lambda (pair)
		(function-put! (car pair) (cadr pair)))
    `(
    	(<>		,(lambda (x) (not x)))
    	(=		,(lambda (x y) (equal? x y)))
    	(<		,(lambda (x y) (< x y)))
    	(>		,(lambda (x y) (> x y)))
    	(>=		,(lambda (x y) (>= x y)))
    	(<=		,(lambda (x y) (<= x y)))
    	(*		,(lambda (x y) (* (evalexpr x) (evalexpr y))))
    	(/		,(lambda (x y) (/ (evalexpr x) (+ (evalexpr y) 0.0))))
    	(%		,(lambda (x y) (modulo (evalexpr x) (evalexpr y))))
    	(^		,(lambda (x y) (expt (evalexpr x) (evalexpr y))))
    	(+		,(lambda (x y) (+ (evalexpr x) (evalexpr y))))
    	(-		,(lambda (x y) (- (evalexpr x) (evalexpr y))))
        (abs		,(lambda (x) (abs (evalexpr x))))
        (acos		,(lambda (x) (acos (evalexpr x))))
        (asin		,(lambda (x) (asin (evalexpr x))))
        (atan		,(lambda (x) (atan (evalexpr x))))
        (ceil		,(lambda (x) (ceiling (evalexpr x))))
        (cos		,(lambda (x) (cos (evalexpr x))))
        (exp		,(lambda (x) (exp (evalexpr x))))
        (floor		,(lambda (x) (floor (evalexpr x))))
        (log		,(lambda (x) (log (evalexpr x))))
        (log10		,(lambda (x) (log (evalexpr x) 10)))
        (log2		,(lambda (x) (log (evalexpr x) 2)))
        (round		,(lambda (x) (round (evalexpr x))))
        (sin		,(lambda (x) (sin (evalexpr x))))
        (sqrt		,(lambda (x) (sqrt (evalexpr x))))
        (tan		,(lambda (x) (tan (evalexpr x))))
        (trunc		,(lambda (x) (truncate (evalexpr x))))
        (print  ,(lambda (x) (display (evalexpr x)) (newline)))
        (if     ,(lambda (x y) (when (evalexpr x) (read-prog (label-get y)))))
        (dim    ,(lambda (x) (variable-put! (car x) 
        			(make-vector (cadr x)))))
        (let	,(lambda (x y) (cond ((symbol? x) (variable-put! x (evalexpr y)))
                                  ((pair? x) (let ([vec (variable-get (car x))])
                                               (vector-set! vec (evalexpr (cadr x)) (evalexpr y))
                                               (variable-put! (car x) vec))))))
        (goto	,(lambda (x) (read-prog (label-get x))))
        (input  ,(lambda (x) (if (pair? x) (let ([vec (variable-get (car x))])
                                               (vector-set! vec (evalexpr (cadr x))
                                                            (read (current-input-port))))
                                 (variable-put! x (read (current-input-port))))))
    ))

(define (make-labels prog)
  (when (pair? (cdr (car prog))) (label-put! (cadr (car prog)) prog))
  (when (not (null? (cdr prog))) (make-labels (cdr prog))))

(define (read-prog prog)
  (if (pair? (cdr (car prog))) (if (not (null? (cdr (cdr (car prog)))))
      (evalexpr (cadr (cdr (car prog))))
      (evalexpr (car (cdr (car prog)))))
  (when (not (null? (cdr (car prog)))) (evalexpr (cadr (car prog)))))
  (if (null? (cdr prog)) (exit) (read-prog (cdr prog))))

;;
;; Evaluates if expression is number, variable, or function
;;
(define (evalexpr expr)
	(cond ((number? expr) expr)
		((and (symbol? expr) (variable-has-key? expr)) (variable-get expr))
                ((and (pair? expr) (variable-has-key? (car expr)))
                 (vector-ref (variable-get (car expr)) (cadr expr)))
		((and (pair? expr) (function-has-key? (car expr)))
                 (apply (function-get (car expr)) (cdr expr))) ;*function-table*
                (not(null? expr) expr)
		(else #f)))

(define (thruprog n)
  (when (not(null? (label-get n))) (evalexpr (car (label-get n))))
  (when (< n (variable-get '(labelcount))) (thruprog (+ n 1))))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (make-labels program)
              (variable-put! 'pi 3.141592653589793238462643383279502884197169399)
              (variable-put! 'e 2.718281828459045235360287471352662497757247093)
              (read-prog program)
)))

(main (vector->list (current-command-line-arguments)))