#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.2 2015-09-23 17:11:09-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an
;;    SBIR program, which is the executed.  Currently it is only
;;    printed.
;;

(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (
           ( 
             (dirpath basepath root?)
             (split-path (find-system-path 'run-file))
           ) 
         )
        (path->string basepath)
    )
)
;;define hash table
(define *temp-table* (make-hash))
(define (temp-get key)
        (hash-ref *temp-table* key '(no such key)))
(define (temp-put! key value)
        (hash-set! *temp-table* key value))

(define *function-table* (make-hash))
(define (function-get key)
        (hash-ref *function-table* key '(no such key)))
(define (function-put! key value)
        (hash-set! *function-table* key value))

(define *lable-table* (make-hash))
(define (lable-get key)
        (hash-ref *lable-table* key))
(define (lable-put! key value)
        (hash-set! *lable-table* key value))

(define *variable-table* (make-hash))
(define (variable-get key)
        (hash-ref *variable-table* key))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))

(for-each
    (lambda (pair)
            (function-put! (car pair) (cadr pair)))
    `(

        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (/       ,(lambda (x y) (floor (/ x y))))
        (*       ,(lambda (x y) (* x y)))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (%       ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,(lambda (x y) (+ x y)))
        (-       ,(lambda (x y) (- x y)))
        (^       ,(lambda (x y) (expt x y)))
        (ceil    ,ceiling)
        (exp     ,exp)
        (floor   ,floor)
        (log     ,(lambda(x)(log x)))
        (sqrt    ,sqrt)

     ))
;;;


;do arithmetic
(define  (func-traverse tok)
    (define (funcc-trav list op arg1 arg2)
       (if  (or (number? arg1) (number? arg2))  
            ( (function-get op) arg1 arg2 )
            (begin
             (func-traverse (cadr list))
             (func-traverse (caddr list))
            )
       ) 
    ) 
    (funcc-trav tok (car  tok) (cadr tok) (caddr tok))
)

;implement print statement
(define (print-traverse tok)
   (if (not (null?  tok) )
     (begin
          (if (string? (car tok))  
            (display (car tok))
            (display (func-traverse (car tok))); change to func-traverse
          )
          (print-traverse (cdr tok))
      )
      (void)
   )
)

;parse statements
(define (func functok arg)
  
    (case functok 
      ((print)  (if (not (null? arg))(print-traverse arg)(void)))
      (else (display 'undefine))
    )
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
                         program))
    )
)

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n")
    (printf "-----------------------End------------------------~n"))

(define (put-in-hash list)
        (when (not (null? list))
              (let ((first (caar list)))
                   (when (number? first)
                         (if (not (null? (cdar list)))
                             (if(not (symbol? (cadar list)))
                                
                                (begin  
                                   (display first)
                                   (display " ")
                                   (display (caadar list))
                                   (display " ")
                                   (func (caadar list)(cdadar list))                             
                                   (newline)
                                )
                                (begin
                                   (display first)
                                   (display " ")
                                   (display (cadar list))
                                   (if (not (null? (cddar list)))
                                      (begin
                                        (display " ")
                                        (display (car (caddar list)))
                                        (display " ")
                                        (func (car (caddar list))(cdr (caddar list)))                             
                                        (newline)
                                      )
                                      (newline)
                                   )
                                )
                             )
                             (void)
                             
                          )
                    )
              )
              (put-in-hash (cdr list))))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* (
                (sbprogfile (car arglist))
                (program (readlist-from-inputfile sbprogfile))
              )
              (begin
                 (write-program-by-line sbprogfile program)
                 (put-in-hash program)
              )
        )
    )
)

(main (vector->list (current-command-line-arguments))
)

