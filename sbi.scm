#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.2 2015-09-23 17:11:09-07 - - $
;; Jinyan Li jli134@ucsc.edu
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
(define *function-table* (make-hash))
(define (function-get key)
        (hash-ref *function-table* key '(no such key in
                                         function-table)))
(define (function-put! key value)
        (hash-set! *function-table* key value))

(define *lable-table* (make-hash))
(define (lable-get key)
        (hash-ref *lable-table* key '()))
(define (lable-put! key value)
        (hash-set! *lable-table* key value ))

(define *variable-table* (make-hash))
(define (variable-get key)
        (hash-ref *variable-table* key '(no such key in 
                                          variable-table)))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))


(define (plus . arg-list)
     (if (< 1 (len arg-list))
      (+ (car arg-list) (cadr arg-list))
      (car arg-list)
     )
)

(define (minus . arg-list)
     (if (< 1 (len arg-list))
      (- (car arg-list) (cadr arg-list))
      (- 0 (car arg-list))
     )
)

(for-each
    (lambda (pair)
            (variable-put! (car pair) (cadr pair)))
    `(
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
       
     ))

;;;

;;get the length of a list
(define len (lambda (l)
         (define len.. (lambda (l.. n)
             (if (null? l..)n
               (len.. (cdr l..) (+ n 1)))))        
         (len.. l 0)))

;do arithmetic
(define (value l)

      (if (pair? l) 
       (apply (function-get (car l)) (map value (cdr l)))      
       (cond ((number? l) l)  
             
             (else (variable-get l))
       )    
      )
)

;implement print statement
(define (print-traverse tok)
   (if (not (null?  tok) )
     (begin
          (if (string? (car tok))  
            (display (car tok))
            (display (value (car tok)))      
          )         
          (print-traverse (cdr tok))
      )
         (newline)
   )
)
;implement let statement
(define (letfunc arg)
;  (display (car arg))
 
;   (display (cadr arg))
;   (newline)
  (if (pair? (car arg))
    (begin
;      (display "yyyyyyy")(newline)
;      (display (caar arg))(newline)
;      (display (variable-get (caar arg)))(newline)
      (vector-set! (variable-get
                       (caar arg)) (value (cadar arg)) (cadr arg))
;       (display "zzzzzzzzzz")(newline)
;       (display (variable-get (caar arg)))(newline)
;       (display ((function-get (caar arg)) 6))(newline)
;       (display "xxxxx")(newline)

    )
    (begin
     (variable-put! (car arg) (value (cadr arg)) )  

    )
  )
)

;implement dim statement
(define (dimfunc arg)
;  (display (caar arg))
;   (display (cdar arg))
;   (newline)
  (variable-put! (caar arg) (make-vector (value (cadar arg))) )
;  (display (variable-get (caar arg)))(newline)
  (function-put! (caar arg)
      (lambda(x) (vector-ref (variable-get (caar arg)) x))
   )
)
;implement goto statement
(define (gotofunc lable program)
   (eval-line program (lable-get (car lable)))

)


(for-each
    (lambda (pair)
            (function-put! (car pair) (cadr pair)))
    `(

        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (/       ,(lambda (x y) (floor (/ x (if (equal? y 0) 0.0 y)))))
        (*       ,(lambda (x y) (* x y)))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (%       ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,plus)
        (-       ,minus)
        (^       ,(lambda (x y) (expt x y)))
        (ceil    ,(lambda(x)(ceiling x)))
        (exp     ,(lambda(x) (expt (variable-get 'e) x)))
        (floor   ,(lambda(x)(floor x)))
        (log     ,(lambda(x)(log (if (equal? x 0) 0.0 x))))
        (sqrt    ,(lambda(x)(sqrt x)))
        (floor    ,(lambda(x)(floor x)))
        (sin    ,(lambda(x)(sin x)))
        (cos    ,(lambda(x)(cos x)))
        (round    ,(lambda(x)(round x)))
        (tan    ,(lambda(x)(tan x)))
        (acos    ,(lambda(x)(acos x)))
        (asin    ,(lambda(x)(asin x)))
        (abs    ,(lambda(x)(abs x)))
        (atan    ,(lambda(x)(atan (if (equal? x 0) 0.0 x))))
        (print   ,print-traverse)
        (let     ,letfunc)
        (dim     ,dimfunc)
        (goto    ,gotofunc)
     ))

;parse statements
(define (func functok program line-nr)
  ;(display (car functok))(display (cdr functok))
  (when (not (hash-has-key? *function-table* (car functok )))
        (die "~s statement doesn't exit" (car functok)))
  (cond
    ((eqv? (car functok) 'goto)
;      (display (cadr functok))(display " ")
;      (display (lable-get (cadr functok)))(newline)
      (eval-line program (- (lable-get (cadr functok)) 1))
    ) 
    (else 
      ((function-get (car functok)) (cdr functok))
      (eval-line program (+ line-nr 1))
    )
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



;;put lables and their statements into hash table
(define (get-all-lable list)
(when (not (null? list))
              (let ((first (caar list)))
                   (when (number? first)
                         (if (not (null? (cdar list)))
                             (if(not (symbol? (cadar list)))
                                (void)
                                (begin

                                (lable-put! (cadar list) (caar list))
                                ;(display (cadar list))(display " ")
                                ;(display (lable-get (cadar list)))(newline)
                                )
                             )
                             (void)  
                          )
                    )
              )
              (get-all-lable (cdr list))))

(define (eval-line program line-nr)
        (when (> (len program) line-nr)
;        (display "line number: ")(display line-nr)(newline)
;         (display "pro len ")(display (len program))(display " ")(display line-nr)(newline)
          (let ((line (list-ref program line-nr)))
;               (display "line len ")(display (len line))(newline)
;               (display line)(newline)
             (cond
               ((= (len line) 3) 
                 ;(display "(cddr line)")(display (cddr line))(newline)
                 (set! line (cddr line))
;                 (display "XXXXXXXXXXXXXXXXx")(newline)
                 ;(display "(car line)")(display (car line))(newline)
                 (func (car line) program line-nr)
               )
               ((and (= (len line) 2) (list? (cadr line)))
                 ;(display "yyyyyyyyyyyy")(newline)
               ;(display "(cdr line)")(display (cdr line))(newline)
               (set! line (cdr line))
               ;(display "(car line)")(display (car line))(newline)
               (func (car line) program line-nr)
              )           
               (else
                  (eval-line program (+ line-nr 1)))
             ))))

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* (
                (sbprogfile (car arglist))
                (program (readlist-from-inputfile sbprogfile))
              )
              (begin
                 (write-program-by-line sbprogfile program)
                 (get-all-lable program)
                 (eval-line program 0)
              )
        )
    )
)

(main (vector->list (current-command-line-arguments))
)

