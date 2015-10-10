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

;define *stderr*
(define *stderr* (current-error-port))

;Function "*run-file*" find the filename
(define *run-file*
    (let-values
        (( 
             (dirpath basepath root?)
             (split-path (find-system-path 'run-file))))
        (path->string basepath)))
;;define function hash table
(define *function-table* (make-hash))

;;function "function-get" gets value of the key
(define (function-get key)
        (hash-ref *function-table* key '(no such key in
                                         function-table)))

;;function "function-get" gets value of the key
(define (function-put! key value)
        (hash-set! *function-table* key value))

;;define lable hash table
(define *lable-table* (make-hash))

;;function "lable-get" gets value of the key
;takes key output value
(define (lable-get key)
        (hash-ref *lable-table* key '()))

;;function "lable-put" update key's value
;input key 
(define (lable-put! key value)
        (hash-set! *lable-table* key value ))

;;define variable hash table
(define *variable-table* (make-hash))

;;function "variable-get" gets value of the key
;takes key output value
(define (variable-get key)
        (hash-ref *variable-table* key '(no such key in 
                                          variable-table)))
;;function "variable-put"  update key's value
;takes key and value
(define (variable-put! key value)
        (hash-set! *variable-table* key value))

;function "plus" does plus operation and return value
;it take one or two oprand output numeric value
(define (plus . arg-list)
     (if (< 1 (len arg-list))
      (+ (car arg-list) (cadr arg-list))
      (car arg-list)
     )
)

;function "minus" does substraction operation and return value
;it take one or two oprand output numeric value
(define (minus . arg-list)
     (if (< 1 (len arg-list))
      (- (car arg-list) (cadr arg-list))
      (- 0 (car arg-list))
     )
)

;initialize variable table
(for-each
    (lambda (pair)
            (variable-put! (car pair) (cadr pair)))
    `(
        (e       2.718281828459045235360287471352662497757247093)
        (pi      3.141592653589793238462643383279502884197169399)
        (inputcount 0)
     ))

;;function "len" input: list output: the length of a list

(define len (lambda (l)
         (define len.. (lambda (l.. n)
             (if (null? l..)n
               (len.. (cdr l..) (+ n 1)))))        
         (len.. l 0)))

;function "value" input: expression output:value of the 
;expression 
;RECURSIVE
(define (value l)

      (if (pair? l) 
       (apply (function-get (car l)) (map value (cdr l)))      
       (cond ((number? l) l)  
             
             (else (variable-get l)))))

;function "print-traverse" input "string|expr" output:display
;RECURSIVE
(define (print-traverse tok) 
   (if (not (null?  tok) )
     (begin
          (if (string? (car tok))  
            (display (car tok))
            (display (value (car tok)))      
          )         
          (print-traverse (cdr tok))
      )
         (newline)))

;function "let" input "string|expr". It makes an assignmant to 
;a variable
(define (letfunc arg)
;   (newline)
  (if (pair? (car arg))
    (begin
;      (display "yyyyyyy")(newline)
      (vector-set! (variable-get
                (caar arg)) (- (value (cadar arg)) 1) (value (cadr arg)))
;       (display "zzzzzzzzzz")(newline)
;       (display (variable-get (caar arg)))(newline)
;       (display "xxxxx")(newline)
    )
    (begin
     (let ((result (value (cadr arg))))
       (variable-put! (car arg) result)
     ))))

;function "dimfunc" input (Varivale Expression)
;it create an array of given size
(define (dimfunc arg)
;  (display (caar arg))
;   (display (cdar arg))
;   (newline)
  (variable-put! (caar arg) (make-vector (value (cadar arg))) )
;  (display (variable-get (caar arg)))(newline)
  (function-put! (caar arg) 
      (lambda(x) (vector-ref (variable-get (caar arg)) (- x 1)))))


;function "gotofunc" implement "goto" statement
;input: lable, program
(define (gotofunc lable program)
   (exe-lines program (lable-get (car lable))))

;function "inputfunc" implement "input" statement
;input: variable or array    RECURSIVE
(define (inputfunc arg)
  (variable-put! 'inputcount 0)
  (define (get-input arg)
;(display (car arg))(newline)
     (when (not (null? (car arg)))
        (variable-put! (car arg) (void))
        (let ((object (read)))
;(display (value (car arg)))(display "debug")(newline)
             (cond [(eof-object? object)(variable-put! 'inputcount -1)]
                   [(number? object)(variable-put! (car arg) object)
                  (variable-put! 'inputcount (+ (variable-get 'inputcount) 1))]
                   [else (begin (printf "invalid number: ~a~n" object)
                                )] )) 
         
         (when (not (null? (cdr arg)))
     (get-input (cdr arg)))
   )
)
(get-input arg)
;(display (car arg))(display " <-key value-> ")
;(display (variable-get (car arg)))(newline)
)

(for-each
    (lambda (pair)
            (function-put! (car pair) (cadr pair)))
    `(

        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (/       ,(lambda (x y)  (/ x (if (equal? y 0) 0.0 y))))
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
        (input   ,inputfunc)
        (if      ,(void))
        (<=      ,(lambda (x y) (<= x y)))
        (>=      ,(lambda (x y) (>= x y)))
        (<      ,(lambda (x y) (< x y)))
        (>      ,(lambda (x y) (> x y)))
        (=      ,(lambda (x y) (eqv? x y)))
        (<>      ,(lambda (x y) (not (equal? x y))))
     ))

;parse statements
(define (func functok program line-nr)
;  (display (car functok))(display "  ")(display (cdr functok))(newline)
  (when (not (hash-has-key? *function-table* (car functok )))
        (display (car functok))(display " doesn't exit")(newline))
  (cond
    ((eqv? (car functok) 'goto)
;      (display (cadr functok))(display " ")
;      (display (lable-get (cadr functok)))(newline)
      (exe-lines program (- (lable-get (cadr functok)) 1))
    )
    ((eqv? (car functok) 'if)
;(display "if statement")(newline) 
;(display (variable-get 'inputcount))(newline)
;     (display (value (cadr functok)))(newline)
      (if (equal? #t (value (cadr functok)))
        (exe-lines program (- (lable-get (caddr functok)) 1))
        (exe-lines program (+ line-nr 1))
      )
    )  
    (else
;(display "XXXXXXXXXX")(newline) 
      ((function-get (car functok)) (cdr functok))
      (exe-lines program (+ line-nr 1))
    )))


(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1))

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename")))

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

;function
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
                                ))
                             (void))))
              (get-all-lable (cdr list))))
;RECURSIVE
(define (exe-lines program line-nr)
        (when (> (len program) line-nr)
;        (display "line number: ")(display line-nr)(newline)

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
                  (exe-lines program (+ line-nr 1)))
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
                 (exe-lines program 0)
              ))))

(main (vector->list (current-command-line-arguments)))

