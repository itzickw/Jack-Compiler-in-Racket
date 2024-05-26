#lang racket

(provide handle-line)

(define output-asm-file "")
(define is-false-count 0)
(define is-true-count 0)


;     ============================== handling funcs ====================================


(define (handle-line split-line output-file file-name)
    (set! output-asm-file output-file)
  
    (define operation (first split-line))
    
    (when (string=? operation "push") (push-handling split-line file-name))
    (when (string=? operation "pop")  (pop-handling split-line file-name))

    (when (string=? operation "add")(math operation "+"))
    (when (string=? operation "sub")(math operation "-"))
    (when (string=? operation "and")(math operation "&"))
    (when (string=? operation "or" )(math operation "|"))

    (when (string=? operation "neg")(unary operation "-"))
    (when (string=? operation "not")(unary operation "!"))
    
    (when (string=? operation "eq") (compare operation))
    (when (string=? operation "gt") (compare operation))
    (when (string=? operation "lt") (compare operation))
)

(define (push-handling split-line file-name)
  (define operation (second split-line))
  (displayln (string-append "\n//push " operation " " (third split-line)) output-asm-file)

  (when (string=? operation "constant") (push-constant (third split-line)))
  (when (string=? operation "local")    (push-through-pointer "LCL" (third split-line)))
  (when (string=? operation "argument") (push-through-pointer "ARG" (third split-line)))
  (when (string=? operation "this")     (push-through-pointer "THIS" (third split-line)))
  (when (string=? operation "that")     (push-through-pointer "THAT" (third split-line)))
  (when (string=? operation "temp")     (push-from-memory "5" (third split-line)))
  (when (string=? operation "pointer")  
    (if (string=? "0" (third split-line)) (push-from-memory "3" "0")(push-from-memory "4" "0"))
  )
  (when (string=? operation "static")    (push-from-static file-name (third split-line)))
)

(define (pop-handling split-line file-name)
  (define operation (second split-line))
  (displayln (string-append "\n//pop " operation " " (third split-line)) output-asm-file)

  (when (string=? operation "local")    (pop-through-pointer "LCL" (third split-line)))
  (when (string=? operation "argument") (pop-through-pointer "ARG" (third split-line)))
  (when (string=? operation "this")     (pop-through-pointer "THIS" (third split-line)))
  (when (string=? operation "that")     (pop-through-pointer "THAT" (third split-line)))
  (when (string=? operation "temp")     (pop-to-memory "5" (third split-line)))
  (when (string=? operation "pointer") 
    (if (string=? "0" (third split-line))(pop-to-memory "3" "0")(pop-to-memory "4" "0"))
  )
  (when (string=? operation "static")    (pop-to-static file-name (third split-line)))
)


;       ============================= operation funcs ==================================


(define (math operation operator)
  (displayln (string-append "\n//" operation " operation") output-asm-file)
  (pop-to-D)
  (displayln (string-append "M=M" operator "D\n@SP\nM=M-1") output-asm-file)
)

(define (push-constant num)
    (displayln (string-append "@" num) output-asm-file)
    (displayln "D=A\n@SP\nA=M\nM=D\n@SP\nM=M+1" output-asm-file)
)

(define (unary operation operator)
  (displayln (string-append "\n//" operation " operation") output-asm-file)
  (displayln (string-append "@SP\nA=M-1\nM=" operator "M") output-asm-file)
)

(define (compare operation)
  (define true-label "")
  (displayln (string-append "\n//" operation " operation") output-asm-file)

  (pop-to-D)
  ;equal(x = y), D = x-y
  (displayln "D=M-D" output-asm-file)
  
  (when (string=? operation "eq") (set! true-label (jump-if-true "D;JEQ")))
  (when (string=? operation "gt") (set! true-label (jump-if-true "D;JGT")))
  (when (string=? operation "lt") (set! true-label (jump-if-true "D;JLT")))

  ;"0" = false. (for true goto IS_TRUEi)
  (displayln "D=0\n@SP\nA=M-1\nA=A-1\nM=D" output-asm-file)
  ;skip on IS_TRUEi block
  (define false-label (jump-if-false "0;JMP"))
  (displayln (string-append "(" true-label ")") output-asm-file)
  (displayln "D=-1\n@SP\nA=M-1\nA=A-1\nM=D" output-asm-file)
  (displayln (string-append "(" false-label ")") output-asm-file)

  (displayln "@SP\nM=M-1" output-asm-file)
)

(define (push-through-pointer base my-index)
  (displayln (string-append "@" base) output-asm-file)
  (displayln "D=M" output-asm-file)
  (displayln (string-append "@" my-index) output-asm-file)
  (displayln "A=D+A\nD=M" output-asm-file)
  (displayln "@SP\nA=M\nM=D\n@SP\nM=M+1" output-asm-file)
)

(define (push-from-memory base my-index)
  (set! my-index(+ (string->number base) (string->number my-index)))
  (set! my-index (number->string my-index))
  (displayln (string-append "@"my-index) output-asm-file)
  (displayln "D=M\n@SP\nA=M\nM=D\n@SP\nM=M+1" output-asm-file)
)

(define (push-from-static file-name num)
  (displayln (string-append "@"file-name"."num) output-asm-file)
  (displayln "D=M" output-asm-file)
  (displayln "@SP\nA=M\nM=D\n@SP\nM=M+1" output-asm-file)
)

(define (pop-through-pointer base my-index)
  (displayln "@SP\nA=M-1\nD=M\n@SP\nM=M-1" output-asm-file)
  (displayln (string-append "@" base "\nA=M") output-asm-file)
  
  (set! my-index (string->number my-index))
  (increase-A-my-index-times my-index)

  (displayln "M=D" output-asm-file)
)

(define (pop-to-memory base my-index)
  (set! my-index(+ (string->number base) (string->number my-index)))
  (set! my-index (number->string my-index))
  (displayln "@SP\nA=M-1\nD=M\n@SP\nM=M-1" output-asm-file)
  (displayln (string-append "@"my-index) output-asm-file)
  (displayln "M=D" output-asm-file)
)

(define (pop-to-static file-name num)
  (displayln "@SP\nA=M-1\nD=M\n@SP\nM=M-1" output-asm-file)
  (displayln (string-append "@"file-name"."num) output-asm-file)
  (displayln "M=D" output-asm-file)
)

;            ============================  auxiliary funcs =========================


(define (increase-A-my-index-times my-index)
  (let loop ()
    (when
      (> my-index 0)
      (displayln "A=A+1" output-asm-file)
      (set! my-index (- my-index 1))
      (loop)
    )
  )
)

(define (gen-is-true-label)
  (define is-true-label (string-append "IS_TRUE" (number->string is-true-count)))
  (set! is-true-count (+ is-true-count 1))
  is-true-label
)

(define (gen-is-false-label)
  (define is-false-label (string-append "IS_FALSE" (number->string is-false-count)))
  (set! is-false-count (+ is-false-count 1))
  is-false-label
)

(define (jump-if-true comp-cond)
  (define true-label (gen-is-true-label))
  (displayln (string-append "@" true-label) output-asm-file)
  (displayln comp-cond output-asm-file)
  true-label
)

(define (jump-if-false comp-cond)
  (define false-label (gen-is-false-label))
  (displayln (string-append "@" false-label) output-asm-file)
  (displayln comp-cond output-asm-file)
  false-label
)

;pops the top value from the stack into D register, and than A register = second value from the stack
(define (pop-to-D)
  (displayln "@SP\nA=M-1\nD=M\nA=A-1" output-asm-file)
) 
