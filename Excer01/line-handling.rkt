#lang racket

(provide handle-line)

(define output-asm-file "")

(define (handle-line split-line output-file)

    (displayln "a")
    (set! output-asm-file output-file)
    (displayln "b")
    ;(define split-line(string-split line " "))
    (define operation (first split-line))
    ;(displayln operation)
    (displayln (string-append "\n//" operation " operation") output-asm-file)
    
    (when (string=? operation "push")
        (when (string=? (second split-line) "constant") (push-constant (third split-line)))
        ;(push var (second split-line))
    )
    (when (string=? operation "add")(add))
    (when (string=? operation "sub")(sub))
    (when (string=? operation "neg")(neg))
    (when (string=? operation "and")(my-and))
    (when (string=? operation "or" )(my-or))
    (when (string=? operation "not")(my-not))
    (when (string=? operation "eq") (compare operation))
    (when (string=? operation "gt") (compare operation))
    (when (string=? operation "lt") (compare operation))
)

(define is-true-count 0)

(define (gen-is-true-label)
  (define is-true-label (string-append "IS_TRUE" (number->string is-true-count)))
  (set! is-true-count (+ is-true-count 1))
  is-true-label
)

(define is-false-count 0)

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

(define (push-constant num)
    (displayln (string-append "@" num) output-asm-file)
    (displayln "D=A\n@SP\nA=M\nM=D\n@SP\nM=M+1" output-asm-file)
)

(define (add)    
    (pop-to-D)
    (displayln "M=M+D\n@SP\nM=M-1" output-asm-file)
)

(define (sub)
    (pop-to-D)
    (displayln "M=M-D\n@SP\nM=M-1" output-asm-file)
)

(define (neg)
  (displayln "@SP\nA=M-1\nM=-M" output-asm-file)
)

(define (my-and)
  (pop-to-D)
  (displayln "M=M&D\n@SP\nM=M-1" output-asm-file)
)

(define (my-or)
  (pop-to-D)
  (displayln "M=M|D\n@SP\nM=M-1" output-asm-file)
)

(define (my-not)
  (displayln "@SP\nA=M-1\nM=!M" output-asm-file)
)

(define (compare operation)
  (define true-label "")

  (pop-to-D)
  ;equal(x = y), D = x-y
  (displayln "D=M-D" output-asm-file)
  

  (when (string=? operation "eq") (set! true-label (jump-if-true "D;JEQ")))
  (when (string=? operation "gt") (set! true-label (jump-if-true "D;JGT")))
  (when (string=? operation "lt") (set! true-label (jump-if-true "D;JLT")))

  ;moving the stack pointer -2
  (displayln "@SP\nM=M-2" output-asm-file)

  ;"0" = false. (for true goto IS_TRUEi)
  (push-constant "0")

  ;skip on IS_TRUEi block
  (define false-label (jump-if-false "0;JMP"))
  
  (displayln (string-append "(" true-label ")") output-asm-file)

  (push-constant "-1")
  
  (displayln (string-append "(" false-label ")") output-asm-file)
)