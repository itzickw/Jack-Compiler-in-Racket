#lang racket

(provide handle-line)

(define file-name "")
(define output-asm-file "")
(define is-false-count 0)
(define is-true-count 0)


;     ============================== handling line ====================================


(define (handle-line split-line output-file arg-file-name)
    (set! output-asm-file output-file)
    (set! file-name arg-file-name)
    
    (define operation (first split-line))
    
    (when (string=? operation "push") (push-handling split-line))
    (when (string=? operation "pop")  (pop-handling split-line))

    (when (string=? operation "add")(math operation "+"))
    (when (string=? operation "sub")(math operation "-"))
    (when (string=? operation "and")(math operation "&"))
    (when (string=? operation "or" )(math operation "|"))

    (when (string=? operation "neg")(unary operation "-"))
    (when (string=? operation "not")(unary operation "!"))
    
    (when (string=? operation "eq") (compare operation))
    (when (string=? operation "gt") (compare operation))
    (when (string=? operation "lt") (compare operation))

    (when (string=? operation "label") (create-label (second split-line)))
    (when (string=? operation "goto") (goto (second split-line)))
    (when (string=? operation "if-goto") (if-goto (second split-line)))

    (when (string=? operation "function") (func-hendling (second split-line) (third split-line)))
    (when (string=? operation "call") (func-call (second split-line) (third split-line)))
    (when (string=? operation "return") (func-return))
)

(define (push-handling split-line)
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
  
  (when (string=? operation "static")    (push-from-static (third split-line)))
)

(define (pop-handling split-line)
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

  (when (string=? operation "static")    (pop-to-static (third split-line)))
)


;       ============================= operation funcs ==================================


(define (math operation operator)
  (displayln (string-append "\n//" operation " operation") output-asm-file)
  
  (stack-to-D-and-A-1)
  
  (displayln (string-append "M=M"operator"D\n@SP\nM=M-1") output-asm-file)
)

(define (push-constant num)
    (insert-D num)
    ;(insert-A num)
    ;(displayln (string-append "@" num) output-asm-file)
    (displayln "@SP\nA=M\nM=D\n@SP\nM=M+1" output-asm-file)
)

(define (unary operation operator)
  (displayln (string-append "\n//" operation " operation") output-asm-file)
  (displayln (string-append "@SP\nA=M-1\nM=" operator "M") output-asm-file)
)

(define (compare operation)
  (define true-label empty)
  
  (displayln (string-append "\n//" operation " operation") output-asm-file)

  (stack-to-D-and-A-1)

  ;equal(x = y), D = x-y
  (displayln "D=M-D" output-asm-file)
  
  (when (string=? operation "eq") (set! true-label (jump-if-true "D;JEQ")))
  (when (string=? operation "gt") (set! true-label (jump-if-true "D;JGT")))
  (when (string=? operation "lt") (set! true-label (jump-if-true "D;JLT")))

  ;"0" = false. (for true goto IS_TRUE_i)
  (compersion-result-update "0")

  ;skip on IS_TRUEi block
  (define false-label (jump-if-false "0;JMP"))

  (displayln (string-append "(" true-label ")") output-asm-file)
  (compersion-result-update "-1")
  (displayln (string-append "(" false-label ")") output-asm-file)

  (displayln "@SP\nM=M-1" output-asm-file)
)

(define (push-through-pointer base my-index)
  (insert-A base)
  (displayln "D=M" output-asm-file)
  (insert-A my-index)
  (displayln "A=D+A" output-asm-file)
  (push-M)
)

(define (push-from-memory base my-index)
  (set! my-index(+ (string->number base) (string->number my-index)))
  (set! my-index (number->string my-index))
  (insert-A my-index)
  (push-M)
)

(define (push-from-static num)
  (insert-A (string-append file-name"."num))
  (push-M)
)

(define (pop-through-pointer base my-index)
  (pop-to-D)
  (insert-A base)
  (displayln "A=M" output-asm-file)
  
  (set! my-index (string->number my-index))
  (increase-A-my-index-times my-index)

  (displayln "M=D" output-asm-file)
)

(define (pop-to-memory base my-index)
  (set! my-index(+ (string->number base) (string->number my-index)))
  (set! my-index (number->string my-index))
  (pop-to-D)
  (insert-A my-index)
  (displayln "M=D" output-asm-file)
)

(define (pop-to-static num)
  (pop-to-D)
  (insert-A (string-append file-name"."num))
  (displayln "M=D" output-asm-file)
)

(define (create-label label)
  (displayln "\n//label" output-asm-file)
  (displayln (string-append "("label")") output-asm-file)
)

(define (goto label)
  (displayln "\n//goto" output-asm-file) 
  (insert-A label)
  (displayln "0;JMP" output-asm-file)
)

(define (if-goto label)
  (displayln "\n//if-goto" output-asm-file)
  (pop-to-D)
  (insert-A label)
  (displayln "D;JNE" output-asm-file)
)


;            ============================     func hendling    =========================


(define (func-hendling func-name num-of-lcl)
  (displayln (string-append "\n//function " func-name) output-asm-file)

  (create-label func-name)

;if D=0 jump to the end of LCL initializing
  (insert-A num-of-lcl)
  (displayln "D=A" output-asm-file)
  (insert-A (string-append func-name".end"))
  (displayln "D;JEQ" output-asm-file)

;while D != 0, initializing next LCL
  (create-label (string-append func-name".loop"))
  (displayln "@SP\nA=M\nM=0\n@SP\nM=M+1" output-asm-file)
  (insert-A (string-append func-name".loop"))
  (displayln "D=D-1;JNE" output-asm-file)
  (create-label (string-append func-name".end"))
)

(define (func-call func-name num-of-arg)
  (displayln (string-append "\n//calling of " func-name) output-asm-file)

  (when (string=? func-name "Sys.init") (file-initialization))

  (define return-label (create-return-label))
  
  (push-constant return-label)
  (mem-ptr-push "LCL")
  (mem-ptr-push "ARG")
  (mem-ptr-push "THIS")
  (mem-ptr-push "THAT")


  (displayln "@SP\nD=M\n@5\nD=D-A" output-asm-file)
  (insert-A num-of-arg)
  (displayln "D=D-A\n@ARG\nM=D" output-asm-file)

  (displayln "@SP\nD=M\n@LCL\nM=D" output-asm-file)

  (goto func-name)

  (create-label return-label)
)

(define (func-return)
  (operation-title "function return")

  ;taking return address
  (insert-A "LCL")
  (displayln "D=M" output-asm-file)
  (insert-A "5")
  (displayln "A=D-A\nD=M" output-asm-file)
  
  ;RAM[13] = return address
  (insert-A "13")
  (displayln "M=D" output-asm-file)

  ;return value to ARG 0 that will be the stack head
  (pop-to-D)
  (insert-A "ARG")
  (displayln "A=M\nM=D" output-asm-file)

  ;update SP
  (insert-A "ARG")
  (displayln "D=M+1\n@SP\nM=D" output-asm-file)

  ;memory pointers recovering
  (mem-ptr-recover "THAT")
  (mem-ptr-recover "THIS")
  (mem-ptr-recover "ARG")
  (mem-ptr-recover "LCL")

  ;goto return address
  (insert-A "13")
  (displayln "A=M\n0;JMP" output-asm-file)
)

(define (file-initialization)
  (insert-D "256")
  (displayln "@SP\nM=D" output-asm-file)
)


;            ============================  auxiliary funcs =========================

 
(define (pop-to-D)
 (displayln "@SP\nM=M-1\nA=M\nD=M" output-asm-file)
)

(define (push-M)
  (displayln "D=M\n@SP\nM=M+1\nA=M-1\nM=D" output-asm-file)
)

(define (mem-ptr-push ptr)
  (insert-A ptr)
  (push-M)
)

(define (insert-A value)
  (displayln (string-append "@"value) output-asm-file)
)

(define (insert-D value)
  (insert-A value)
  (displayln "D=A" output-asm-file)
)

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
  (define is-true-label (string-append "IS_TRUE_" (number->string is-true-count)))
  (set! is-true-count (+ is-true-count 1))
  is-true-label
)

(define (gen-is-false-label)
  (define is-false-label (string-append "IS_FALSE_" (number->string is-false-count)))
  (set! is-false-count (+ is-false-count 1))
  is-false-label
)

(define (jump-if-true comp-cond)
  (define true-label (gen-is-true-label))
  (insert-A true-label)
  (displayln comp-cond output-asm-file)
  true-label
)

(define (jump-if-false comp-cond)
  (define false-label (gen-is-false-label))
  (insert-A false-label)
  (displayln comp-cond output-asm-file)
  false-label
)

;pops the top value from the stack into D register, and than A register = second value from the stack
(define (stack-to-D-and-A-1)
  (displayln "@SP\nA=M-1\nD=M\nA=A-1" output-asm-file)
) 

(define (operation-title operation)
  (displayln (string-append "\n//" operation) output-asm-file)
)

(define (mem-ptr-recover mem)
  (insert-A "LCL")
  (displayln "M=M-1\nA=M\nD=M" output-asm-file)
  (insert-A mem)
  (displayln "M=D" output-asm-file)
)

(define (create-return-label)
  (define return-count-file "return-count.txt")
  (define return-count (read-return-count return-count-file))
  
  ; Increment the return count
  (set! return-count (+ return-count 1))
  
  ; Write the updated count back to the file
  (write-return-count return-count-file return-count)
  
  ; Generate and return the new return label
  (generate-return-label return-count)
)

(define (read-return-count file)
  (if (file-exists? file)
      (let ((input-port (open-input-file file)))
        (define count (string->number (read-line input-port)))
        (close-input-port input-port)
        count)
      0); Initialize count to 0 if the file doesn't exist
) 

(define (write-return-count file count)
  (let ((output-port (open-output-file file #:exists 'replace)))
    (displayln (number->string count) output-port)
    (close-output-port output-port))
)

(define (generate-return-label count)
  (string-append "RETURN_" (number->string count))
)

( define (compersion-result-update result)
  (displayln (string-append "D="result"\n@SP\nA=M-1\nA=A-1\nM=D") output-asm-file)
)

