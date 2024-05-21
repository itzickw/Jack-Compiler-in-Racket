#|
 Name 1: Itzik Walles   ID: 021996954
 Name 2: Yair Evron     ID: 204742878    
; Group num.  42
|#

#lang racket

; (define (equal)
;   (displayln (string-append "\n// equal operation") output-asm-file)
;   (pop-to-D) ; Pop y (second value) from the stack
;   (displayln "D=M-D" output-asm-file) ; Compute x - y and store result in D

;   ; Generate unique labels for true and false conditions
;   (define true-label (gen-is-true-label))
;   (define false-label (gen-is-false-label))

;   ; If x - y is zero (i.e., x equals y), jump to true label
;   (displayln (string-append "@" true-label) output-asm-file)
;   (displayln "D;JEQ" output-asm-file)

;   ; Push false (0) onto the stack
;   (push-constant "0")

;   ; Skip to the false label
;   (displayln (string-append "@" false-label) output-asm-file)
;   (displayln "0;JMP" output-asm-file)

;   ; True label: push true (-1) onto the stack
;   (displayln (string-append "(" true-label ")") output-asm-file)
;   (push-constant "-1")

;   ; False label
;   (displayln (string-append "(" false-label ")") output-asm-file)
; )


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
; (define (stack-pointer-load)
;   (displayln "@SP\nA=M\n" output-asm-file)
; )


;pops the top value from the stack and stores it in the D register.
(define (pop-to-D)
  (displayln "@SP\nA=M-1\nD=M\nA=A-1" output-asm-file)
) 

(define (handle-line split-line)
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

(define (push-constant num)
    (displayln (string-append "@" num) output-asm-file)
    (displayln "D=A\n@SP\nA=M\nM=D\n@SP\nM=M+1" output-asm-file)
)

(define (add)    
    (pop-to-D)
    (displayln "M=M+D\nD=A+1\n@SP\nM=D" output-asm-file)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (get-folder-name folder-path)
  (define folder-path-parts (string-split folder-path "\\"))
  (define folder-name (last folder-path-parts))
  folder-name
)


(define (open-asm-file name)
  (define file-path (string-append folder-path "/" (string-append name ".asm")))
  (define output-asm-file (open-output-file file-path #:exists 'replace))
  output-asm-file
)


(define (isVm file)
  (define file-name (path->string file))
  (string-suffix? file-name ".vm")
)


(define (drop-extension name)
  (define str-name (path->string name))
  (define parts (string-split str-name "."))
  (define temp1 (drop-right parts 1))
  (define temp2 (string-join temp1 "."))
  temp2
)


(define (read-file file-path)

  (define input-port (open-input-file file-path))

  (let loop ()
    (let ((line (read-line input-port)))
        (displayln line)
        (cond
            [
                (eof-object? line) ; Check for end-of-file
                (close-input-port input-port)
            ]
            [else
                (set! line(string-split(string-trim line)))
                (when (not (empty? line)) (handle-line line))
                (loop)
            ]
        )
    )
  )
)


(define (get-folder-path)
    (define user-input(string-trim (read-line)))
    (let loop()
        (when (not (directory-exists? user-input))
            (displayln "The folder path is incorrect, plesae enter again")
            (set! user-input(string-trim (read-line)))
            (loop)
        )
    )
    user-input
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;          main           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(displayln "Enter the folder path please")
(define folder-path(get-folder-path))

(define file-list (directory-list folder-path))

(define folder-name(get-folder-name folder-path))
(define output-asm-file(open-asm-file folder-name))


(for ([file file-list])
  (when (isVm file) 

      (define file-name (drop-extension file))
      (displayln (string-append "// " file-name " file") output-asm-file)
      (define file-path (string-append folder-path "\\" (path->string file)))
      (read-file file-path)
      (displayln  "" output-asm-file)
  )
)
