#|
 Name 1: Itzik Walles   ID: 021996954
 Name 2: Yair Evron     ID: 204742878    
; Group num.  42
|#

#lang racket

; (define (stack-pointer-load)
;   (displayln "@SP\nA=M\n" output-asm-file)
; )

(define (pop-to-D)
  (displayln "@SP\nA=M-1\nD=M" output-asm-file)
)

(define (handle-line split-line)
    ;(define split-line(string-split line " "))
    
    (define operation (first split-line))
    ;(displayln operation)

    (when (string=? operation "push")
        (when (string=? (second split-line) "constant") (push-constant (third split-line)))
        ;(push var (second split-line))
    )
    (when (string=? operation "add") (add))
    (when (string=? operation "sub") (sub))
    (when (string=? operation "eql") (eql))
)

(define (push-constant num)
    (displayln "\n//push constant" output-asm-file)
    (displayln (string-append "@" num) output-asm-file)
    (displayln "D=A\n@SP\nA=M\nM=D\nD=A+1\n@SP\nM=D" output-asm-file)
)

(define (add)
    (displayln (string-append "\n//add operation") output-asm-file)
    (pop-to-D)
    (displayln "A=A-1\nM=M+D\nD=A+1\n@SP\nM=D" output-asm-file)
)

(define (sub)
    (displayln (string-append "\n//sub operation") output-asm-file)
    (pop-to-D)
    (displayln "A=A-1\nM=M-D\n@SP\nM=M-1" output-asm-file)
)

(define (eql)
  (pop-to-D)
  (displayln "A=A-1\nD=M-D\n@")
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
