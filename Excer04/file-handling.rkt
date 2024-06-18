#lang racket

(provide file-handling)
(require "Tokenizing.rkt")
(require "Parsing.rkt")

;(define active-func tokenizing-line-handling)

(define (file-handling folder-path file-path file-name)
    (create-tokens-file folder-path file-path file-name)
    (create-parse-file folder-path file-name)
)


(define (create-tokens-file folder-path file-path file-name)
  (set! file-name (string-append file-name "T.xml"))
  (define output-Txml-file (open-xml-file folder-path file-name))

  (displayln "<tokens>" output-Txml-file)
  (read-file file-path output-Txml-file tokenizing-line-handling)
  (displayln "</tokens>" output-Txml-file)
  
  (close-output-port output-Txml-file)
)


(define (create-parse-file folder-path file-name)

  (define file-path (string-append folder-path "\\" file-name "T.xml"))
  (define output-xml-file (open-xml-file folder-path (string-append file-name ".xml")))

  (read-file file-path output-xml-file parsing-line-handling)
  (close-output-port output-xml-file)
)


;=================================================================================


(define (open-xml-file folder-path name)
  (define file-path (string-append folder-path "\\" name))
  (define output-asm-file (open-output-file file-path #:exists 'replace))
  output-asm-file
)

(define (read-file file-path output-xml-file active-func)

  (define input-port (open-input-file file-path))

  (let loop ()
    (let ((line (read-line input-port)))
      (cond
        [
            (eof-object? line) ; Check for end-of-file
            (close-input-port input-port)
        ]
        [else
            (active-func line output-xml-file)
            (loop)
        ]
      )
    )
  )
)