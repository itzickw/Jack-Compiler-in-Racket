#lang racket

(provide file-handling)
(require "Tokenizing.rkt")
(require "Parsing.rkt")

(define parse-string "")

;(define active-func tokenizing-line-handling)

(define (file-handling folder-path file-path file-name)
    (define Txaml-file-path(create-tokens-file folder-path file-path file-name))
    (create-parse-file folder-path file-name Txaml-file-path)
)


(define (create-tokens-file folder-path input-file-path file-name)
  (displayln file-name)
  (define output-file-path (string-append folder-path"\\"file-name"T.xml"))
  (define output-Txml-file (open-output-file output-file-path #:exists 'replace))

  (displayln "<tokens>" output-Txml-file)
  (read-file input-file-path output-Txml-file tokenizing-line-handling)
  (displayln "</tokens>" output-Txml-file)
  
  (close-output-port output-Txml-file)
  output-file-path
)


(define (create-parse-file folder-path file-name input-file-path)
  (define output-file-path (string-append folder-path"\\"file-name".xml"))
  (define output-xml-file (open-output-file output-file-path #:exists 'replace))
  (set! parse-string "")

  (read-file input-file-path "" create-parse-string)
  ;(displayln parse-string)
  (parsing-line-handling parse-string output-xml-file)
  (close-output-port output-xml-file)
)


;=================================================================================


(define (read-file file-path output active-func)

  (define input-port (open-input-file file-path))

  (let loop ()
    (let ((line (read-line input-port)))
      (cond
        [
            (eof-object? line) ; Check for end-of-file
            (close-input-port input-port)
        ]
        [else
            (active-func (string-trim line) output)
            ;(displayln "finish to read line")
            (loop)
        ]
      )
    )
  )
)

(define (create-parse-string line output)
  (set! parse-string (string-append parse-string "\n" line))
)