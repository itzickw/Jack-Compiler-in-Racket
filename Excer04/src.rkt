#|
 Name 1: Itzik Walles   ID: 021996954
 Name 2: Yair Evron     ID: 204742878    
; Group num.  42
|#

#lang racket

(require "file-handling.rkt")

(define (is-jack file)
  (define file-name (path->string file))
  (string-suffix? file-name ".jack")
)

(define (drop-extension name)
  (define str-name (path->string name))
  (define parts (string-split str-name "."))
  (define temp1 (drop-right parts 1))
  (define temp2 (string-join temp1 "."))
  temp2
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

(define updated-list empty)

(set! updated-list (filter (lambda (file) (is-jack file)) file-list))

(for ([file updated-list])
  (when (is-jack file) 
      (define file-name (drop-extension file))
      (define file-path (string-append folder-path "\\" (path->string file)))
      (file-handling folder-path file-path file-name)
  )
)

(displayln "finish")