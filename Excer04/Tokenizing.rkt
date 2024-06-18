#lang racket

(provide tokenizing-line-handling)

(define (tokenizing-line-handling line output-xml-file)
    ;(when (not (string=? line "")) (set! line(substring line 1)))
    (displayln line output-xml-file)
)