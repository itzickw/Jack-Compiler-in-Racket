#lang racket

(provide tokenizing-line-handling)

(define output-file empty)
(define note-flag #f)

(define (tokenizing-line-handling line output-xml-file)
    (when (not (empty? line))
        (set! output-file output-xml-file)
        (set! line (string->list line))
        (main-state line))
)

;=============================== jack otomation ========================


(define (main-state line)
    (when (not (empty? line))
        (define char (first line))
        
        (cond
            [(boolean=? note-flag #t)                
                (remove-note line)]

            [(char-ci=? char #\space) 
                (main-state (remove char line))]

            [(char=? char #\") 
                (str-constant-state "" (remove char line))]

            [(is-letter? char)
                (identifier-kyword-state (string char) (remove char line))]

            [(is-numeric? char) 
                (int-constant-state (string char) (remove char line))]

            [(is-symbol-char? char)
                (symbol-state (string char) (remove char line))]    

            [(is-undefined-symbol-char? char)
                (undifined-symbol-state (string char)) 
                (main-state  (remove char line))]        
        )))


(define (int-constant-state buffer line)
    
    (if (or (empty? line) (not (is-numeric? (first line))))
        (begin
            (create-token "integerConstant" buffer)
            (main-state line))
        (int-constant-state (string-append buffer (string (first line))) (remove (first line) line))))


(define (str-constant-state buffer line)
    
    (define char (first line))
    (if (char=? char #\")
        (begin  (create-token "stringConstant" buffer)
                (main-state (remove char line)))
        (str-constant-state (string-append buffer (string char)) (remove char line))
    )
)


(define (identifier-kyword-state buffer line)
    
    (if (or (empty? line) 
            (is-symbol-char? (first line)) 
            (is-undefined-symbol-char? (first line))
            (char-ci=? (first line) #\space))
        (begin
            (if (is-keyword-str? buffer)
                (create-token "keyword" buffer)
                (create-token "identifier" buffer))
            (main-state line))
        (identifier-kyword-state (string-append buffer (string (first line))) (remove(first line)line))
    ))


(define (symbol-state buffer line)
    
    (when (not (empty? line)) 
        (define char (first line))
        (cond
            [(not (string=? buffer "/"))
                (create-token "symbol" buffer)(main-state line)]

            [(char=? char #\/)
                 (main-state empty)]

            [(char=? char #\*) 
                (remove-note (remove char line))]

            ((create-token "symbol" buffer) (main-state line))
        )
    )

    (when (empty? line) (create-token "symbol" buffer)))


(define (undifined-symbol-state buffer)
    (cond
        [(string=? buffer "<") (create-token "symbol" "&lt;")]
        [(string=? buffer ">") (create-token "symbol" "&gt;")]
        [(string=? buffer "&") (create-token "symbol" "&amp;")]
    )
)


;================================== auxiliary functions ======================


(define (create-token token buffer)
    (displayln (string-append "<" token "> " buffer " </" token ">") output-file))


(define (is-numeric? char)
    (and (char-ci>=? char #\0) (char-ci<=? char #\9)))


(define (is-letter? char)
    (or (and (char-ci>=? char #\a) (char-ci<=? char #\Z)) (char-ci=? char #\_)))


(define (is-member? item my-list)
    (define flag #f)
    (map (lambda (element) 
            (when (string=? element item) (set! flag #t)))
        my-list)
    flag)


(define (is-symbol-char? char)
    (define symbol-list (list "{" "}" "(" ")" "[" "]" "." "," ";" "+" 
                                "-" "*" "/" "|" "=" "~"))
    (is-member? (string char) symbol-list))


(define (is-undefined-symbol-char? char)
    (define undifend-symbol-list (list "<" ">" "&"))
    (is-member? (string char) undifend-symbol-list)
)


(define (is-keyword-str? word)
    (define keyword-list (list "class" "constructor" "function" "method" "field" 
                                "static" "var" "int" "char" "boolean" "void" "true" "false" 
                                "null" "this" "let" "do" "if" "else" "while" "return"))
    (is-member? word keyword-list))


(define (remove-note line)
    (cond
        [(< (length line) 2) 
            (set! note-flag #t)
            (main-state empty)]

        [(and (char=? (first line) #\*) (char=? (second line) #\/))
            (set! note-flag #f)
            (main-state (remove* (list (first line) (second line)) line))]
        
        [else
            (remove-note (remove (first line) line))]
    )
)
