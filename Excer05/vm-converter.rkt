#lang racket

(provide vm-converter)

(define name "")
(define kind "")
(define type "")
(define index "")
(define data-list list)
(define converted-str "")
(define true-count 0)
(define false-count 0)
(define if-end-count 0)
(define while-exp-count 0)
(define while-end-count 0)

(define (vm-converter val-type my-data-list)
    ;(displayln my-data-list)
    (when (not (empty? my-data-list))
        (set! data-list my-data-list)
        (set! name  (first   my-data-list))
        (set! type  (string-trim (second  my-data-list)))
        (set! kind  (third   my-data-list))
        (set! index (fourth  my-data-list))
        (set! converted-str ""))

    (cond
        [(string=? val-type "l-val")
            (l-val-convert)]

        [(string=? val-type "r-val")
            (r-val-convert)]

        [(string=? val-type "func-dec")
            (func-dec-convert)]
        
        [(string=? val-type "func-call")
            (func-call-convert)]

        [(string=? val-type "operation")
            (operation-convert)]
        
        [(string=? val-type "constructor-dec")
            (constructor-dec-convert)]
        
        [(string=? val-type "true-label")
            (true-label-convert)]
        
        [(string=? val-type "false-label")
            (false-label-convert)] 
        
        [(string=? val-type "while-exp-label")
            (while-exp-label-convert)] 
        
        [(string=? val-type "while-end-label")
            (while-end-label-convert)]

        [(string=? val-type "if-end-label")
            (if-end-label-convert)] 
        
        [(string=? val-type "array-handling")            
            (array-convert)] 
    )
    converted-str
)


(define (l-val-convert)
    (kind-index-convert)
    (set! converted-str (string-append "pop " converted-str))
;    [(not (string=? type "Array"))
;         (displayln type)
;         (kind-index-convert)
;         (set! converted-str (string-append "pop " converted-str))]

;     [(string=? type "Array")
;         (displayln "a")
;         (r-val-convert)
;         (set! converted-str (string-append converted-str "\nadd\npop pointer 1\npop that 0"))]
)

(define (r-val-convert)
    (kind-index-convert)
    (set! converted-str (string-append "push " converted-str))
    ;(displayln converted-str)
)


(define (kind-index-convert)
    ;(displayln kind)
    (cond 
        [(string=? "static" kind)
            (set! converted-str(string-append "static " index))]

        [(string=? "field" kind)
            (set! converted-str(string-append "this " index))]

        [(string=? "argument" kind)
            (set! converted-str(string-append "argument " index))]

        [(string=? "var" kind)
            (set! converted-str(string-append "local " index))]

        [(string=? "constant" kind)
            (constant-convert)]
    )
    ;(displayln converted-str)
)


(define (func-dec-convert)
    (set! true-count 0)
    (set! false-count 0)
    (set! if-end-count 0)
    (set! while-exp-count 0)
    (set! while-end-count 0)
    (set! converted-str (string-append "function "type"."name" "index))
)

(define (func-call-convert)    
    (set! converted-str (string-append "call "name" "index))
)


(define (constant-convert)
    (cond
        [(string=? type "int")
            (set! converted-str (string-append "constant " name))]

        [(string=? type "string")
            (string-convert)]

        [(string=? type "keyword")
            (keyword-convert)]
    )
)


(define (string-convert)
    (set! converted-str (string-append " constant " (number->string (string-length name))))
    (set! converted-str (string-append converted-str "\ncall String.new 1"))

    (set! name (string->list name))

    (define ascii-list (map (lambda (char) (char->integer char))name))

    (for-each   (lambda (char) 
                    (set! converted-str (string-append converted-str "\npush constant " (number->string char)))
                    (set! converted-str  (string-append converted-str "\ncall String.appendChar 2")))
        ascii-list)
    ;(displayln str-element)     
)


(define (keyword-convert)
    (cond
        [(string=? name "true")
            (set! converted-str "constant 0\nnot")]

        [(or (string=? name "false") (string=? name "null"))
            (set! converted-str "constant 0")]

        [(string=? name "this")
            (set! converted-str "pointer 0")]
    )    
)


(define (operation-convert)
    (cond
        [(string=? type "unary")
            (unary-convert)]
            
        [(string=? type "binary")
            (binary-convert)]
    )
)


(define (unary-convert)
    (cond
        [(string=? name "-")
            (set! converted-str "neg")]
        [(string=? name "~")
            (set! converted-str "not")]
    )
)


(define (binary-convert)
    (cond
        [(string=? name "+")
            (set! converted-str "add")]
        [(string=? name "-")
            (set! converted-str "sub")]
        [(string=? name "*")
            (set! converted-str "call Math.multiply 2")]
        [(string=? name "/")
            (set! converted-str "call Math.divide 2")]
        [(string=? name "&amp;")
            (set! converted-str "and")]
        [(string=? name "|")
            (set! converted-str "or")]
        [(string=? name "&lt;")
            (set! converted-str "lt")]
        [(string=? name "&gt;")
            (set! converted-str "gt")]
        [(string=? name "=")
            (set! converted-str "eq")]
    )
)


(define (constructor-dec-convert)
    (set! converted-str (string-append "push constant " index "\ncall Memory.alloc 1\npop pointer 0"))
)


(define (true-label-convert)
    (set! converted-str (string-append "IF_TRUE" (number->string true-count)))
    (set! true-count (+ true-count 1))
)


(define (false-label-convert)    
    (set! converted-str (string-append "IF_FALSE" (number->string false-count)))
    (set! false-count (+ false-count 1))
)


(define (while-exp-label-convert)
    (set! converted-str (string-append "WHILE_EXP" (number->string while-exp-count)))
    (set! while-exp-count (+ while-exp-count 1))
)


(define (while-end-label-convert)
    (set! converted-str (string-append "WHILE_END" (number->string while-end-count)))
    (set! while-end-count (+ while-end-count 1))
)


(define (if-end-label-convert)
    (set! converted-str (string-append "IF_END" (number->string if-end-count)))
    (set! if-end-count (+ if-end-count 1))
)


(define (array-convert)
    (set! converted-str "pop temp 0\npop pointer 1\npush temp 0\npop that 0")
)