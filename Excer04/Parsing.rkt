#lang racket

(provide parsing-line-handling)

(define output-file empty)
(define tokens-list empty)
(define tabs "")


(define (parsing-line-handling line output-xml-file)
    (set! output-file output-xml-file)
    (set! tokens-list  (regexp-split #px"\n" line))
    (set! tokens-list  (remove* (list (first tokens-list) (second tokens-list)) tokens-list))
    (class-rule)
    (displayln "finished to parsing")
)


;========================================   program structure   ============================


(define (class-rule)
    (create-opening-tag "class")
    
        (create-token)  ;keyword class
        (create-token)  ;class name
        (create-token)  ; {

        (let loop() (when (nxt-class-var-dec?)  ; var dec *
                        (class-var-dec-rule)
                        (loop))
        )
        
        (let loop() (when (nxt-subroutine-dec?) ; subroutine dec *
                        (subroutine-dec-roule)
                        (loop))
        )
        
        
    (create-closing-tag "class")
)


(define (class-var-dec-rule)
    (create-opening-tag "classVarDec")
        (create-token) ;"static" or "field"
        (create-token) ; type
        (create-token) ; identifier
        (let loop ()
            (when (nxt-data-is? ",")
                (create-token) ; ","
                (create-token) ; identifier  
                (loop)             
            )
        )
        (create-token) ; ";"
    (create-closing-tag "classVarDec")
)


(define (subroutine-dec-roule)
    (create-opening-tag "subroutineDec")
        (create-token)  ; "constructor" or "function" or "metod"
        (create-token)  ; "return type" or "new" (for ctr)
        (create-token)  ; subroutine name
        (create-token)  ; "("

        (when (not (nxt-data-is? ")"))
            (parameter-list-role))

        (create-token)  ; ")"

        (subroutine-body-role)
        
    (create-closing-tag "subroutineDec")
)


(define (parameter-list-role)
    (create-opening-tag "parameterList")

        (create-token)  ; type
        (create-token)  ; var name

        (let loop ()
            (when (nxt-data-is? ",")
                (create-token)  ; ","
                (create-token)  ; type
                (create-token)  ; var nme
                (loop)
            )
        )

    (create-closing-tag "parameterList")
)


(define (subroutine-body-role)
    (create-opening-tag "subroutineBody")
        (create-token)  ; "{"

        (let loop()
            (when (nxt-data-is? "var")
                (var-dec-rule)(loop))
        )

        (statements-role)

    (create-closing-tag "subroutineBody")
)


(define (var-dec-rule)
    (create-opening-tag "varDec")

        (create-token)  ; "var"
        (create-token)  ; type
        (create-token)  ; var name
        (let loop ()
            (when (nxt-data-is? ",")
                (create-token) ; ","
                (create-token) ; identifier  
                (loop)             
            )
        )
        (create-token) ; ";"

    (create-closing-tag "varDec")

)


;=====================================   statements   ==================================


(define (statements-role)
    (create-opening-tag "statements")

        (let loop()
            (cond
                [(nxt-data-is? "let") (let-role) (loop)]
            )
        )

    (create-closing-tag "statements")
)


(define (let-role)
    (create-opening-tag "letStatement")

        (create-token)  ; "let"
        (create-token)  ; var name

        (when (nxt-data-is? "[")

            (create-token)      ; "["
            (expression-role)
            (create-token)      ; "]"
        )

        (create-token) ; "="
        (expression-role)
        (create-token)  ; ";"

    (create-closing-tag "letStatement")
)


(define (expression-role)
    (create-opening-tag "expression")

        (term-role)

    (create-closing-tag "expression")
)


(define (term-role)
    (create-opening-tag "term")

        (cond
            [(nxt-data-is? "(")
                (create-token)    ; "("
                (expression-role)
                (create-token)]   ; ")"
            
            [(or(nxt-type-is? "integerConstant")(nxt-type-is? "stringConstant")(nxt-is-keywordConstant?)) 
                (create-token)] ; number or string or keyword
            
            [(or (nxt-data-is? "-") (nxt-data-is? "~"))
                (create-token)  ; "-" or "~"
                (term-role)]
            
            [(string=? (get-token-data (second-token)) "[")
                (create-token)  ; var name
                (create-token)  ; "["
                (expression-role)
                (create-token)] ; "]"
            
            [(or (is-op? (get-token-data(second-token))) (string=? (get-token-data (second-token)) ";"))
                (create-token)] ; var name

            [else 
                (subroutine-call-role)] ; var name
        )

    (create-closing-tag "term")
)


(define (subroutine-call-role)
    (create-opening-tag "subroutineCall")

        (create-token)  ; subroutine name
        (cond
            [(nxt-data-is? "(")
                (create-token)  ; "("
                (expression-list-role)
                (create-token)] ; ")"
            [else
                (create-token)  ; "."
                (create-token)  ; subroutine
                (create-token)  ; "("
                (expression-list-role)
                (create-token)] ; ")"
        )

    (create-closing-tag "subroutineCall")
)


(define (expression-list-role)
    (create-opening-tag "expressionList")

        (when (not (nxt-data-is? ")"))
            ((expression-role))
        )
        (let loop()
            (when (nxt-data-is? ",")
                (create-token)  ; ","
                (expression-role)
                (loop))
        )

    (create-closing-tag "expressionList")
)


;========================================================================================


(define (create-opening-tag tag-name)
    (displayln (string-append tabs"<" tag-name ">") output-file)
    (increase-tabs)
)


(define (create-closing-tag tag-name)
    (decrease-tabs)
    (displayln (string-append tabs"</" tag-name ">") output-file)
)


(define (create-token)
    (define token (first tokens-list))
    (set! tokens-list (remove (first tokens-list) tokens-list))
    (displayln (string-append tabs token) output-file)
)


(define (increase-tabs)
    (set! tabs (string-append tabs "  "))
)


(define (decrease-tabs)
    (define len (string-length tabs))
    (displayln len)
    (set! tabs "")
    (let loop ()
            (when (> len 2)
                (set! tabs (string-append tabs "  "))
                (set! len (- len 2))
                (loop))
    )
    (displayln "out of decreas tabs")
)

; (define (is-keyword? line)
;     (string=? (second line) "keyword")
; )


; (define (is-identifier? line)
;     (string=? (second line) "identifier")
; )

(define (nxt-token-static?)
    (string=? (get-token-data (first tokens-list)) "static")
)

(define (nxt-token-field?)
    (string=? (get-token-data (first tokens-list)) "field")
)

(define (get-token-data token)
    (second (string-split token))
)


(define (get-token-type token)
    (define token-type (first (string-split token)))
    (substring token-type 1 (- (string-length token-type) 1))
)

(define (nxt-class-var-dec?)
    (define var-class-list (list "static" "field"))
    (is-member? (get-token-data (nxt-token)) var-class-list)
)

(define (nxt-subroutine-dec?)
    (define subroutin-list (list "function" "constructor" "method"))
    (is-member? (get-token-data (nxt-token)) subroutin-list)
)

(define (is-member? item my-list)
    (define flag #f)
    (map (lambda (element) 
            (when (string=? element item) (set! flag #t)))
        my-list)
    flag)

(define (nxt-data-is? data)
    (string=? (get-token-data (nxt-token)) data)
)

(define (nxt-type-is?)
    (define token-type (first (string-split (first tokens-list))))
    (substring token-type 1 (- (string-length token-type) 1))
)


(define (nxt-is-keywordConstant?)
    (define keywordConstant-list (list "true" "false" "null" "this"))
    (is-member? (get-token-data (nxt-token)) keywordConstant-list)
)


(define (is-op? token)
    (define op-list (list "+" "-" "*" "/" "&" "|" "<" ">" "="))
    (is-member? (get-token-data token) op-list)
)


(define (nxt-token)
    (first tokens-list)
)


(define (second-token)
    (second tokens-list)
)