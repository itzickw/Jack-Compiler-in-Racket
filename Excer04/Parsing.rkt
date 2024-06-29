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

        (let loop() (when (class-var-dec-nxt?)  ; var dec *
                        (class-var-dec-rule)
                        (loop))
        )
        
        (let loop() (when (subroutine-dec-nxt?) ; subroutine dec *
                        (subroutine-dec-roule)
                        (loop))
        )
        
        (create-token)  ; "}"
    (create-closing-tag "class")
)


(define (class-var-dec-rule)
    (create-opening-tag "classVarDec")
        (create-token) ;"static" or "field"
        (create-token) ; type
        (create-token) ; identifier
        (let loop ()
            (when (is-nxt-data? ",")
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
        (parameter-list-role)
        (create-token)  ; ")"
        (subroutine-body-role)
        
    (create-closing-tag "subroutineDec")
)


(define (parameter-list-role)
    (create-opening-tag "parameterList")
        (when (not (is-nxt-data? ")"))        
            (create-token)  ; type
            (create-token)  ; var name

            (let loop ()
                (when (is-nxt-data? ",")
                    (create-token)  ; ","
                    (create-token)  ; type
                    (create-token)  ; var nme
                    (loop)
                )
            )
        )

    (create-closing-tag "parameterList")
)


(define (subroutine-body-role)
    (create-opening-tag "subroutineBody")
        (create-token)  ; "{"

        (let loop()
            (when (is-nxt-data? "var")
                (var-dec-rule)(loop))
        )

        (statements-role)
        (create-token)  ; "}"
    (create-closing-tag "subroutineBody")
)


(define (var-dec-rule)
    (create-opening-tag "varDec")

        (create-token)  ; "var"
        (create-token)  ; type
        (create-token)  ; var name
        (let loop ()
            (when (is-nxt-data? ",")
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
                [(is-nxt-data? "let") (let-role) (loop)]
                [(is-nxt-data? "if") (if-role) (loop)]
                [(is-nxt-data? "while") (while-role) (loop)]
                [(is-nxt-data? "do") (do-role) (loop)]
                [(is-nxt-data? "return") (return-role) (loop)]
            )
        )

    (create-closing-tag "statements")
)


(define (let-role)
    (create-opening-tag "letStatement")

        (create-token)  ; "let"
        (create-token)  ; var name

        (when (is-nxt-data? "[")

            (create-token)      ; "["
            (expression-role)
            (create-token)      ; "]"
        )

        (create-token) ; "="
        (expression-role)
        (create-token)  ; ";"

    (create-closing-tag "letStatement")
)


(define (if-role)
    (create-opening-tag "ifStatement")

        (create-token)  ; "if"
        (create-token)  ; "("
        (expression-role)
        (create-token)  ; ")"
        (create-token)  ; "{"
        (statements-role)
        (create-token)  ; "}"
        
        (when (is-nxt-data? "else")
            (create-token)  ; "elae"
            (create-token)  ; "{"
            (statements-role)
            (create-token)) ; "}"

    (create-closing-tag "ifStatement")
)

(define (while-role)
    (create-opening-tag "whileStatement")

        (create-token)  ; "while"
        (create-token)  ; "("
        (expression-role)
        (create-token)  ; ")"
        (create-token)  ; "{"
        (statements-role)
        (create-token)  ; "}"

    (create-closing-tag "whileStatement")
)


(define (do-role)
    (create-opening-tag "doStatement")

        (create-token)  ; "do"
        (subroutine-call-role)
        (create-token)  ; ";"

    (create-closing-tag "doStatement")
)


(define (return-role)
    (create-opening-tag "returnStatement")

        (create-token)  ; "return"
        (when (not (is-nxt-data? ";"))
            (expression-role)
        )
        (create-token)  ; ";"

    (create-closing-tag "returnStatement")
)


; ======================================  expressions  ===============================


(define (expression-role)
    (create-opening-tag "expression")

        (term-role)

        (let loop()
            (when (is-nxt-op?)
                (create-token)  ; operation
                (term-role)
                (loop))
        )
        
    (create-closing-tag "expression")
)


(define (term-role)
    (create-opening-tag "term")

        (cond
            [(is-nxt-type? "integerConstant")                
                (create-token)] ; number
            
            [(is-nxt-type? "stringConstant")               
                (create-token)] ; string
            
             [(is-nxt-type? "keyword")               
                (create-token)] ; "true" or "fulse" or "null" or "this"
                        
            [(or (is-nxt-data? "-") (is-nxt-data? "~"))               
                (create-token)  ; "-" or "~"
                (term-role)]  

            [(is-second-data? "[")               
                (create-token)  ; var name
                (create-token)  ; "["
                (expression-role)
                (create-token)] ; "]"

            [(is-nxt-data? "(")               
                (create-token)    ; "("
                (expression-role)
                (create-token)]   ; ")"  

            [(or (is-second-data? "(") (is-second-data? "."))                    
                (subroutine-call-role)] 

            [(is-nxt-type? "identifier")
                (create-token)] ; var name              
        )

    (create-closing-tag "term")
)


(define (subroutine-call-role)
    ;(create-opening-tag "subroutineCall")

        (create-token)  ; subroutine name
        (cond
            [(is-nxt-data? "(")
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

    ;(create-closing-tag "subroutineCall")
)


(define (expression-list-role)
    (create-opening-tag "expressionList")

        (when (not (is-nxt-data? ")"))
            (expression-role)
        )
        
        (let loop()
            (when (is-nxt-data? ",")
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
    ;(displayln tag-name)
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
    (set! tabs "")
    (let loop ()
            (when (> len 2)
                (set! tabs (string-append tabs "  "))
                (set! len (- len 2))
                (loop))
    )
)


(define (get-token-data token)
    (second (string-split token))
)


(define (class-var-dec-nxt?)
    (define var-class-list (list "static" "field"))
    (is-member? (get-token-data (nxt-token)) var-class-list)
)


(define (subroutine-dec-nxt?)
    (define subroutin-list (list "function" "constructor" "method"))
    (is-member? (get-token-data (nxt-token)) subroutin-list)
)


(define (is-member? item my-list)
    (define flag #f)
    (map (lambda (element) 
            (when (string=? element item) (set! flag #t)))
        my-list)
    flag)


(define (is-nxt-data? data)
    (string=? (get-token-data (nxt-token)) data)
)


(define (is-second-data? data)
    (string=? data (get-token-data (second-token)))
)


(define (is-nxt-type? type)
    (define token-type (first (string-split (nxt-token))))
    (string=? (substring token-type 1 (- (string-length token-type) 1)) type)
)


(define (is-nxt-op?)
    (define op-list (list "+" "-" "*" "/" "&amp;" "|" "&lt;" "&gt;" "="))
    (is-member? (get-token-data (nxt-token)) op-list)
)


(define (nxt-token)
    (first tokens-list)
)


(define (second-token)
    (second tokens-list)
)