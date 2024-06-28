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
    (is-member? (get-token-data (first tokens-list)) var-class-list)
)

(define (nxt-subroutine-dec?)
    (define subroutin-list (list "function" "constructor" "method"))
    (is-member? (get-token-data (first tokens-list)) subroutin-list)
)

(define (is-member? item my-list)
    (define flag #f)
    (map (lambda (element) 
            (when (string=? element item) (set! flag #t)))
        my-list)
    flag)

(define (nxt-data-is? data)
    (string=? (get-token-data (first tokens-list)) data)
)