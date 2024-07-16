#lang racket

(require "vm-converter.rkt")
(provide parsing-line-handling)

(define xml-file empty)
(define vm-file empty)
(define tokens-list empty)
(define tabs "")
(define class-name empty)

(define class-symbol-table list)
(define method-symbol-table list)
(define static-count 0)
(define field-count 0)
(define argument-count 0)
(define local-var-count 0)



;===================================   main function   ==================================

(define (parsing-line-handling line output-xml-file output-vm-file)

    (set! xml-file output-xml-file)
    (set! vm-file output-vm-file)

    (set! tokens-list  (regexp-split #px"\n" line))
    (set! tokens-list  (remove* (list (first tokens-list) (second tokens-list)) tokens-list))

    (class-rule)

    (displayln "finished to parsing")
)


(define (class-var-set)
    (set! class-symbol-table empty)
    (set! static-count 0)
    (set! field-count 0)
)


;==================================   program structure   ===============================


(define (class-rule)
    (class-var-set)
    (set! class-name (print-creat-class))     ; claas opening tag, keyword class, name, {
        
        (let loop() (when (class-var-dec-nxt?)  ; var dec *
                        (class-var-dec-rule)
                        (loop)))

        (subroutine-dec-roule)
        (let loop() (when (subroutine-dec-nxt?) ; subroutine dec *                    
                        (print-vm "\n")
                        (subroutine-dec-roule)
                        (loop)))
    (print-close-class)
)


(define (class-var-dec-rule)
    (create-opening-tag "classVarDec")

        (define kind (create-token))    ;"static" or "field"
        (define type (create-token))    ; type
        (define name (create-token))    ; identifier    

        (push-class-symbol-table name type kind)

        (let loop ()
            (when (is-nxt-data? ",")
                (create-token)  ; ","
                (set! name (create-token))  ; identifier  
                (push-class-symbol-table name type kind)
                (loop)             
            )
        )
        (create-token) ; ";"

    (create-closing-tag "classVarDec")
)


(define (subroutine-dec-roule)
    (create-opening-tag "subroutineDec")
        
        (subroutin-var-set)

        (define type (create-token))  ; "constructor" or "function" or "metod"
        (create-token)  ; "return type" or "new" (for constructor)

        (define name (create-token))  ; subroutine name
        (create-token)  ; "("
            (when (string=? type "method")
                (push-method-symbol-table "this" class-name "argument"))
                
        (parameter-list-role)
        (create-token)  ; ")"

        (subroutine-body-role (list name class-name type))        
        
    (create-closing-tag "subroutineDec")    
)


(define (parameter-list-role)
    (create-opening-tag "parameterList")

        (when (not (is-nxt-data? ")"))      

            (define type (create-token))  ; type
            (define name (create-token))  ; var name
            (push-method-symbol-table name type "argument")            
            (let loop ()
                (when (is-nxt-data? ",")
                    (create-token)  ; ","
                    (set! type (create-token))  ; type
                    (set! name (create-token))  ; var nme
                    (push-method-symbol-table name type "argument")                    
                    (loop)
                )
            )
        )

    (create-closing-tag "parameterList")
)


(define (subroutine-body-role data-list)
    (create-opening-tag "subroutineBody")
        (create-token)  ; "{"        
        (define var-num 0)
        (let loop()
            (when (is-nxt-data? "var")
                (set! var-num (+ var-num (var-dec-rule)))
                (loop))
        )

        (set! data-list (append data-list (list (number->string var-num)))) 
        (print-vm (vm-converter "func-dec" data-list))

        (when (string=? (third data-list) "constructor")
            
            (set! data-list (remove (fourth data-list) data-list))
            (set! data-list (append data-list (list (number->string field-count))))                
            (print-vm (string-append "\n"(vm-converter "constructor-dec" data-list)))
        )

        (when (string=? (third data-list) "method")
            (print-vm "\npush argument 0\npop pointer 0"))

        (print-vm (statements-role))

        (create-token)  ; "}"
    (create-closing-tag "subroutineBody")
)


(define (var-dec-rule)
    (create-opening-tag "varDec")
        (define var-counter 0)

        (create-token)  ; "var"
        (define type (create-token))  ; type
        (define name (create-token))  ; var name
        (push-method-symbol-table name type "var")
        (set! var-counter (+ var-counter 1))
        
        (let loop ()
            (when (is-nxt-data? ",")
                (create-token) ; ","
                (set! name (create-token)) ; identifier 
                (push-method-symbol-table name type "var")
                (set! var-counter (+ var-counter 1))
                (loop)             
            )
        )
        (create-token) ; ";"
 
    (create-closing-tag "varDec")
    var-counter
)


;=====================================   statements   ==================================


(define (statements-role)
    (create-opening-tag "statements")
        (define statments-str "")
        (let loop()
            (cond
                [(is-nxt-data? "let")   (set! statments-str (string-append statments-str"\n"(let-role)))]
                [(is-nxt-data? "if")    (set! statments-str (string-append statments-str"\n"(if-role)))]
                [(is-nxt-data? "while") (set! statments-str (string-append statments-str"\n"(while-role)))]
                [(is-nxt-data? "do")    (set! statments-str (string-append statments-str"\n"(do-role)))]
                [(is-nxt-data? "return")(set! statments-str (string-append statments-str"\n"(return-role)))]
            )
            (when (not(string=? (nxt-token-data) "}")) (loop))
        )
    
    (create-closing-tag "statements")
    statments-str
)


(define (let-role)
    (create-opening-tag "letStatement")
        (define coverted-let-str "")
        (create-token)  ; "let"        
        (cond
            [(not (is-second-data? "["))
                (set! coverted-let-str (vm-converter "l-val" (get-symbol-data (create-token))))
                (create-token) ; "="
                (set! coverted-let-str (string-append (expression-role)"\n"coverted-let-str))   ; var name
                (create-token)]  ; ";"  
            
            [(is-second-data? "[")                
                (set! coverted-let-str (vm-converter "r-val" (get-symbol-data (create-token)))) ; var name
                (create-token)  ; "["                
                (set! coverted-let-str (string-append (expression-role)"\n"coverted-let-str"\nadd"))   ; index                
                (create-token)  ; "]"
                (create-token)  ; "="
                (set! coverted-let-str (string-append coverted-let-str"\n"(expression-role)))                
                (set! coverted-let-str (string-append coverted-let-str"\n"(vm-converter "array-handling" empty)))                
                (create-token)] ; ";"
        )

    (create-closing-tag "letStatement")    
    coverted-let-str
)


(define (if-role)
    (create-opening-tag "ifStatement")
        (define converted-if-str "")
        (define true-label (vm-converter "true-label" empty))
        (define false-label (vm-converter "false-label" empty))
        (define end-label (vm-converter "if-end-label" empty))

        (create-token)  ; "if"
        (create-token)  ; "("
        (set! converted-if-str (expression-role))
        (create-token)  ; ")"

        (set! converted-if-str (string-append converted-if-str "\nif-goto " true-label))
        (set! converted-if-str (string-append converted-if-str "\ngoto "false-label))
        (set! converted-if-str (string-append converted-if-str"\nlabel "true-label))

        (create-token)  ; "{"
        (set! converted-if-str (string-append converted-if-str (statements-role)))        
        (create-token)  ; "}"
        
        (when (is-nxt-data? "else")
            (set! converted-if-str (string-append converted-if-str"\ngoto "end-label)))
        (set! converted-if-str (string-append converted-if-str"\nlabel "false-label))

        (when (is-nxt-data? "else")
            (create-token)  ; "elae"
            (create-token)  ; "{"
            (set! converted-if-str (string-append converted-if-str (statements-role)))
            (set! converted-if-str (string-append converted-if-str"\nlabel "end-label))
            (create-token)) ; "}"
    (create-closing-tag "ifStatement")
    converted-if-str
)

(define (while-role)
    (create-opening-tag "whileStatement")
        (define converted-while-str "")
        (define while-exp-label (vm-converter "while-exp-label" empty))
        (define while-end-label (vm-converter "while-end-label" empty))

        (set! converted-while-str (string-append "label "while-exp-label))
        (create-token)  ; "while"
        (create-token)  ; "("
        (set! converted-while-str (string-append converted-while-str"\n"(expression-role)))
        (set! converted-while-str (string-append converted-while-str"\nnot\nif-goto "while-end-label))
        (create-token)  ; ")"

        (create-token)  ; "{"
        (set! converted-while-str (string-append converted-while-str (statements-role)))
        (create-token)  ; "}"

        (set! converted-while-str (string-append converted-while-str"\ngoto "while-exp-label))
        (set! converted-while-str (string-append converted-while-str"\nlabel "while-end-label))
    (create-closing-tag "whileStatement")
    converted-while-str
)


(define (do-role)
    (create-opening-tag "doStatement")
        (create-token)  ; "do"
        (define converted-do-str (string-append (subroutine-call-role)"\npop temp 0"))
        (create-token)  ; ";"
    (create-closing-tag "doStatement")
    converted-do-str
)


(define (return-role)
    (create-opening-tag "returnStatement")
        (define converted-return-str empty)
        (create-token)  ; "return"
        (when (not (is-nxt-data? ";"))
            (set! converted-return-str (expression-role))
        )
        (create-token)  ; ";"         

    (create-closing-tag "returnStatement")
    (when (empty? converted-return-str) (set! converted-return-str "push constant 0"))
    (string-append converted-return-str "\nreturn")
)


; ======================================  expressions  ===============================


(define (expression-role)
    (create-opening-tag "expression")
        (define converted-expression-str "")
        (set! converted-expression-str(term-role))

        (let loop()
            (when (is-nxt-op?)            
                (define op (vm-converter "operation" (list (create-token) "binary" "" "0")))  ; operation
                (set! converted-expression-str (string-append converted-expression-str"\n"(term-role)"\n"op))                
                (loop))
        )
        
    (create-closing-tag "expression")    
    converted-expression-str
)


(define (term-role)
    (create-opening-tag "term")
        (define converted-term-str "")

        (cond
            [(is-nxt-type? "integerConstant")                
                (set! converted-term-str(vm-converter "r-val" (list (create-token) "int" "constant" "0")))] ; number
            
            [(is-nxt-type? "stringConstant")               
                (set! converted-term-str(vm-converter "r-val" (list (create-token) "string" "constant" "0")))] ; string
            
             [(is-nxt-type? "keyword")               
                (set! converted-term-str(vm-converter "r-val" (list (create-token) "keyword" "constant" "0")))] ; "true" or "fulse" or "null" or "this"
                        
            [(or (is-nxt-data? "-") (is-nxt-data? "~"))                         
                (set! converted-term-str (vm-converter "operation" (list (create-token) "unary" "" "0")))  ; "-" or "~"
                (set! converted-term-str (string-append (term-role)"\n"converted-term-str ))]                     

            [(is-second-data? "[")               
                (set! converted-term-str (vm-converter "r-val" (get-symbol-data(create-token))))  ; var name
                (create-token)  ; "["
                (set! converted-term-str (string-append (expression-role)"\n"converted-term-str))
                (create-token)
                (set! converted-term-str (string-append converted-term-str "\nadd\npop pointer 1\npush that 0"))] ; "]"

            [(is-nxt-data? "(")               
                (create-token)    ; "("
                (set! converted-term-str (expression-role))
                (create-token)]   ; ")"  

            [(or (is-second-data? "(") (is-second-data? "."))                    
                (set! converted-term-str (subroutine-call-role))] 

            [(is-nxt-type? "identifier")
                (set! converted-term-str (vm-converter "r-val" (get-symbol-data (create-token))))] ; var name              
        )    
    (create-closing-tag "term")
    converted-term-str
)


(define (subroutine-call-role)
    (define converted-call-str "")
    (define exp-list-data empty)
    (define func-name (create-token))   ; name
    ;(displayln func-name)
    (define arg-count 0)

    (cond
        [(is-nxt-data? "(")
            (set! converted-call-str "push pointer 0")
            (set! arg-count (+ arg-count 1))
            (set! func-name (string-append class-name"."func-name ))]

        [(not (empty? (get-symbol-data func-name)))
            (set! converted-call-str (vm-converter "r-val" (get-symbol-data func-name)))
            (set! arg-count (+ arg-count 1))
            (set! func-name (second(get-symbol-data func-name)))
            (set! func-name (string-append func-name (create-token)))  ; "."
            (set! func-name (string-append func-name (create-token)))]  ; name
        
        [(empty? (get-symbol-data func-name))
            (set! func-name (string-append func-name (create-token)))  ; "."
            (set! func-name (string-append func-name (create-token)))]  ; name
    )
    
    (create-token)  ; "("
        (set! exp-list-data (expression-list-role))
    (create-token)  ; ")"
    
    (set! arg-count (number->string (+ arg-count (second exp-list-data))))
    (set! converted-call-str (string-append converted-call-str"\n"(first exp-list-data)))
    (set! exp-list-data (list func-name class-name "" arg-count))
    (set! converted-call-str(string-append converted-call-str"\n"(vm-converter "func-call" exp-list-data)))        
    converted-call-str
)


(define (expression-list-role)
    (create-opening-tag "expressionList")
        (define converted-expr-str "")
        (define parameter-count 0)
        (when (not (is-nxt-data? ")"))
            (set! converted-expr-str (expression-role))
            (set! parameter-count (+ parameter-count 1))
        )
        
        (let loop()
            (when (is-nxt-data? ",")
                (create-token)  ; ","
                (set! converted-expr-str (string-append converted-expr-str"\n"(expression-role)))
                (set! parameter-count (+ parameter-count 1))
                (loop))
        )
    (create-closing-tag "expressionList")
    (list converted-expr-str parameter-count)
)


;===============================   xml printing functions   =======================================


(define (create-opening-tag tag-name)
    (displayln (string-append tabs"<" tag-name ">") xml-file)
    (increase-tabs)
)


(define (create-closing-tag tag-name)
    (decrease-tabs)
    (displayln (string-append tabs"</" tag-name ">") xml-file)    
)


(define (create-token)
    (define token (nxt-token))
    (set! tokens-list (rest tokens-list))
    (displayln (string-append tabs token) xml-file)
    (get-token-data token)
)


(define (print-creat-class)
    
    (create-opening-tag "class")
    (create-token)  ;keyword class  
    (define name (create-token))  ;class name
    (create-token)  ; {
    name
)


(define (print-close-class)
    (create-token)  ; "}"
    (create-closing-tag "class")    
)


;===============================   vm printing functions   =======================================


(define (print-vm str)
    (display str vm-file)
)

;==================================   predicates   ======================================


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


;========================================================================================


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
  (define open-tag-regex #rx"<[^>]*>")
  (define close-tag-regex #rx"</[^>]*>")
  (define token-data (regexp-replace open-tag-regex token ""))
  (set! token-data (regexp-replace close-tag-regex token-data ""))
  (set! token-data (substring token-data 1 (- (string-length token-data) 1)))
  token-data
)


(define (nxt-token)
    (first tokens-list)
)


(define (second-token)
    (second tokens-list)
)


(define (nxt-token-data)
    (get-token-data (nxt-token))
)


(define (push-class-symbol-table name type kind)
    (define index 0)
    (cond
        [(string=? kind "static")
            (set! index static-count)
            (set! static-count (+ static-count 1))]

        [(string=? kind "field")
            (set! index field-count)
            (set! field-count (+ field-count 1))]
    )
    (set! class-symbol-table (append class-symbol-table (list (list name type kind (number->string index)))))
)


(define (push-method-symbol-table name type kind)
    (define index 0)
    (cond
        [(string=? kind "argument")
            (set! index argument-count)
            (set! argument-count (+ argument-count 1))]

        [(string=? kind "var")
            (set! index local-var-count)
            (set! local-var-count (+ local-var-count 1))]
    )
    (set! method-symbol-table (append method-symbol-table (list (list name type kind (number->string index)))))
)


(define (subroutin-var-set)
    (set! method-symbol-table empty)
    (set! argument-count 0)
    (set! local-var-count 0)
) 


(define (get-symbol-data-or-empty name symbol-table)
    (define symbol-data empty)
    (when (not (empty? symbol-table))
        (map (lambda (var-data) 
                        (when (string=? (first var-data) name)
                            (set! symbol-data var-data)))
            symbol-table)                  
    )
    symbol-data
)


(define (get-symbol-data name)
    (define symbol-data (get-symbol-data-or-empty name method-symbol-table))
    (when (empty? symbol-data)
        (set! symbol-data (get-symbol-data-or-empty name class-symbol-table)))    
    symbol-data
)