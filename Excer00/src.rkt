#|
 Name 1: Itzik Walles   ID: 021996954
 Name 2: Yair Evron     ID: 204742878    
; Group num.  42
|#

#lang racket


(define total-buy 0)
(define total-sell 0)


(define (HandleBuy ProductName cost output-port)
  (define title(string-append "### BUY " ProductName " ###"))
  (displayln title output-port)
  (displayln cost output-port)
  (set! total-buy(+ total-buy cost))
)


(define (HandleSell  ProductName cost output-port) 
  (define title (string-append "$$$ SELL " ProductName " $$$"))
  (displayln title output-port)
  (displayln cost output-port)
  (set! total-sell(+ total-sell cost))

)


(define (handle-line line output-port)
  (define split-line(string-split line " "))

  (define operation (first split-line))
  (define product-name (second split-line))
  (define amount (string->number(third split-line)))
  (define price (string->number(fourth split-line)))
  
  (define cost (* amount price))
  
  (when (equal? operation "buy") (HandleBuy product-name cost output-port))
  (when (equal? operation "sell") (HandleSell product-name cost output-port))
)


(define (write-buttom-line)
    (define buy (number->string total-buy))
    (define sell (number->string total-sell))

    (define writing-line(string-append "TOTAL BUY: " buy))
    (displayln writing-line output-asm-file)
    (set! writing-line(string-append "TOTAL SELL: " sell))
    (displayln writing-line output-asm-file)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (get-folder-name folder-path)
  (define folder-path-parts (string-split folder-path "\\"))
  (define folder-name (last folder-path-parts))
  folder-name
)


(define (open-asm-file name)
  (define file-name (string-append name ".asm"))
  (define output-asm-file (open-output-file file-name #:exists 'replace))
  output-asm-file
)


(define (isVm file)
  (define file-name (path->string file))
  (string-suffix? file-name ".vm")
)


(define (drop-extension name)
  (define str-name (path->string name))
  (define parts (string-split str-name "."))
  (define temp1 (drop-right parts 1))
  (define temp2 (string-join temp1 "."))
  temp2
)


(define (read-file file-path)

  (define input-port (open-input-file file-path))

  (let loop ()

    (let ((line (read-line input-port)))
      (cond
        [(eof-object? line) ; Check for end-of-file
          (close-input-port input-port)
        ]
        [else
          (handle-line line output-asm-file)
          (loop)
        ]
      )
    )
  )
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

(define folder-name(get-folder-name folder-path))
(define output-asm-file(open-asm-file folder-name))


(for ([file file-list])
  (when (isVm file) 

      (define file-name (drop-extension file))
      (displayln file-name output-asm-file)
      (define file-path (string-append folder-path "\\" (path->string file)))
      (read-file file-path)
      (displayln  "" output-asm-file)
  )

  (when(equal? file (last file-list))
    (write-buttom-line)
  )
)
