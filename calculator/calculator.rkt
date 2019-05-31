; NOTE TO SELF: continue woth process-request

; language and modules
#lang racket

(require
  web-server/servlet
  web-server/servlet-env
  web-server/http)

; a Calculator is
; (calculator result operation input)
; where result and input are numbers
; and operation is an Operation
(struct calculator (result operation input) #:transparent)

; an Operation a String that is one of:
; - "+" representing addition
; - "-" representing subtraction
; - "*" representing multiplication
; - "/" representing division
; - "^" representing exponentiation

; a CalculatorRequest is a Request consisting of the following fields:
; - calculator-result (Number as a Bytes)
; - calculator-operation (Operation as a Bytes)
; - calculator-input (Number as a Bytes)
; - button-pressed (CalculatorButton as a Bytes)

; a CalculatorButton is a String that is one of:
; - one of Operation, meaning the operation should be changed
; - "=", representing that a calculator should be evaluated
; - a 1-digit number, from 0 to 9, which should be appended to the input

;;;;;;;;;;;; model

; process-request : CalculatorRequest -> Calculator
; this processes a CalculatorRequest and turns it into a Calculator
(define (process-request req)
  (if (calculator-request? req)
      (let*
	([binds (request-bindings/raw req)]
	[cr-result
	  (string->number
	    (bytes->string/utf-8
	      (binding:form-value
		(bindings-assq #"calculator-result" binds))))]
	[cr-operation
	  (bytes->string/utf-8
	    (binding:form-value
	      (bindings-assq #"calculator-operation" binds)))]
	[cr-input
	  (string->number
	    (bytes->string/utf-8
	      (binding:form-value
		(bindings-assq #"calculator-input" binds))))]
	[cr-button
	  (bytes->string/utf-8
	    (binding:form-value
	      (bindings-assq #"button-pressed" binds)))])
	(cond
	  [(string-contains? "+-/*^" cr-button)            ; button is an operation
	   (calculator cr-result cr-button cr-input)]
	  [(string=? "=" cr-button)                        ; button is "="
	   (evaluate-calculator
	     (calculator cr-result cr-operation cr-input))]
	  [(string-contains? "0123456789" cr-button)       ; button is a number
	   (calculator cr-result
		       cr-operation
		       (number-append cr-input (string->number cr-button)))]))
      (calculator 0 "+" 0)))

; evaluate-calculator : Calculator -> Calculator
; this evaluates a calculator by applying it's Operation to its result and input
(define (evaluate-calculator calc)
  (calculator
    (apply-operation
      (calculator-result calc)
      (calculator-operation calc)
      (calculator-input calc))
    (calculator-operation calc)
    0))

; apply-operation : Number, Operation, Number -> Number
; this figures out the result of applying an Operation to two numbers
; essentially does (operation result input)
(define (apply-operation num1 op num2)
  (cond
    [(string=? op "+") (+ num1 num2)]
    [(string=? op "-") (- num1 num2)]
    [(string=? op "*") (* num1 num2)]
    [(string=? op "/") (if (not (zero? num2))
			   (/ num1 num2)
			   0)]
    [(string=? op "^") (expt num1 num2)]
    [else num2]))

; calculator-request? : Any -> Boolean
; determines if a data type is a CalculatorRequest
; predicate for CalculatorRequest
(define (calculator-request? req)
  (define binds
    (request-bindings/raw req))
  (if (request? req)
      (and
	(bindings-assq #"calculator-result" binds)
	(bindings-assq #"calculator-operation" binds)
	(bindings-assq #"calculator-input" binds)
	(bindings-assq #"button-pressed" binds)
	#t)
      #f))

; boolean->string : Boolean -> String
; converts a boolean to a string.
(define (boolean->string b)
  (if b "#t" "#f"))

; number-append : Number, Number -> Number
; Takes two numbers as input and appends the second to the first
(define (number-append num1 num2)
  (string->number
    (string-append
      (number->string num1)
      (number->string num2))))

;;;;;;;;;;;; view

; render-calculator-display : Calculator -> Xexpr
; this renders the display of the calculator in html
(define (render-calculator-display calc)
  `(div ((class "calculator-display"))
	(div ((class "prev-input"))
	     ,(string-append
	       (number->string (calculator-result calc))
	       " "
	       (calculator-operation calc)))
	,(number->string (calculator-input calc))))

; render-calculator-keypad : -> Xexpr
; this renders the keypad in html
; and will make the proper request when a key is pressed

;;;;;;;;;;;; run the program
(define (start req)
  (response/xexpr
    `(html
       (head
	 (title "calculator"))
       (body
	 (h1 "calculator")))))

(provide (all-defined-out))
