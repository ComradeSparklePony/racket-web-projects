; language and modules
#lang racket

(require
  web-server/servlet
  web-server/servlet-env)

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

;;;;;;;;;;;; model

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

;;;;;;;;;;;; run the program
(define (start req)
  (response/xexpr
    `(html
       (head
	 (title "calculator"))
       (body
	 (h1 "calculator")))))

(provide (all-defined-out))
