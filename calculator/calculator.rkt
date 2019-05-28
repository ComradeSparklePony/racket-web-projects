; language and modules
#lang racket

(require
  web-server/servlet
  web-server/servlet-env)

; a Calculator is
; (calculator result operation input)
; where result and input are numbers
; and operation is an Operation
(struct calculator (result operation input))

; an Operation a String that is one of:
; - "+" representing addition
; - "-" representing subtraction
; - "*" representing multiplication
; - "/" representing division
; - "^" representing exponentiation

;;;;;;;;;;;; model



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
