; lang and modules
#lang racket

(require
  "calculator.rkt"
  rackunit)

;;;;;;;;;;;; model

; evaluate-calculator : Calculator -> Calculator
; this evaluates a calculator by applying it's Operation to its result and input
(check-equal?
  (evaluate-calculator (calculator 5 "+" 5))
  (calculator 10 "+" 0))
(check-equal?
  (evaluate-calculator (calculator 1 "-" 4))
  (calculator -3 "-" 0))
(check-equal?
  (evaluate-calculator (calculator 10 "*" 2))
  (calculator 20 "*" 0))
(check-equal?
  (evaluate-calculator (calculator 1000000 "/" 0))
  (calculator 0 "/" 0))
(check-equal?
  (evaluate-calculator (calculator 4 "^" 0.5))
  (calculator 2.0 "^" 0))

; apply-operation : Number, Operation, Number -> Number
; this figures out the result of applying an Operation to two numbers
; essentially does (operation result input)
(check-equal?
  (apply-operation 5 "+" 5) 10)
(check-equal?
  (apply-operation 1 "-" 4) -3)
(check-equal?
  (apply-operation 10 "*" 2) 20)
(check-equal?
  (apply-operation 1000000 "/" 0) 0)
(check-equal?
  (apply-operation 4 "^" 0.5) 2.0)
(check-equal?
  (apply-operation 5 "nPr" 2) 2)

;;;;;;;;;;;; view

; render-calculator-display : Calculator -> Xexpr
; this renders the display of the calculator in html
(check-equal?
  (render-calculator-display (calculator 5 "+" 5))
  '(div ((class "calculator-display"))
	(div ((class "prev-input")) "5 +")
	"5"))
(check-equal?
  (render-calculator-display (calculator 4.5 "^" 0.5))
  '(div ((class "calculator-display"))
	(div ((class "prev-input")) "4.5 ^")
	"0.5"))

