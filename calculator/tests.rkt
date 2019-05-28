; lang and modules
#lang racket

(require
  "calculator.rkt"
  rackunit)

;;;;;;;;;;;; model


;;;;;;;;;;;; view
(calculator 1 "+" 2)

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

