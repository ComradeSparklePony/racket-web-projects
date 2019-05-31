; lang and modules
#lang racket

(require
  "calculator.rkt"
  rackunit
  web-server/test
  web-server/servlet)

;;;;;;;;;;;; model

; process-request : CalculatorRequest -> Calculator
; this processes a CalculatorRequest and turns it into a Calculator

(define (process-request-servlet req) ; servlet wrapper for process-request, for web-server/test
  (define rcalc (process-request req))
  (response/xexpr
    `(li
       ,(string-append
	  (number->string (calculator-result rcalc))
	  (calculator-operation rcalc)
	  (number->string (calculator-input rcalc))))))
(define process-request-tester
  (make-servlet-tester process-request-servlet))
(check-equal?
  (process-request-tester
    "/"
    (list
      (binding:form #"calculator-result" #"5")
      (binding:form #"calculator-operation" #"+")
      (binding:form #"calculator-input" #"5")
      (binding:form #"button-pressed" #"7")))
  '(li
     ()
     "5+57"))
(check-equal?
  (process-request-tester
    "/"
    (list
      (binding:form #"calculator-result" #"0")
      (binding:form #"calculator-operation" #"/")
      (binding:form #"calculator-input" #"50")
      (binding:form #"button-pressed" #"=")))
  '(li
     ()
     "0/0"))
(check-equal?
  (process-request-tester
    "/"
    (list))
  '(li
     ()
     "0+0"))
(check-equal?
  (process-request-tester
    "/"
    (list
      (binding:form #"calculator-result" #"23")
      (binding:form #"calculator-operation" #"-")
      (binding:form #"calculator-input" #"50")
      (binding:form #"button-pressed" #"*")))
  '(li
     ()
     "23*50"))

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

; calculator-request? : Any -> Boolean
; determines if a data type is a CalculatorRequest
; predicate for CalculatorRequest

(define (calculator-request?-servlet req)
  (response/xexpr
    `(li
       ,(boolean->string (calculator-request? req)))))

(define calculator-request?-tester
  (make-servlet-tester calculator-request?-servlet))

(check-equal?
  (calculator-request?-tester 
    "/"
    (list))
  '(li () "#f"))
(check-equal?
  (calculator-request?-tester
    "/"
    (list
      (binding:form #"calculator-result" #"23")
      (binding:form #"calculator-operation" #"-")
      (binding:form #"calculator-input" #"50")
      (binding:form #"button-pressed" #"*")))
  '(li () "#t"))
(check-equal?
  (calculator-request?-tester
    "/"
    (list
      (binding:form #"calculator-operation" #"+")
      (binding:form #"calculator-input" #"50")))
  '(li () "#f"))
(check-equal?
  (calculator-request?-tester
    "/"
    (list
      (binding:form #"name" #"john smith")
      (binding:form #"date" #"today")))
  '(li () "#f"))

; number-append : Number, Number -> Number
; Takes two numbers as input and appends the second to the first
(check-equal?
  (number-append 5 3)
  53)
(check-equal?
  (number-append 0 7)
  7)
(check-equal?
  (number-append 408 3)
  4083)

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

