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



;;;;;;;;;;;; run the program
(define (start req)
  (response/xexpr
    `(html
       (head
	 (title "calculator"))
       (body
	 (h1 "calculator")))))

(serve/servlet start)
