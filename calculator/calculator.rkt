; language and modules
#lang racket

(require
  web-server/servlet
  web-server/servlet-env)

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
