;;;;;;;;;;;; language and modules
#lang racket

(require
  web-server/servlet)

;;;;;;;;;;;; model


;;;;;;;;;;;; view


;;;;;;;;;;;; wrapper

; function that, when called, runs the webapp
(define start
  (response/xexpr
    '(html
       (head
         (title "todolist"))
       (body
         (h1 "todolist")))))

(provide (all-defined-out))
