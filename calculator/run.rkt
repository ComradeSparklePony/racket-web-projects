#lang racket

(require
  "calculator.rkt"
  web-server/servlet
  web-server/servlet-env)

(serve/servlet start)

