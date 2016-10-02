#lang at-exp racket/base

;; Language
(define-syntax-rule (dept-module-begin body ...)
  (#%module-begin
   (define dept
     (list body ...))
   (module+ main
     (require racket/cmdline)
     (command-line #:program "dept"
                   #:args ()
                   (build! dept)))))

(provide (rename-out [dept-module-begin #%module-begin])
         #%datum
         #%app
         #%top)

;; Data Model
(struct data (name content))
(define-syntax-rule (define-data kind)
  (begin (define (kind . content) (data kind content))
         (provide kind)))
(define-syntax-rule (define-data* k ...)
  (begin (define-data k) ...))

(define-data*
  area n assoc-chair coord-ugrad coord-msit coord-grad coord-navitas
  retired emeritus start end title outside prefix year sort-group
  email chair br)

(struct person-info (content))
(define-syntax-rule (person . body)
  (person-info (list . body)))

(define-syntax-rule (prof . body)
  (person @prefix{Prof.} @sort-group['faculty]
          . body))
(define-syntax-rule (adjunct . body)
  (person @prefix{Adjunct Prof.} @sort-group['adjunct]
          . body))
(define-syntax-rule (staff . body)
  (person @sort-group['staff] . body))

(provide prof adjunct staff)

;; Compiler
(define (build! people)
  (error 'build! "XXX"))
