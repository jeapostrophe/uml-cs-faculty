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
         #%top
         quote)

;; Data Model
(define-syntax-rule (define&provide f x)
  (begin (define f x)
         (provide f)))
(define-syntax-rule (repeat f x ...)
  (begin (f x) ...))

(struct data (name content))
(define-syntax-rule (define-data kind)
  (define&provide kind (λ content (data 'kind content))))
(repeat define-data
        area n title prefix year sort-group email
        sabbatical
        ;; XXX
        ugradcom gradcom opscom p&t)

(struct status data ())
(define-syntax-rule (define-status kind)
  (define&provide kind (status 'kind #f)))
(repeat define-status
        assoc assistant full
        retired emeritus outside deceased
        start end)

(struct position data ())
(define-syntax-rule (define-position kind)
  (define&provide kind (position 'kind #f)))
(repeat define-position
        chair assoc-chair
        coord-ugrad coord-msit coord-grad
        coord-colloq coord-navitas)

;; chair => p&t
;; coord-ugrad => ugradcom
;; coord-grad => gradcom
;; coord-grad-admissions => gradcom (XXX note when ended)

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
(require racket/match
         racket/set)

(define (gather-years p)
  (match p
    [(person-info p)
     (gather-years p)]
    [(data 'year (list (? number? n)))
     (seteq n)]
    [(data _ p)
     (gather-years p)]
    [(cons x y)
     (set-union (gather-years x)
                (gather-years y))]
    [(or `()
         #t #f
         (? number?)
         (? string?)
         (? symbol?))
     (seteq)]))

(define (project y ps)
  (eprintf "XXX project ~a\n" y)
  ps)

(define (current-academic-year)
  (local-require racket/date)
  (define d (current-date))
  (define y (date-year d))
  (define m (date-month d))
  (if (<= m 9)
    (sub1 y)
    y))

;; XXX detect gaps in service

(define (build-committee! y people%y)
  ;; XXX Commitee page (like
  ;; https://teaching.cs.uml.edu/~heines/academic/department/Committees2014.jsp)
  (eprintf "build-committee! ~a XXX\n" y))

(define (build-archive! y people%y)
  (build-committee! y people%y))

;; XXX two to a page, landscape
(define (build-board! people%y)
  (eprintf "build-board! XXX\n"))

(define (build-phone-list! people%y)
  ;; XXX Get a photo
  (eprintf "build-phone-list! XXX\n"))

(define (build-face-list! people%y)
  ;; XXX Get a photo
  (eprintf "build-face-list! XXX\n"))

(define (build-current! y people%y)
  (build-board! people%y)
  (build-phone-list! people%y)
  (build-face-list! people%y))

(define (build! people)
  (define all-years (gather-years people))
  (define cay (current-academic-year))
  (for ([y (in-set all-years)])
    (define people%y (project y people))
    (build-archive! y people%y)
    (when (= y cay)
      (build-current! y people%y))))
