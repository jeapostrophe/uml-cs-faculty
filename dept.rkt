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

(define (merge-content v nv)
  (cond
    [(not v) nv]
    [else
     (error 'merge-content "~v ~v" v nv)]))

(struct data (name content)
  #:property prop:procedure
  (λ (d x)
    (data (data-name d)
          (merge-content (data-content d) x)))
  #:transparent)
(define-syntax-rule (define-data kind)
  (define&provide kind (data 'kind #f)))
(repeat define-data
        area n title prefix year sort-group email
        adjunct-class)

(define (e x) (email (format "~a@cs.uml.edu" x)))
(provide e)

(struct status data () #:transparent)
(define-syntax-rule (define-status kind)
  (define&provide kind (status 'kind #f)))
(repeat define-status
        assoc assistant full
        retired emeritus outside deceased
        sabbatical out-of-dept leave award-teaching
        start end)

(struct position data () #:transparent)
(define-syntax-rule (define-position kind)
  (define&provide kind (position 'kind #f)))
(repeat define-position
        chair assoc-chair
        coord-ugrad coord-msit coord-grad
        coord-navitas coord-bsit coord-grad-ms
        college-personnel college-grad
        student-advisory-board college-ugrad
        newsletter advisor-acm coord-grad-phd
        wics advisor-gso advisor-robotics
        coord-grad-admissions coord-colloq
        ugradcom gradcom opscom p&t senate
        coord-grad-quals)

;; chair => p&t
;; coord-ugrad => ugradcom
;; coord-grad => gradcom
;; coord-grad-admissions => gradcom (XXX note when ended)

(struct person-info (content) #:transparent)
(define-syntax-rule (person . body)
  (person-info (list . body)))

(define-syntax-rule (prof . body)
  (person @prefix{Prof.} @sort-group['faculty]
          . body))
(define-syntax-rule (lecturer . body)
  (person @prefix{Prof.} @sort-group['faculty]
          . body))
(define-syntax-rule (adjunct . body)
  (person @prefix{Adjunct Prof.} @sort-group['adjunct]
          . body))
(define-syntax-rule (staff . body)
  (person @sort-group['staff] . body))

(provide prof lecturer adjunct staff)

;; Data Gathering
(require racket/match
         racket/list)

(define (snoc l x)
  (append l (list x)))

(define k->merger
  (hasheq))

(define (default-merger d y k kc)
  (hash-set d k kc))

(define (gather d y c)
  (match-define (data k kc) c)
  (define m (hash-ref k->merger k (λ () default-merger)))
  (m d y k kc))

(define (process-ps ps)
  (define y->pds (make-hasheq))

  (for ([p (in-list ps)])
    (define (save-cy! d cy)
      (unless (zero? cy)
        (hash-update! y->pds cy
                      (λ (old) (cons d old))
                      empty)))
    (define-values (ld lcy)
      (for/fold ([d (hash)]
                 [cy 0])
                ([c (in-list (person-info-content p))])
        (match c
          [(data 'year (? number? ny))
           (save-cy! d cy)
           (values d ny)]
          ["\n"
           (values d cy)]
          [_
           (values (gather d cy c) cy)])))
    (when (zero? lcy)
      (error 'process-ps "No date for ~e" p))

    (save-cy! ld lcy))

  y->pds)

;; Compiler
(require racket/runtime-path)

(define (current-academic-year)
  (local-require racket/date)
  (define d (current-date))
  (define y (date-year d))
  (define m (date-month d))
  (if (<= m 9)
    (sub1 y)
    y))

(define-runtime-path build "build")
(define (output! p title . content)
  (local-require xml
                 racket/file)
  (make-directory* build)
  (with-output-to-file
    (build-path build p)
    #:exists 'replace
    (λ ()
      (displayln "<!DOCTYPE html>")
      (write-xexpr
       `(html
         (head
          (title ,title)
          (meta ([http-equiv "Content-Type"] [content "text/html; charset=utf-8"]))
          (meta ([charset "utf-8"]))
          (link ([rel "stylesheet"] [href "style.css"]) ""))
         (body
          ,@content))))))

;; XXX detect gaps in service

(define (year-link y)
  (cond
    [y
     `(a ([href ,(format "~a.html" y)]) ,(number->string y))]
    [else
     'nbsp]))

(require racket/pretty)
(define (build-directory! py y ny people%y)
  ;; XXX Commitee page (like
  ;; https://teaching.cs.uml.edu/~heines/academic/department/Committees2014.jsp)

  (output!
   (format "~a.html" y)
   (format "UMass Lowell CS > ~a Directory" y)
   `(div ([class "menu"])
         (ul
          (li ,(year-link py))
          (li (a ([href "index.html"]) "Top"))
          (li ,(year-link ny))))
   "XXX"
   `(pre
     ,(pretty-format people%y))))

;; XXX two to a page, landscape
(define (build-board! people%y)
  (eprintf "build-board! XXX ~e\n" people%y))

(define (build-extras! y people%y)
  ;; XXX faces
  ;; XXX offices
  ;; XXX research
  (build-board! people%y))

(define (build-index! all-years cay)
  (output!
   "index.html"
   "UMass Lowell CS > Department Info"
   `(ul ([class "links"])
        (li (a ([href "faces.html"]) "Faces"))
        (li (a ([href "offices.html"]) "Offices"))
        (li (a ([href "board.html"]) "Board"))
        (li (a ([href "board.pdf"]) "Board (PDF)"))
        (li (a ([href "research.html"]) "Research")))
   `(ul ([class "years"])
        ,@(for/list ([y (in-list (reverse all-years))])
            `(li ,(year-link y))))))

(define (build! people)
  (define year->people (process-ps people))
  (define cay (current-academic-year))
  (define all-years (sort (hash-keys year->people) <=))
  (for ([y (in-list all-years)]
        [py (in-list (cons #f all-years))]
        [ny (in-list (snoc (rest all-years) #f))])
    (define people%y (hash-ref year->people y))
    (build-directory! py y ny people%y)
    (when (= y cay)
      (build-extras! y people%y)))
  (build-index! all-years cay))
