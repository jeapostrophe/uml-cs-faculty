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
        adjunct-class adhoc)

(define (e x) (email (format "~a@cs.uml.edu" x)))
(provide e)

(define&provide assoc (data 'rank 'assoc))
(define&provide assistant (data 'rank 'assistant))
(define&provide full (data 'rank 'full))

(define&provide out-of-dept (data 'sort-group 'out-of-dept))
(define&provide deceased (data 'sort-group 'deceased))
(define&provide end (data 'sort-group 'end))

(struct status data () #:transparent)
(define-syntax-rule (define-status kind)
  (define&provide kind (status 'kind #f)))
(repeat define-status
        emeritus
        sabbatical leave award-teaching
        start)

(struct position data () #:transparent)
(define-syntax-rule (define-position kind)
  (define&provide kind (position 'kind #f)))
(repeat define-position
        chair assoc-chair
        coord-ugrad coord-msit coord-grad
        coord-ugrad-asst
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

(define list-merger
  (λ (d y k kc)
    (hash-update d k (λ (okc) (cons kc okc)) empty)))

(define k->merger
  (hasheq
   'area list-merger))

(define (default-merger d y k kc)
  (hash-set d k kc))

(define (gather d y c)
  (match-define (data k kc) c)
  (define m (hash-ref k->merger k (λ () default-merger)))
  (m d y k kc))

(define (filter-past pid->pd the-pid c)
  (match-define (data k kc) c)
  (match k
    ;; There can be only one!
    [(or 'chair 'assoc-chair
         'coord-grad 'coord-ugrad 'coord-msit 'coord-navitas)
     (for/fold ([pid->pd pid->pd])
               ([(pid pd) (in-hash pid->pd)]
                #:unless (equal? pid the-pid))
       (hash-set pid->pd pid
                 (hash-remove pd k)))]
    [x
     #;(eprintf "no filter for ~a ~a\n" k kc)
     pid->pd]))

(define (process-ps ps)
  ;; ps is organized as a list of people with embedded year commands

  ;; We need to turn it into a list of years so we can filter
  (define y->pid->cs
    (for/fold ([last-y->pid->cs (hasheq)])
              ([p (in-list ps)]
               [pid (in-naturals)])

      (define-values (next-y->pid->cs last-cy)
        (for/fold ([last-y->pid->cs last-y->pid->cs] [cy 0])
                  ([c (in-list (person-info-content p))])
          (match c
            [(data 'year (? number? ny))
             (values last-y->pid->cs ny)]
            ["\n"
             (values last-y->pid->cs cy)]
            [_
             (values
              (hash-update last-y->pid->cs cy
                           (λ (ht)
                             (hash-update ht
                                          pid
                                          (λ (old) (cons c old))
                                          empty))
                           (λ () (hasheq)))
              cy)])))

      next-y->pid->cs))

  ;; Then we turn the list of years into a year to list of person data
  (define-values (y->pid->pd _)
    (for/fold ([last-y->pid->pd (hasheq)]
               [last-pid->pd (hasheq)])
              ([y (in-list (sort (hash-keys y->pid->cs) <=))])

      (define pid->cs (hash-ref y->pid->cs y))

      (define filtered-pid->pd
        (for*/fold ([last-pid->pd last-pid->pd])
                   ([(pid cs) (in-hash pid->cs)]
                    [c (in-list cs)])
          (filter-past last-pid->pd pid c)))

      (define next-pid->pd
        (for/fold ([next-pid->pd filtered-pid->pd])
                  ([(pid cs) (in-hash pid->cs)])
          (hash-update next-pid->pd pid
                       (λ (this-pd)
                         (for/fold ([this-pd this-pd])
                                   ([c (in-list cs)])
                           (gather this-pd y c)))
                       (λ () (hasheq)))))

      (values (hash-set last-y->pid->pd y next-pid->pd)
              next-pid->pd)))

  (for/hasheq ([(y pid->pd) (in-hash (hash-remove y->pid->pd 0))])
    (values y (hash-values pid->pd))))

;; Compiler
(require racket/runtime-path
         racket/string)

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
  ;; XXX Commitee page
  ;; https://teaching.cs.uml.edu/~heines/academic/department/Committees2014.jsp

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

(define (build-board! people%y)
  (define (extract t)
    (define (matches-t? p)
      (match t
        [(? symbol? k)
         (hash-has-key? p k)]
        [(cons k v)
         (equal? v (hash-ref p k #f))]))
    (filter matches-t? people%y))
  (define (extract1 t)
    (match (extract t)
      [(list x) x]
      [x (error 'extract1 "not 1 thing for ~e: ~e" t x)]))

  (define gc (extract1 'coord-grad))
  (define ac (extract1 'assoc-chair))
  (define ch (extract1 'chair))
  (define cc (extract1 'coord-ugrad))
  (define mi (extract1 'coord-msit))
  (define nv (extract1 'coord-navitas))
  (define (except-offices l)
    (sort
     (for/fold ([l l]) ([i (in-list (list gc ac ch cc mi nv))])
       (remove i l))
     string<=?
     #:key (λ (pd) (second (string-split (hash-ref pd 'n))))))

  (define fac (except-offices (extract (cons 'sort-group 'faculty))))
  (define st (except-offices (extract (cons 'sort-group 'staff))))
  (define adj (except-offices (extract (cons 'sort-group 'adjunct))))
  (define out (except-offices (extract (cons 'sort-group 'out-of-dept))))

  (define (board-info pd)
    (define (has k t)
      (and (hash-has-key? pd k) t))
    (define name (hash-ref pd 'n))
    (values (hash-ref pd 'prefix)
            name
            (format "img/~a.jpg" name)
            (or (has 'coord-grad "Graduate Coordinator")
                (has 'coord-ugrad "Undergraduate Coordinator")
                (has 'coord-ugrad-asst "Assistant Undergraduate Coordinator")
                (has 'assoc-chair "Associate Chair")
                (has 'chair "Chair")
                (has 'coord-msit "MS in IT Coordinator")
                (has 'coord-bsit "BS in IT Coordinator")
                (has 'coord-navitas "Navitas Coordinator")
                (hash-ref pd 'title #f))
            (hash-ref pd 'area #f)
            (and (not (eq? 'out-of-dept (hash-ref pd 'sort-group)))
                 (hash-ref pd 'email #f))))

  (define (board-html pd)
    (define-values (prefix name img t as em)
      (board-info pd))
    `(div ([class "entry"])
          (img ([src ,img]))
          (span ([class "name"])
                ,prefix nbsp ,name)
          ,(if t
             `(span ([class "title"]) ,t)
             "")
          ,(if as
             `(span ([class "research"])
                    ,@(for/list ([a (in-list as)])
                        `(span ,a)))
             "")))

  (define (latex! p . content)
    (local-require racket/file)
    (make-directory* build)
    (with-output-to-file
      (build-path build p)
      #:exists 'replace
      (λ ()
        (let loop ([c content])
          (cond
            [(pair? c)
             (loop (car c))
             (loop (cdr c))]
            [(string? c)
             (displayln c)])))))

  (define (board-tex pd)
    (define-values (prefix name img t as em)
      (board-info pd))
    (define r
      (if (file-exists? img)
        (format "\\BoardEntry{~a}{~a}{~a}{~a}{~a}{~a}"
                prefix name img (or t "")
                (string-join (or as empty) ", ")
                (or em ""))
        ""))
    (eprintf "~a\n" r)
    r)

  (latex!
   "board-content.tex"
   (list (board-tex ch)
         (board-tex ac)
         (board-tex gc)
         (board-tex cc)
         (board-tex mi)
         (board-tex nv))
   (map board-tex fac)
   (map board-tex st)
   (map board-tex adj)
   (map board-tex out))

  (output!
   "board.html"
   "UMass Lowell CS > Board"
   `(div ([class "menu"])
         (ul
          (li (a ([href "index.html"]) "Top"))))
   `(div ([class "board"])
         (span ([class "label"]) "Administration")
         (div ([class "group"])
              ,(board-html ch)
              ,(board-html ac)
              ,(board-html gc)
              ,(board-html cc)
              ,(board-html mi)
              ,(board-html nv))
         (span ([class "label"]) "Faculty")
         (div ([class "group"])
              ,@(map board-html fac))
         (span ([class "label"]) "Staff")
         (div ([class "group"])
              ,@(map board-html st))
         (span ([class "label"]) "Adjuncts")
         (div ([class "group"])
              ,@(map board-html adj))
         (span ([class "label"]) "Outside of Department")
         (div ([class "group"])
              ,@(map board-html out)))))

(define (build-faces! people%y)
  (output!
   "faces.html"
   "UMass Lowell CS > Faces"
   `(div ([class "menu"])
         (ul
          (li (a ([href "index.html"]) "Top"))))
   "XXX"
   `(pre
     ,(pretty-format people%y))))

(define (build-offices! people%y)
  (output!
   "offices.html"
   "UMass Lowell CS > Offices"
   `(div ([class "menu"])
         (ul
          (li (a ([href "index.html"]) "Top"))))
   "XXX"
   `(pre
     ,(pretty-format people%y))))

(define (build-research! people%y)
  (output!
   "research.html"
   "UMass Lowell CS > Research"
   `(div ([class "menu"])
         (ul
          (li (a ([href "index.html"]) "Top"))))
   "XXX"
   `(pre
     ,(pretty-format people%y))))

(define (build-extras! y people%y)
  (build-faces! people%y)
  (build-offices! people%y)
  (build-research! people%y)
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
    (printf "Building year ~a\n" y)
    (define people%y (hash-ref year->people y))
    (build-directory! py y ny people%y)
    (when (= y cay)
      (build-extras! y people%y)))
  (build-index! all-years cay))

;; XXX Face dir on 1st and 3rd
;; XXX Office dir on 2nd
