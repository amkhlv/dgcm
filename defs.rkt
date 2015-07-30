(module defs racket

  (require (for-syntax racket/base bystroTeX/slides_for-syntax))
  (require racket scribble/core scribble/base scribble/html-properties)
  (require bystroTeX/common bystroTeX/slides)

  ;; Here the basic syntax can be adjusted:
  (provide bystro-def-formula)
  (define-syntax (bystro-def-formula stx)
    (bystro-formula-syntax 
     #:autoalign-formula-prefix "f"
     #:manual-formula-prefix    "f"
     #:display-math-prefix      "equation"
     #:size-change-notation     "fsize"
     #:size-increase-notation   "fsize+"
     #:size-restore-notation    "fsize="
     #:max-size-increase        8
     #:max-size-decrease        5
     #:max-vert-adjust          8
     stx))


  ;; Here we define new functions:
  (provide label)
  (define (label s) (elemtag s (number-for-formula s)))  
  (provide ref)
  (define (ref s) (elemref s (ref-formula s)))
  (provide red)
  (define (red . x) (apply clr (cons "red" x)))
  (provide green)
  (define (green . x) (apply clr (cons "green" x)))
  (provide greenbox-style)
  (define (greenbox-style more) 
    (bystro-elemstyle (string-append "border-style:solid;border-color:#00aa00;" more)))
  (provide redbox-style)
  (define (redbox-style more)   
    (bystro-elemstyle (string-append "border-style:solid;border-color:#aa0000;" more)))
  (provide greenbox)
  (define (greenbox more . x) (elem #:style (greenbox-style more) x))
  (provide redbox)
  (define (redbox   more . x) (elem #:style (redbox-style more) x))
  (provide greenbox-wide)
  (define (greenbox-wide more . x) (nested #:style (greenbox-style more) x))
  (provide redbox-wide)
  (define (redbox-wide   more . x) (nested #:style (redbox-style   more) x))
  (provide hrule)
  (define (hrule) (element (make-style #f (list (alt-tag "hr"))) ""))
  (provide leftbar)
  (define (leftbar . x) 
    (para 
     #:style (bystro-elemstyle 
              "border-left-style:solid;border-color:green;padding-left:12px;") 
     x))
                                        ; some more definitions:
  (require racket/dict)
  (init-counter exercise)
  (init-counter theorem)
  (provide ex-num ex-ref th-num th-ref defn-num defn-ref gbl gbr)
  (define (ex-num label)
    (elemtag label (exercise-next label)))
  (define (ex-ref label)
    (elemref label (list "Exercise "  (exercise-number label))))
  (define (th-num label)
    (elemtag label (theorem-next label)))
  (define (th-ref label)
    (elemref label (list "Theorem "  (theorem-number label))))
  (define (defn-num label)
    (elemtag label (exercise-next label)))
  (define (defn-ref label)
    (elemref label (list "Exercise "  (exercise-number label))))
  (define (gbl) (string-append 
                 "[" 
                 "\\! "
                                        ;"\\hspace{-" 
                                        ;(number->string (- (quotient (bystro-formula-size bystro-conf) 5) 1))  
                                        ;"pt}" 
                 "["))
  (define (gbr) (string-append 
                 "]" 
                 "\\! "
                                        ;"\\hspace{-" 
                                        ;(number->string (- (quotient (bystro-formula-size bystro-conf) 5) 1))  
                                        ;"pt}" 
                 "]"))

  (provide comment)
  (define-syntax (comment stx)
    (syntax-case stx ()
      [(_ x ...)
       #'(let ([a (bystro-bg 240 240 255)]
               [b (nested 
                   #:style (style "comment" 
                             (list (make-attributes '((style . "background-color:rgb(240,240,255);")))))
                   x ...)]
               [c (bystro-bg 255 255 255)])
           b)]))


  )
