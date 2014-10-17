(module defs racket
; ---------------------------------------------------------------------------------------------------
; it is possible to define new functions:
(require racket scribble/core scribble/base scribble/html-properties)
(require (planet amkhlv/bystroTeX/common) (planet amkhlv/bystroTeX/slides))

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
)
