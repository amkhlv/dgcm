#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require (for-syntax (planet amkhlv/bystroTeX/slides_for-syntax)))
@(require (planet amkhlv/bystroTeX/common) (planet amkhlv/bystroTeX/slides))
@(require (only-in (planet jaymccarthy/sqlite) close))


@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(define bystro-conf 
   (bystro (find-executable-path "amkhlv-java-formula.sh")
           "diff-forms_formulas.sqlite"  ; name for the database
           "diff-forms_formulas" ; directory where to store .png files of formulas
           25  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@(begin ;do not change here:
   (define (start-formula-database)
       (configure-bystroTeX-using bystro-conf)
       (bystro-initialize-formula-collection bystro-conf))
   (define formula-database (start-formula-database)))
@; ---------------------------------------------------------------------------------------------------
@; This controls the single page mode:
@(define singlepage-mode #f)
@; ---------------------------------------------------------------------------------------------------
@; it is possible to define new functions:
@(begin
   (define (label s) (elemtag s (number-for-formula s)))
   (define (ref s) (elemref s (ref-formula s)))
   (define (red . x) (apply clr (cons "red" x)))
   (define (green . x) (apply clr (cons "green" x)))
   (define (greenbox-style more) 
     (bystro-elemstyle (string-append "border-style:solid;border-color:#00aa00;" more)))
   (define (redbox-style more)   
     (bystro-elemstyle (string-append "border-style:solid;border-color:#aa0000;" more)))
   (define (greenbox more . x) (elem #:style (greenbox-style more) x))
   (define (redbox   more . x) (para #:style (redbox-style more) x))
   (define (greenbox-wide more . x) (nested #:style (greenbox-style more) x))
   (define (redbox-wide   more . x) (nested #:style (redbox-style   more) x))
   (define (hrule) (element (make-style #f (list (alt-tag "hr"))) ""))
   (define (leftbar . x) 
     (para 
      #:style (bystro-elemstyle 
               "border-left-style:solid;border-color:green;padding-left:12px;") 
      x))
   )
@; ---------------------------------------------------------------------------------------------------
@; The basic syntax is somewhat tunable:
@(define-syntax (defineshiftedformula x) 
   (bystro-formula-syntax 
    #:autoalign-formula-prefix "f"
    #:manual-formula-prefix    "f"
    #:display-math-prefix      "equation"
    #:size-change-notation     "fsize"
    #:size-increase-notation   "fsize+"
    #:size-restore-notation    "fsize="
    x))
@; ---------------------------------------------------------------------------------------------------

@; ---------------------------------------------------------------------------------------------------
@; INITIALIZATION (please do not change anything in this part!):
@(unless (bystro-formula-processor bystro-conf)
   (error "*** could not find executable for formula processing ***"))
@(defineshiftedformula "formula-enormula-humongula!")
@(bystro-titlepage-init #:singlepage-mode singlepage-mode)
@; ---------------------------------------------------------------------------------------------------

@; ---------------------------------------------------------------------------------------------------
@; AND HOPEFULLY SOME CONTENT:


@(require racket/dict)
@(init-counter exercise)
@(define (ex-num label)
   (elemtag label (number->string (exercise-next label))))
@(define (ex-ref label)
   (elemref label (string-append "Exercise " (number->string (exercise-number label)))))


@title{Differential forms, integration, exterior derivatives}
We follow the book of Arnold closely. 

@bold{Exercise @ex-num{Pullback}:} Show that @f{F^*df = d(f\circ F)}

@bold{Exercise @ex-num{OrientationOfPolihedron}:} Show that @f{\int\limits_{\sigma} \omega = - \int\limits_{-\sigma}\omega}
where @f{-\sigma} is the singular polyhedron with the opposite orientation.

@bold{Exercise @ex-num{ExteriorProductIsDerivative}:}
Show that @f-1{d(\stackrel{p}{\omega}\wedge\stackrel{q}{\omega}) = 
(d\stackrel{p}{\omega})\wedge \stackrel{q}{\omega} + (-1)^{p}\stackrel{p}{\omega}\wedge d\stackrel{q}{\omega}
}

@bold{Exercise @ex-num{ConeOfChain}:}
In class we gave the construction of the cone @f{p(c)} for a singular chain @f{c}.  Prove that
@f{\partial p (c) + p (\partial c) = c}.

@linebreak[]
@linebreak[]
@hyperlink["../index.html"]{go back to main page}

@; ---------------------------------------------------------------------------------------------------
@close[formula-database]
