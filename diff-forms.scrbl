#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require (for-syntax "defs_for-syntax.rkt" (planet amkhlv/bystroTeX/slides_for-syntax)))
@(require "defs.rkt" (planet amkhlv/bystroTeX/common) (planet amkhlv/bystroTeX/slides))
@(require (only-in (planet jaymccarthy/sqlite) close))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(define bystro-conf   
   (bystro (find-executable-path "amkhlv-java-formula.sh")
           "diff-forms_formulas.sqlite"  ; name for the database
           "diff-forms" ; directory where to store .png files of formulas
           21  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           1   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@; This controls the single page mode:
@(define singlepage-mode #f)
@; ---------------------------------------------------------------------------------------------------
@(begin ;do not change anything here:
   (define-syntax (syntax-setter x) (defines-syntax-for-formulas x))                
   (syntax-setter defineshiftedformula)
   (defineshiftedformula "formula-enormula-humongula!"))
@; ---------------------------------------------------------------------------------------------------


@(bystro-inject-style "misc.css" "no-margin.css")

@title[#:style '(no-toc no-sidebar)]{Differential forms, integration, exterior derivatives}

@table-of-contents[]


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

 
  
