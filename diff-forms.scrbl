#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)

@(require "defs.rkt" bystroTeX/common bystroTeX/slides (for-syntax bystroTeX/slides_for-syntax))
@(require (only-in db/base disconnect))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(define bystro-conf   
   (bystro (bystro-connect-to-server #f "127.0.0.1" 29049 "svg")
           "diff-forms/formulas.sqlite"  ; name for the database
           "diff-forms" ; directory where to store .png files of formulas
           21  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           1   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@(set-bystro-extension! bystro-conf "svg")
@; This controls the single page mode:
@(define singlepage-mode #f)
@; ---------------------------------------------------------------------------------------------------


@(bystro-def-formula "formula-enormula-humongula!")

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

@disconnect[formula-database]

 
  
