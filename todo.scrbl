#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require "defs_for-syntax.rkt" (for-syntax bystroTeX/slides_for-syntax))
@(require "defs.rkt" bystroTeX/common bystroTeX/slides)
@(require (only-in db/base disconnect))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(define bystro-conf   
   (bystro (bystro-connect-to-server #f "127.0.0.1" 29049 "svg")
           "todo/formulas.sqlite"  ; name for the database
           "todo" ; directory where to store .png files of formulas
           21  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           1   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@; This controls the single page mode:
@(define singlepage-mode #t)
@; ---------------------------------------------------------------------------------------------------


@(bystro-def-formula "formula-enormula-humongula!")

@; ---------------------------------------------------------------------------------------------------


@(bystro-inject-style "misc.css" "no-margin.css")

@title[#:style '(no-toc no-sidebar)]{TODO list}



@itemlist[#:style 'ordered
@item{Explain notations @f{\Gamma(\Lambda^mTX)} @italic{etc.}}
@item{Prove the Frobenius criterium; introduce the Frobenius form}
]

@; ---------------------------------------------------------------------------------------------------

@disconnect[formula-database]
