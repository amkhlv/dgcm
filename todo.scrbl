#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require (for-syntax "defs_for-syntax.rkt" (planet amkhlv/bystroTeX/slides_for-syntax)))
@(require "defs.rkt" (planet amkhlv/bystroTeX/common) (planet amkhlv/bystroTeX/slides))
@(require (only-in (planet jaymccarthy/sqlite) close))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(define bystro-conf   
   (bystro (find-executable-path "amkhlv-java-formula.sh")
           "todo_formulas.sqlite"  ; name for the database
           "todo" ; directory where to store .png files of formulas
           25  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@; This controls the single page mode:
@(define singlepage-mode #t)
@; ---------------------------------------------------------------------------------------------------
@(begin ;do not change anything here:
   (define-syntax (syntax-setter x) (defines-syntax-for-formulas x))                
   (syntax-setter defineshiftedformula)
   (defineshiftedformula "formula-enormula-humongula!"))
@; ---------------------------------------------------------------------------------------------------


@(bystro-inject-style "misc.css" "no-margin.css")

@title[#:style '(no-toc no-sidebar)]{TODO list}



@itemlist[#:style 'ordered
@item{Explain notations @f{\Gamma(\Lambda^mTX)} @italic{etc.}}
@item{Prove the Frobenius criterium; introduce the Frobenius form}
]

@; ---------------------------------------------------------------------------------------------------

@close[formula-database]
