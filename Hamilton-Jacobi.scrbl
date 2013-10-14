#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require (for-syntax (planet amkhlv/bystroTeX/slides_for-syntax)))
@(require (planet amkhlv/bystroTeX/common) (planet amkhlv/bystroTeX/slides))
@(require (only-in (planet jaymccarthy/sqlite) close))


@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(define bystro-conf 
   (bystro (find-executable-path "amkhlv-java-formula.sh")
           "Hamilton-Jacobi_formulas.sqlite"  ; name for the database
           "Hamilton-Jacobi_formulas" ; directory where to store .png files of formulas
           25  ; formula size
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

@(init-counter theorem)
@(define (th-num label)
   (elemtag label (number->string (theorem-next label))))
@(define (th-ref label)
   (elemref label (string-append "Theorem " (number->string (theorem-number label)))))

@(init-counter defn)
@(define (defn-num label)
   (elemtag label (number->string (defn-next label))))
@(define (defn-ref label)
   (elemref label (string-append "Definition " (number->string (defn-number label)))))

@title{Hamilton-Jacobi theory}
@bystro-toc[]
@linebreak[]
@linebreak[]
@hyperlink["../index.html"]{go back to main page}

@slide["Quasilinear PDE" #:tag "QuasilinearPDE" #:showtitle #t]{

Consider a function @f{u(x,y)} of two variables @f{(x,y)\in {\bf R}^2}. 



@bold{Definition @defn-num{QPDE}:} 
A @spn[attn]{quasilinear partial differential equation} is an equation of the following form:
@equation[#:label "QuasilinearPDE"]{
   au_x + bu_y = c
}
where @f{a}, @f{b} and @f{c} are functions of @f{x,y,u}.

@bold{Definition @defn-num{CharacteristicCurvesOfQPDE}:}
@spn[attn]{Characteristic curves} are solutions of the following system of differential equations:
@align[r.l @list[
@f{{dx\over dt} = \;}@f{a}
]@list[
@f{{dy\over dt} =\;}@f{b}
]@list[
@f{{du\over dt} =\;}@f{c}
]]
@div[redbox]{
To construct a solution of the quasilinear PDE, it is enough
to specify a curve @f{(x(s), y(s), u(s))} and consider all the characteristic
curves passing through it. These curves will sweep the graph of @f{u(x,y)}.
}


}


@; ---------------------------------------------------------------------------------------------------

@close[formula-database]
