#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require (for-syntax "defs_for-syntax.rkt" (planet amkhlv/bystroTeX/slides_for-syntax)))
@(require "defs.rkt" (planet amkhlv/bystroTeX/common) (planet amkhlv/bystroTeX/slides))
@(require (only-in (planet jaymccarthy/sqlite) close))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(define bystro-conf 
   (bystro (find-executable-path "amkhlv-java-formula.sh")
           "integrability_formulas.sqlite"  ; name for the database
           "integrability" ; directory where to store .png files of formulas
           25  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@; This controls the single page mode:
@(define singlepage-mode #f)
@; ---------------------------------------------------------------------------------------------------
@(begin ;do not change anything here:
   (define (start-formula-database)
     (configure-bystroTeX-using bystro-conf)
     (bystro-initialize-formula-collection bystro-conf))
   (define formula-database (start-formula-database))
   (unless (bystro-formula-processor bystro-conf)
     (error "*** could not find executable for formula processing ***"))
   (define-syntax (syntax-setter x) (defines-syntax-for-formulas x))                
   (syntax-setter defineshiftedformula)
   (defineshiftedformula "formula-enormula-humongula!")
   (bystro-titlepage-init #:singlepage-mode singlepage-mode))
@; ---------------------------------------------------------------------------------------------------

@title{Liouville integrability}
@bystro-toc[]
@linebreak[]
@linebreak[]
@hyperlink["../index.html"]{go back to main page}

@slide["Integrals of motion in involution" #:tag "IntegralsOfMotion" #:showtitle #t]{
Let us consider a Hamiltonian system with the @f{2n}-dimensional phase space @f{M}, with the 
Hamiltonian @f{H}.

An integral of motion is a function @f{F} @bold{in involution} with the Hamiltonian, @italic{i.e.}:
@equation{
\{ H , F \} = 0
}

Suppose that there are @f{n} integrals of motion @f{F_1,\ldots,F_n} @bold{all in involution with each other}:
@equation{
\{F_i,F_j\} = 0
}
but at the same time @bold{functionally independent}, @italic{i.e.} the differentials @f{dF_1,\ldots,dF_n}
are linearly independent at each point. (The Hamiltonian can be one of them, or a function of them.)

For a list of values @f{f = (f_1,\ldots,f_n)}, consider the @bold{common level set}:
@equation[#:label "LevelSet"]{
M_f  = \{x\; | \; F_i(x) = f_i\;,\; i=1,\ldots,n\}
}
Notice that @f{M_f} has the following properties: 
@itemlist[
@item{it is invariant under the Hamiltonian evolution, @italic{i.e.} a trajectory starting
at any point @f{x\in M_f} remains inside @f{M_f}}
@item{it is a Lagrangian manifold, @italic{i.e.} the restriction of @f{\omega} on @f{M_f} is zero}
]
Suppose also that @f{M_f} is compact and connected. Then, the following is true:
@itemlist[#:style 'ordered
@item{@f{M_f} is a torus @f{{\bf T}^n} (this is the easy part)}
@item{In the vicinity of @f{M} exist @f{n} functions @f{I_j = I_j(F_1,\ldots,F_n)} such that for 
every @f{I_j} the corresponding Hamiltonian
vector field @f{\xi_j} has periodic trajectories with the period @f{2\pi}:
@equation{g_{[\xi_j]}^{2\pi}(x) = x}
These functions @f{I_j} are called @spn[attn]{action variables}
}]

}

@slide[@elem{Proof that @f{M_f} is a torus} #:tag "MfIsTorus" #:showtitle #t]{
The proof is in Arnold's book.
}

@slide["Existence of action variables" #:tag "ActionVariables" #:showtitle #t]{
We have therefore a family of tori, with the following additional structure.
On each torus, we have a notion of ``straight line''. Indeed, these 
``straight lines'' are orbits of the action of linear combinations of @f{F_i}. 
Locally, this is the same as to say that we are given the action on @f{{\bf R}^n}  
on each @f{M_f}. In particular, suppose that @f{\{e_i|i\in\{1,\ldots,n\}\}} form a basis 
in @f{H_1(M_f)}. We will now prove that for every @f{i}, @f{e_i} is a Hamiltonian
vector field (that is, @f{{\cal L}_{e_i}\omega  = 0}).

Let us denote @f{\xi_k} the Hamiltonian vector field corresponding to @f{F_k}:
@equation{
   \iota_{\xi_k}\omega = dF_k
}
There exists @bold{periodic combinations}:
@equation{
   e_k  =  X_k^j(F\,)\xi_j
}
where @f-2{X_k^j(F\,)} are constant on the tori, @italic{i.e.} depend only on @f{F}. 
This immediately implies:
@equation{
   \iota_{e_k}\omega = X^j_k(F\,) dF_j
}
In  particular, we see that:
@equation{
   {\cal L}_{e_i} (\iota_{e_k} \omega) = 0
}
This immediately implies (by taking @f{d}):
@equation[#:label "LLIsZero"]{
   {\cal L}_{e_i} {\cal L}_{e_k}\omega = 0
}
This, plus periodicity, implies:
@equation{
   {\cal L}_{e_i} \omega = 0
}
Indeed, (@ref{LLIsZero}) with @f{i=k} implies that @f{(g^t_{[e_k]}\;\;\,)^*\omega} is linear in @f{t}:
@equation{
   (g^t_{[e_k]}\;\;\,)^*\omega = \omega + t\sigma
}
This could only be periodic in @f{t} when @f{\sigma=0}. 

We conclude that @f{e_k} is, in fact, a Hamiltonian vector field. What is the
corresponding Hamiltonian? Being constant on @f{M_f}, it is equal to its average over the period:
@equation{
   I_k = {1\over 2\pi} \oint I_k dt
}
After these observations, to calculate @f{I_k}, it is enough to calculate its
variation from one torus to another. Which is equal to the variation of:
@equation{
   {1\over 2\pi} \int_{D_k} \omega
}
This proves that the action variable is given by the formula:
@equation{
I_k = {1\over 2\pi}\int_{S^k} p_jdq^j 
}

}

@slide["Perturbation theory" #:tag "PerturbationTheory" #:showtitle #t]{
Consider perturbation of the integrable Hamiltonian:
@equation{
   H = H_0(I) + \sum V_{\vec{m}}(I) e^{i(\vec{m}\cdot\vec{\phi})}
}
Ask ourselves the question:
@centered{
@spn[redbox]{
@larger{Can we make this Hamiltonian integrable by a change of variables?}
}}
Perhaps by a change of variables @f-2{(I,\phi)\rightarrow (\tilde{I},\tilde{\phi})} we can bring it to the form:
@equation{
   \tilde{H}(\tilde{I}\;)
}
But we have to remember that the change of variables should preserve also the
symplectic form:
@equation{
   dI\wedge d\phi = d\tilde{I}\wedge d\tilde{\phi}
}
In other words, it should be a @bold{canonical transformation}.

Notice that an @bold{infinitesimal} canonical transformation is the same as the
Hamiltonian vector field. What about small (but not infinitesimal) canonical
transformation. A @bold{perturbative} canonical transformation has a series
expansion:
@align[r.l @list[
@f{\tilde{I} =\;}@f{I + \varepsilon \Delta_1 I + \varepsilon^2 \Delta_2 I + \ldots}
]@list[
@f{\tilde{\phi} =\;}@f{\phi + \varepsilon \Delta_1 \phi + 
\varepsilon^2 \Delta_2 \phi + \ldots}
]]
In particular, for it to be canonical at the first order @f{\varepsilon}, it has to be:
@align[r.l @list[
@f{\Delta_1I =\;}@f{\{\Psi,I\}}
]@list[
@f{\Delta_1\phi =\;}@f{\{\Psi,\phi\}}
]]
for some @f{\Psi}. Generally speaking:
@align[r.l @list[
@f{\tilde{I} = \;}@f{\mbox{ exp }\left( \varepsilon \xi_{\Psi} \right)\;I}
]@list[
@f{\tilde{\phi} = \;}@f{\mbox{ exp }\left( \varepsilon \xi_{\Psi} \right)\;\phi}
]]
Generally speaking @f{\Psi} has also some expansion in powers of @f{\varepsilon}. 

We have to find @f{\Psi(\varepsilon)} such that:
@equation{
   \mbox{exp }(\varepsilon \xi_{\Psi(\varepsilon)}) \; H \mbox{ does not contain } \phi
}
To the first order in @f{\varepsilon} we get:
@equation{
   (\vec{n}\cdot\vec{\omega})\Psi_{\vec{n}} + V_{\vec{n}} = 0
}
where @f{\omega = {\partial H\over\partial I}}. If @f{\vec{\omega}} is irrational then there is no problem. But if @f{\vec{\omega}} is 
rational, then the @bold{resonance} happens.
}

@; ---------------------------------------------------------------------------------------------------

@close[formula-database]
