#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require (for-syntax (planet amkhlv/bystroTeX/slides_for-syntax)))
@(require (planet amkhlv/bystroTeX/common) (planet amkhlv/bystroTeX/slides))
@(require (only-in (planet jaymccarthy/sqlite) close))


@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(define bystro-conf 
   (bystro (find-executable-path "amkhlv-java-formula.sh")
           "classical-mechanics_formulas.sqlite"  ; name for the database
           "classical-mechanics_formulas" ; directory where to store .png files of formulas
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

@(init-counter theorem)
@(define (th-num label)
   (elemtag label (number->string (theorem-next label))))
@(define (th-ref label)
   (elemref label (string-append "Theorem " (number->string (theorem-number label)))))

@title{Symplectic and Poisson geometry}
@bystro-toc[]
@linebreak[]
@linebreak[]
@hyperlink["../index.html"]{go back to main page}

@slide["Action Principle" #:tag "ActionPrinciple" #:showtitle #t]{
Classical mechanics studies extrema of the @bold{action functional}:
@equation[#:label "ActionFunctional"]{
S[x(t)] = \int_{t_{in}}^{t_{fin}} dt\; L(x(t),\dot{x}(t),\ddot{x}(t),\ldots) 
}
The @bold{Lagrangian} @f{L} should depend on the trajectory @f{x(t)} and its derivative up to some @bold{finite order}. 

@div[redbox]{ The @spn[attn]{action principle} says that a classical trajectory @f{x_{cl}(x)} extremizes the action.  }

But we have to be more precise and explain what the word ``extremizes'' means. If the Lagrangian only depends on 
@f{x} and @f{\dot{x}} @bold{(no higher derivatives)} then the statement is:
@equation{
S[x_{cl}(t) + \varepsilon \delta x(t)] - S[x_{cl}(t)]  = o(\varepsilon) \quad \mbox{ when } \delta x(t_{in}) = \delta x(t_{fin}) = 0
}
Notice that we consider @bold{arbitrary} variations, in the sense that @f{x_{cl}(t) + \varepsilon \delta x(t)} does not
have to satisfy any equations of motion. But it is essential that @f{\delta x(t_{in}) = \delta x(t_{fin}) = 0}. This condition
puts a restriction on the class of trajectories ``admitted to the competition''.

In case if the Lagrangian contains higher derivatives @f{\ddot{x}}, @f{\dddot{x}}, ... the action principle requires
even stronger restriction on the allowed variations. Suppose that:
@equation{
L = L\left(x, {dx\over dt},\ldots,{d^n x\over dt^n}\right)
}
Then the variation of the action on the classical trajectory is zero provided that:
@equation[#:label "BoundaryConditionsOnVariation"]{
{d^k x\over dt^k}(t_{in}) = {d^k x\over dt^k}(t_{fin}) = 0 \quad \mbox{ for } k\in \{0,1,\ldots,n-1\}
}

}

@slide["Phase space" #:tag "PhaseSpace" #:showtitle #t]{
Definition:
@centered{
@tabular[@list[@list[@hspace[10]
@div[redbox]{Phase space is the space of all classical trajectories}
@hspace[7]]]]
}

This is a finite-dimensional manifold. If @f+1{x} has @f+1{n} component and the action depends only on
@f{x} and @f{\dot{x}}, then the dimension of the phase space is @f{2n}. 

If the action depends on higher  derivatives of @f{x} up to order @f{d^kx\over dt^k}, then the dimension
of the phase space is typically @f{n(k+1)}.
}

@slide["Symplectic Structure" #:tag "SymplecticStructure" #:showtitle #t]{
We will now explain that the phase space is naturally endowed with a symplectic structure.

To introduce the symplectic structure, we will consider @bold{the dependence of the action on the trajectory}.

@table-of-contents[]

@section{Dependence of action on trajectory}
As we explained, the variation of the action on the classical trajectory is zero once the variation 
satisfies the boundary conditions (@ref{BoundaryConditionsOnVariation}). Let us now relax those boundary
conditions. 

With the boundary conditions relaxed, we find:
@align[r.l.n @list[
"" @f{S[x_{cl}(t) + \varepsilon \delta x(t)] - S[x_{cl}(t)]  \;=\; } @label{DefAlpha1}
]@list[
@f{=\;} 
@f{\left.\left(
\alpha^{(0)}_m(t) \delta x^m(t) + \alpha^{(1)}_m(t) \delta \dot{x}^m(t) + \ldots + 
\alpha^{(k-1)}_m(t)  {d^{k-1}\delta x\over dt^{k-1}}^m(t) 
\right)\right|^{t_{fin}}_{t_{in}}}
""
]]
where @f{\alpha^{(j)}(t) = \alpha^{(j)}\left(x(t),\dot{x}(t),\ldots, {d^k x\over dt^k}\right)}
are some functions. This equation follows from integration by parts.

Eq. (@ref{DefAlpha1}) is the definition of 
functions @f{\alpha^{(j)}\left(x(t),\dot{x}(t),\ldots, {d^k x\over dt^k}\right)}. At first glance, it
would seem that we have only defined the @italic{difference} @f{\alpha(t_{fin}) - \alpha(t_{in})} and
therefore there is some ambiguity in the definition. But in fact there is no such ambiguity.
We can choose @f{\delta x(t)} to be zero in the neighborhood of @f{t=t_{in}}, then the right
hand side of (@ref{DefAlpha1}) will only contain  @f{\alpha(t_{fin})}. Notice that we have not
required (so far) that @f{\delta x(t)} satisfy any equations of motion. It could be completely
arbitrary (the variation is ``off-shell''). 

@section{Action as a function on the phase space}
Now consider the particular case when the variation @f{\delta x(t)} is ``on-shell'', in the following
sense: require that @f{x(t)+\varepsilon\delta x(t)} be a solution of the classical equations of motion
up to the terms of the order @f{o(\varepsilon)}. In this case @f{\delta x(t)} can be interpreted
as a tangent vector to the space of classical trajectories, @italic{i.e.} a 
@bold{tangent vector to the phase space}. Therefore the functions @f{\alpha^{(j)}(t)} define a 
@bold{1-form on the phase space}:
@equation{
\alpha_t(\delta x_{cl}) = \sum_{j=0}^{k-1} \alpha^{(j)}_m(t){d^j \delta x^m(t)\over dt^j}
}
It is important to remember that this 1-form depends on time @f{t}, and therefore we 
will (sometimes) explicitly put a subindex @f{t}, @italic{i.e.} @f{\alpha = \alpha_t}.

@section{Symplectic form}
It turns out the @bold{exterior derivative of @f{\alpha_t} does not depend on @f{t}}:
@equation{
\omega = d\alpha_t
}
--- we can think of it as a @bold{``form-valued integral of motion''}. We therefore defined
a 2-form @f{\omega} on the phase space. It is called the @bold{canonical symplectic form}.
}

@slide["Noether's theorem" #:tag "NoetherTheorem" #:showtitle #t]{
A @spn[attn]{symmetry} is an @bold{off-shell} transformation which leaves the action invariant. This means that the
Lagrangian changes by a total derivative. We will first consdider an important particular case when the Lagrangian is just invariant,
and then the general case when the Lagrangian changes by a total derivative. 

@table-of-contents[]

@section{Case when the Lagrangian is invariant}
Let us consider some transformation @f{\xi} of the trajectories, which leaves the Lagrangian invariant:
@equation[#:label "LagrangianIsInvariant"]{
\delta_{\xi} L = \sum_{j=0}^k {\partial L\over \partial \left({d^jx^m\over dt^j}\right)}  {d^j(\delta_{\xi}x^m)\over dt^j} = 0
}
This implies that: 
@align[r.l.n @list[
@f{{\cal L}_{\xi}\alpha_t \;=\;} @f{0 \quad \mbox{ for any } t} @label{AlphaIsInvariant}
]@list[
@f{\iota_{\xi}\alpha_{t_{fin}} - \iota_{\xi}\alpha_{t_{in}} \;=\;} @f{0} @label{AlphaIsConstant}
]]
Eq. (@ref{AlphaIsConstant}) implies that @f{\iota_{\xi}\alpha_t} does not depend on @f{t}.
Eq. (@ref{AlphaIsInvariant}) implies that:
@equation{
\iota_{\xi} \omega = -d (\iota_{\xi}\alpha)
}
This means that such a transformation will be a @bold{Hamiltonian vector field} generated by the Hamiltonian @f{F_{\xi} = \iota_{\xi}\alpha}.

@section{Case when the Lagrangian changes by a total derivative}
This is a little bit more complicated. Suppose that instead of (@ref{LagrangianIsInvariant}) we get:
@equation{\delta_{\xi} L = {d\Phi\over dt}}
(off-shell!) for some @f{\Phi = \Phi(x,\dot{x},\ldots)}.

This implies that the following function on the phase space:
@equation[#:label "HamiltonianWhenLGetsTotalDerivative"]{F_{\xi} = \iota_{\xi}\alpha_t - \Phi(t)}
does not depend on @f{t}.

Next, let us calculate @f{{\cal L}_{\xi}\alpha_t}. Consider @bold{any} variation @f{\delta_{\eta}} (off-shell)
which vanishes in the vicinity of @f{t_{in}}. We have, by definition: 
@equation{
\alpha_{t_{fin}}(\eta) = \left.{d\over d\tau}\right|_{\tau=0} S[g^{\tau}_{[\eta]}\;\;x]
}
Therefore:
@align[r.l.n @list[
@f{({\cal L}_{\xi}\alpha)(\eta) \; = \;}
@f{\left.{d\over d\sigma}\right|_{\sigma=0} \alpha(g^{\sigma}_{[\xi]*}\;\;\eta) = 
\left.{d\over d\sigma}\right|_{\sigma=0} \left.{d\over d\tau}\right|_{\tau=0} S[g^{\sigma}_{[\xi]}\;g^{\tau}_{[\eta]}\;x] =}
""
]@list[
@f{\; = \;}
@f{\left.{d\over d\tau}\right|_{\tau=0} \Phi(g_{[\eta]}^{\tau}\;\;x)(t_{fin}) = d\Phi(\eta)|_{t=t_{fin}}}
""
]]
Returning to (@ref{HamiltonianWhenLGetsTotalDerivative}), we get:
@equation{
d F_{\xi} = -\iota_{\xi}\omega
}
Again, we conclude that @f{\xi} is a Hamiltonian vector field generated by @f{F_{\xi}}.

@section{Example: Energy}
For example, consider:
@equation{(\delta_{\xi} x)(t) = {dx\over dt}}
--- time translation is a symmetry!
In this case, we obtain:
@equation{
F_{\xi} = \alpha\left({dx\over dt}\right) - L
}
This is the @bold{energy} of the system.
}

@; ---------------------------------------------------------------------------------------------------

@close[formula-database]
