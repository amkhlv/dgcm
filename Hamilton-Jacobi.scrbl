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
   )
@(require racket/dict)

@; We define some counters:
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

@title{Hamilton-Jacobi theory}
@bystro-toc[]
@linebreak[]
@linebreak[]
@hyperlink["../index.html"]{go back to main page}

@slide["Quasilinear PDE" #:tag "QuasilinearPDE" #:showtitle #t]{

Consider a function @f{u(x,y)} of two variables @f{(x,y)\in {\bf R}^2}. 

@bold{Definition @defn-num{QPDE}:} 
We will consider, as an illustrative example, the case of QPDE in two dimensions.

A @spn[attn]{quasilinear partial differential equation} (in 2d) is an equation of the following form:
@equation[#:label "QuasilinearPDE"]{
   au_x + bu_y = c
}
where @f{a}, @f{b} and @f{c} are functions of @f{x,y,u}.

@bold{Definition @defn-num{CharacteristicCurvesOfQPDE}:}
@spn[attn]{Characteristic curves} are solutions of the following system of differential equations:
@align[r.l.n @list[
@f{{dx\over dt} = \;}@f{a(x,y,u)} ""
]@list[
@f{{dy\over dt} =\;}@f{b(x,y,u)} @label{CharCurves}
]@list[
@f{{du\over dt} =\;}@f{c(x,y,u)} ""
]]
The @bold{characteristic vector field} is:
@equation[#:label "DefCharacteristicVectorField"]{
v = a {\partial\over\partial x} + b {\partial\over\partial y} + c{\partial\over\partial u}
}
@div[redbox]{
To construct a solution of the quasilinear PDE, it is enough
to specify a curve @f{(x(s), y(s), u(s))} and consider all the characteristic
curves passing through it. These curves will sweep the graph of @f{u(x,y)}.
}
@div[comment]{To be more precise: the initial curve @f{(x(s), y(s), u(s))} should be non-characteristic,
@italic{i.e.} the characteristic curves should @bold{not} be tangent to it. (Otherwize we would not get
a two-dimensional surface.)}
Let us prove it.
Consider a given solution @f{u = u_0(x,y)} of (@ref{QuasilinearPDE}). Let us look at the
@spn[attn]{graph} of this solution. The graph is the surface  @f{\Sigma \subset {\bf R}^3_{x,y,u}\;\;\;\;} given by the
equation:
@equation[#:label "DefGraph"]{
\Sigma\;:\;\; u - u_0(x,y) = 0
}
The statement follows immediately from the following reformulation:

@bold{Lemma.} Eq. (@ref{QuasilinearPDE}) is equivalent to the statement that the
characteristic vector field (@ref{DefCharacteristicVectorField}) is tangent to the graph (@ref{DefGraph}):
@equation[#:label "VTangentToSigma"]{
v\in T\;\Sigma
}
@bold{Proof.} This is equivalent to the following statement:
@itemlist[
@item{If @f{\Phi(x,y,u) = 0} is an equation determining the surface @f{\Sigma}, then @f{{\cal L}_v\Phi =\alpha\Phi}
where @f{\alpha = \alpha(x,y,u)} is some function.}
]
@(fsize+ (- 3))
@div[comment]{That is to say, the Lie derivative of @f{\Phi} along @f{v} should be zero on the surface @f{\Phi = 0}}
@(fsize=)
In particular, we can choose @f{\Phi = u - u_0(x,y)}. Evaluation of @f{{\cal L}_v\Phi} gives:
@equation{
{\cal L}_v \Phi(x,y,u) = c(x,y,u) 
- a(x,y,u) {\partial u_0(x,y)\over \partial x} - b(x,y,u) {\partial u_0(x,y)\over \partial y}
}
Vanishing of this expression on the surface @f{u = u_0(x,y)} is equivalent to  (@ref{QuasilinearPDE}).


}

@slide["Contact geometry" #:tag "ContactManifolds" #:showtitle #t]{

@table-of-contents[]

Our goal is to find some analogue of the characteristic curves for the nonlinear PDE:
@equation[#:label "NonlinearPDE"]{
F\left(x^{\mu},u, {\partial u\over\partial x^{\mu}}\right) = 0
}
We will need some new geometrical concept: the notion of  @bold{contact structure}. 

@section{Definition of contact manifold}

Let @f{M} be a @f{2n+1}-dimensional 
manifold. 

@subsection{Distribution of planes}
A contact structure is a distribution of @f{2n}-dimensional planes:
@(fsize+ (- 3))
@equation[#:label "DistributionOfPlanes"]{L\subset TM}
satisfying a certain @bold{nondegeneracy condition}.
@div[comment]{A subspace of dimension @f{k} in a @f{k+1}-dimensional space is sometimes called a ``hyperplane'',
or a ``plane of the codimension 1''. Eq. (@ref{DistributionOfPlanes}) means that for every point @f{x\in M} we have
a hyperplane @f{L_x \subset T_xM}.}
@(fsize=)
@subsection{Nondegeneracy condition}
Let us choose a 1-form @f{\alpha} such that @f{L = {\rm ker} \alpha}. There are many ways
to choose such @f{\alpha}, because we can replace: 

@equation[#:label "AmbiguityInAlpha"]{\alpha \to f\alpha} 

where @f{f} is any nowhere-zero
function. What we are going to say does will not depend on this ambiguity. 

The above-mentioned nondegeneracy condition is:
@equation[#:label "NondegeneracyOfContactStructure"]{
{\rm det}\left( d\alpha|_{{\rm ker}(\alpha)}\right) \neq 0
}
This means that @f{L} is maximally nonintegrable. The condition (@ref{NondegeneracyOfContactStructure}) is 
included in the definition of the contact structure. 

@section{Legendrian manifolds}
Because of the non-degeneracy condition, the distribution @f{L = {\rm ker}(\alpha)} does not have integral manifolds
of dimension @f{2n}. What about integral manifolds of the dimension @f{<2n}, would they exist?
It turns out that, under the condition (@ref{NondegeneracyOfContactStructure}),  the maximal dimension of an integral 
manifold is actually @f{n}.

@bold{Definition @defn-num{LegendrianManifold}:}
@spn[attn]{Legendrian manifolds} are integral manifolds of @f{L} of the dimension @f{n}.

@section{How to build Legendrian manifolds}
@subsection{Continuation problem}
Suppose that we are given an integral manifold @f{K} of the dimension @f{n-1}, and we want to
somehow continue it to a Legendrian manifold. In other words, we want to find an integral manifold @f{L}
of the dimension @f{n}, such that @f{K\subset L}. Is such a continuation possible?

@div[comment]{
This would be somewhat similar to the analytic continuation of a holomorphic surface, @italic{e.g.}
of the graph of a holomorphic function. To reconstruct a holomorphic surface of the real dimension 2, 
it is enough to know a curve inside the surface. Then 
we can act on the tangent vector to the curve by a complex structure; this gives some transverse
direction; we can then ``grow'' the surface in that transverse direction. This provedes a reconstruction
of the surface from the curve, which is usually called ``analytic continuation''.
}

It turns out that the reconstruction of @f{K} from @f{L} is generally speaking possible, but 
@bold{not unique}. But it is possible to achieve uniqueness, if we impose an additional restriction.
Suppose that we are @bold{given} a smooth function @f{F:\; M\to {\bf R}}, and the restriction of
this function on @f{K} is zero. Let us ask ourselves:

@div[redbox]{Is it possible to extend @f{K} to a Legendrian submanifold @f{L} so that @f{F} be identically zero everywhere on @f{L}?}

It turns out that this problem has (under some generality conditions on @f{K}) a unique solution.
The solution is obtained by the @bold{method of characteristics}. 

@subsection{Characteristics}
Characteristics are some curves which are constructed in the following way. Let us define the vector field @f{\xi} 
by the following two conditions:
@align[r.l.n @list[
@f{\xi \;\in\;} @f{{\rm ker}(\alpha)} @label{XiInKerAlpha}
]@list[
@f{(\iota_{\xi}(d\alpha))|_{{\rm ker}(\alpha)} \; = \;} @f{dF|_{{\rm ker}(\alpha)}} @label{IotaXiDAlpha}
]]
This is called @bold{characteristic vector field}. The trajectories of the flux generated by this vector field
are called @bold{characteristics}. 

@(fsize+ (- 3))
@div[comment]{
Notice that the characteristc vector field @italic{will change}
if we change @f{\alpha} as in (@ref{AmbiguityInAlpha}). But, the change will be @f-3{\xi\to {1\over f} \xi} and therefore
the @italic{direction} of @f{\xi} will not change. Therefore, this will not affect the characteristics.
}@(fsize=)

@subsection{Construction of @f{L}}
@tabular[@list[@list[
@para{Let us pass a characteristic through every point of @f{K}. 
They will all sweep an @f{n}-dimensional submanifold @f{L}. We will now prove that @bold{this} @f{L} @bold{is Legendrian}, and
also that @f{F} is zero on @f{L}.
We just have to prove that the restriction of @f{\alpha} on @f{TL} is zero.
In other words @f{\alpha(v) = 0} for any @f{v} tangent to @f{L}. 
}
@(image (string->path "scans/LegendrianManifold.png"))
]]]

@subsection{Proof that @f{L} is Legendrian}
Let us start with the following observation:
@equation{
{\cal L}_{\xi}F = \iota_{\xi}dF = 0
}
and @f{F} is zero on @f{K}. This implies that @f{F} is zero on @f{L}. 

Also, let us observe that Eq. (@ref{IotaXiDAlpha}) can be re-written in the following very useful way:
@equation[#:label "HamiltonianModuloAlpha"]{
\iota_{\xi}(d\alpha) = dF + \rho \alpha
}
where @f{\rho = \rho(x)} is some function on @f{M}.

Let us restrict @f{\alpha}  on the submanifold @f{L}, then we can consider the restricted @f{\alpha} as a 1-form on @f{L},
which we will call @f{\alpha_{\rm res}}. What we have to prove, is that @f{\alpha_{\rm res} = 0}. Let us prove it.
As the vector field @f{\xi} is tangent to @f{L} (and, in this sense, is ``automatically restricted'' to @f{L}), let us
compute the Lie derivative @f{{\cal L}_{\xi} \alpha_{\rm res}}.
Since we have already proven that @f{F|_L = 0}, Eq. (@ref{HamiltonianModuloAlpha}) implies:
@equation{
{\cal L}_{\xi} \alpha_{\rm res} = \rho \alpha_{\rm res}
}
Remembering the definition of the Lie derivative, this implies that for any vector @f{v} 
tangent to @f{L} at some point @f{y\in L}, we get:
@equation{
{d\over dt}\alpha(g^t_{[\xi]*} \;\;\; v) = \rho(g^t_{[\xi]}\;\;y)\alpha(g^t_{[\xi]*} \;\;\; v)
}
This is an ordinary differential equation for @f-2{\alpha(g^t_{[\xi]*} \;\;\; v)}. It can be solved:
@equation[#:label "SolutionForAlphaOfV"]{
\alpha(g^t_{[\xi]*} \;\;\; v) = {\rm exp}\left(\int_0^t ds\; \rho(g^s_{[\xi]}\;\;y)\right) \; \alpha(v)
}
To complete the proof, we observe that when @f{y} is on @f{K}, we automatically have @f{\alpha(v) = 0}.
Indeed, in this case @f{v\in T_y L} can be decomposed as a linear combination of @f{\xi} and a vector
tangent to @f{K}, and we already know that @f{\alpha(\xi)=0} and @f{K} is Legendrian. But for any
point @f{y\in L}, there exists such @f{t\in {\bf R}} that @f{g^t_{[\xi]}\; (y)} belongs to @f{K} --- this
is the intersection of the characteristics passing through @f{y} with @f{K\subset L}.
Then Eq. (@ref{SolutionForAlphaOfV}) implies that @f{\alpha(v) = 0}, which is what we had to prove. 
}

@slide["Explicit description of the characteristic vector fields" #:tag "CharacteristicExplicit" #:showtitle #t]{
Suppose that the contact form is:
@equation{
\alpha =  du - p_{m} dx^{m} 
}
@equation{
   \xi = {\partial F\over\partial p_{m}}{\partial\over\partial x^{m}}
+ p_{\mu}{\partial F\over\partial p_{m}}{\partial\over\partial u} 
-\left(
   {\partial F\over\partial x^{m}} + p_{m}{\partial F\over\partial u}
\right){\partial\over\partial p_{m}}
}
In other words:
@align[r.l.n @list[
@v+[12 @f{\dot{x}^{m} \;=\;}] @f{{\partial F\over\partial p_{m}}} ""
]@list[
@v+[4 @f{\dot{p}_{m} \;=\;}] @f{ - {\partial F\over\partial x^{m}} -  p_{m}{\partial F\over\partial u}} 
@label{ExplicitCharacteristics}
]@list[
@v+[12 @f{\dot{u} \;=\;}] @f{ p_{m}{\partial F\over\partial p_{m}} } ""
]]
}

@slide["How to reconstruct characteristics knowing the solutions of PDE" #:tag "ReconstructionOfCharacteristics" #:showtitle #t]{
We have seen that solutions of PDE (@ref{NonlinearPDE}) could be obtained from knowing the characteristics curves.

In classical mechanics, the inverse problem is often interesting: if we know solutions of PDE, can we reconstruct the
characteristic curves?

A solution is @f{u = u(x^1,\ldots,x^n)}. Knowing just @bold{one} solution does not allow to reconstruct characteristics.
In fact, we need to know @bold{a family of solutions}. For example, for @f{n=2}, let us consider
a 1-parameter family of solutions, parametrized by a parameter @f{s\in {\bf R}}, so we have
@f{u = u_s(x^1,x^2)}:

@(image (string->path "snapshots/envelope-small.png"))
@; (setq amkhlv/snapshot-dir "/home/andrei/Samsung")

The red lines are characteristics. Obviously, they are determined by the equation:
@equation{
{\partial u_s(x^1,x^2)\over \partial s} =0
}
Notice that the so obtained characteristics (red curves) span a two-dimensional surface,
which can be considered as a graph of a new function @f{u= u_{\rm env}(x^1,x^2)}. This is, again,
a solution of (@ref{NonlinearPDE}). This new solution is called the @bold{enveloping} of the family of solutions. 

This way we got a 1-parameter family of characteristics. To get @bold{all} characteristics, we consider
a 2-parameter family of solutions @f{u_{s_1,s_2}(x^1,x^2)}. For any 1-parameter @bold{sub-family}:
@equation{
u_{s_1(\sigma),s_2(\sigma)}(x^1,x^2)
}
we get a 1-parameter family of characteristics, given by:
@equation{
{\partial u_{s_1(\sigma),s_2(\sigma)}(x^1,x^2)\over \partial\sigma} = 0
}
This, again, swaps a 2-dimensional surface, which is again a solution of the PDE (@ref{NonlinearPDE}).

Considering all possible curves @f{s_1(\sigma),s_2(\sigma)} in the parameter space, we get @bold{all solutions}
of the nonlinear PDE (@ref{NonlinearPDE}).

In general case of @f{u} function of @f{n} variables @f{x^1,\ldots,x^n}, we need to know an @f{n}-parameter
family of solutions, in order to reconstruct a general solution and all characteristic. 
}

@slide["Hamilton-Jacobi equation" #:tag "HamiltonJacobiEquation" #:showtitle #t]{
Let us consider the following particular case of this construction. Let @f{M} be a @f{2n+3}-dimensional
contact manifold with the coordinates @f{S, p_{\mu}, q^{\mu}}, and with the contact form defined like this:
@equation{
\alpha = dS - p_{\mu}dx^{\mu}
}
We will have @f{\mu} running from @f{0} to @f{n}. Also, use the following @f{F}:
@equation{F = p_0 + H(\vec{p},\vec{q})}
Then, the characteristic equations (@ref{ExplicitCharacteristics}) become:
@align[r.l.n @list[
@f{{d\over dt} p_0 \;=\;} @v+[5 @f{0}] @label{HamiltonianIsConstant}
]@list[
@f{{d\over dt} q^0 \;=\;} @v+[5 @f{1}] @label{VelocityOfX0}
]@list[
@f{{d\over dt}\vec{q} \;=\;} @v-[2 @f{{\partial F\over \partial \vec{p}}\;}] @label{FirstHamiltonEq}
]@list[
@f{{d\over dt}\vec{p} \;=\;} @v-[2 @f{- {\partial F\over \partial \vec{q}}\;}] @label{SecondHamiltonEq}
]@list[
@f{{d\over dt} S \;=\;} 
@f{
p_0 {\partial F\over \partial p_0} + \vec{p}{\partial F\over\partial\vec{p}}
= -H(\vec{p},\vec{q}) + \vec{p} {d\over dt}\vec{q}
}
@label{VelocityOfAction}
]]
Notice that Eq. (@ref{VelocityOfX0}) tells us that @f{q^0} should be identified with @f{t}. 
In this case the PDE (@ref{NonlinearPDE}) is:
@equation[#:label "HamiltonJacobiEqn"]{
{\partial S(\vec{q},t)\over \partial t} + H\left(\vec{q}, {\partial S\over \partial \vec{q}}\right) = 0
}
}



@; ---------------------------------------------------------------------------------------------------

@close[formula-database]
