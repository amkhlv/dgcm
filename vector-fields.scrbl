#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)

@(require "defs.rkt" bystroTeX/common bystroTeX/slides (for-syntax bystroTeX/slides_for-syntax))
@(require (only-in db/base disconnect))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(define bystro-conf   
   (bystro (bystro-connect-to-server (build-path (find-system-path 'home-dir) ".config" "amkhlv" "latex2svg.xml"))
           "vector-fields/formulas.sqlite"  ; name for the database
           "vector-fields" ; directory where to store .png files of formulas
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

@title[#:style '(no-toc no-sidebar)]{Vector fields}

@table-of-contents[]


@linebreak[]
@hyperlink["../index.html"]{go back to main page}

@slide["One-parameter groups of diffeomorphisms" #:tag "OneParameterGroups" #:showtitle #t]{
@table-of-contents[]
@section{Action of groups on manifolds}
Remember that a group is a set @f{G} with the operation of multiplication @f{G\times G\to G}; given
two elements @f{(g_1,g_2)\in G}, their multiplication is usually denoted @f{g_1g_2}. It has to 
satisfy associativity @f{g_1(g_2g_3)  = (g_1g_2)g_3}; there whould be a special ``unit'' element
@f{{\bf 1}\in G} such that @f{{\bf 1}g = g{\bf 1} = g}; and for every @f{g\in G} should exist
the ``inverse'' @f{g^{-1}\in G} such that @f{g^{-1}g = {\bf 1}}.

@bold{Exercise @ex-num{RightUnit}*:} show that these axioms also imply that @f{g g^{-1} = {\bf 1}}.

Notice that the group multiplication is an operation of the type:
@equation|{
G\times G\to G
}|
Now let @f{M} be a smooth manifold. An @spn[attn]{action of group @f{G} on @f{M}} is an operation
of the type:
@equation|{
G\times M\to M\;,\quad (g,x) \mapsto g.x
}|
which satisfies: @f{g.(h.x) = (gh).x} and @f{{\bf 1}.x = x}. 
@section{Action of @f{{\bf R}}}
Consider a particular case when @f{G = {\bf R}}. Let @f{t} denote the coordinate on @f{\bf R}. In this
case:
@equation|{
\mbox{\small we will write } g^t(x) \mbox{\small instead of }t.x
}|
@bold{Definition:} a @spn[attn]{one-parameter group of diffeomorphisms} of @f{M} is a family of diffeomorphisms
denoted @f{g^t} parametrized by @f{t\in {\bf R}}, which satisfy the @bold{group law}:
@equation[#:label "GroupLaw"]{
g^{t_1}\circ g^{t_2} = g^{t_1+t_2}
}
and also the requirement that when @f{t=0}, the transformation @f{g^t} is identical: @f{g^0(x) = x}.


}

@slide["Vector fields" #:tag "VectorFields" #:showtitle #t]{
Suppose that we pick a vector @f{v(x)\in T_xM} in the tangent space at the point @f{x}, for every 
@f{x\in M}, so that it depends smoothly on @f{x}. Such an object is called a @spn[attn]{vector field}.
In other words, vector fields are ``sections of the tangent bundle'', @italic{i.e.} elements of @f{\Gamma(TM)}.

Given a one-parameter group @f{g^t}, there is a @bold{vector field associated to it}, defined as follows:
@equation{
v(x) = \left.{d\over dt}\right|_{t=0} g^t(x)
}
@div[comment]{Remember that right hand side of this formula is just an @bold{abbreviation} for 
@hyperlink["../manifold/TangentSpaceVelocity.html"]{the equivalence class} 
of the trajectory @f{f(t)=g^t(x)}, as we identify tangent vectors with equivalence classes of trajectories.}

It is a @spn[attn]{crucial fact}, that the flow @f{g^t} is actually @bold{completely determined} by
its velocity vector field @f{v(x)}. This is true because of the Group Law (@ref{GroupLaw}). Indeed:
@equation[#:label "NumericDiffEq"]|{
g^t = \underbrace{g^{t\over N}\circ g^{t\over N}\circ\cdots\circ g^{t\over N}}_{N \hspace{5pt}{\rm times}}
}|
and we can take limit @f{N\to \infty}. In this limit, @f{g^{t\over N}(x)} is approximately the same
as @f{f(t/N)} where @f{f(t)} is any trajectory starting from @f{x} and representing the vector @f{v(x)}
— the difference is of the order @f{o(1/N)}. This procedure is, essentially, how the differential
equations are solved numerically. This allows to find @f{g^t} from @f{v}. 

This allows us to conclude that:
@centered{
@div[redbox]{Vector fields and one-parameter groups of diffeomorphisms are the same thing}
}
@bold{Correction:} Strictly speaking, this statement requires the @bold{compactness} of @f{M}.
On non-compact manifolds, for some vector fields the procedure (@ref{NumericDiffEq}) may diverge in the
limit @f{N\to \infty}. An example which we considered in class, @f{v(x)=x^2} is a case in point. 

@bold{Exercise @ex-num{FlowForX2}:} In class we have shown that the vector field on @f{{\bf R}} given
by @f{v(x) = x^2} results in the one-parameter group of diffeomorphisms @f{g^t(x) = {x\over 1-tx}}. 
Demonstrate by explicit calculation, that this satisfies (@ref{GroupLaw}). Notice that this @f{g^t} is
not a diffeomorphism, because it is not well-defined on the point @f{x=t^{-1}}.

}

@slide["Derivations of algebras" #:tag "Derivations" #:showtitle #t]{
We are actually studying an object which has several faces. We have already seen two: vector fields
and one-parameter groups of diffeomorphisms. We will now uncover the third face, somewhat algebraic.

Remember that an algebra @f{A} is a linear space with an associative multiplication law.
A linear operation @f{{\cal D}:\;A\to A} is called a @spn[attn]{derivation} of @f{A}, if it satisfies
the @bold{Leibniz rule}
@equation|{
{\cal D}(ab) = ({\cal D}a)b + a {\cal D}b \;\mbox{ \small for any two elements } a\in A,\;b\in A
}|
(find it on @hyperlink["http://en.wikipedia.org/wiki/Derivation_(abstract_algebra)"]{Wikipedia}!)

It turns out that a vector field @f{v(x)} on @f{M} is same as a 
@spn[attn]{derivation of algebra of functions} on @f{M}. Algebra of functions on @f{M} is a
infinite-dimensional algebra. It is formed by all smooth functions on @f{M\to {\bf R}}, and the 
operaion of multiplication is just the point-wise multiplication of functions @f{(fg)(x) = f(x)g(x)}. 
This algebra is commutative. It is usually denoted @f{C^{\infty}(M)}. It remains to understand,
how this is related to vector fields. The relation is through the @bold{Lie derivative}, which we
will now define.
}

@slide["Lie derivative" #:tag "LieDerivativeDef" #:showtitle #t]{
Given a vector field @f{v}, the Lie derivative @f{{\cal L}_v f} of a function @f{f:\;M\to {\bf R}}
along @f{v} is defined by any of the following (equivalent) formulas:
@equation[#:label "DefLv"]{
{\cal L}_v f := df(v) = \left.{d\over dt}\right|_{t=0} g^{t*}f = \left.{d\over dt}\right|_{t=0} f \circ g^t 
}
In coordinates:
@equation[#:label "DefLieDer"]{
{\cal L}_v f(x) = v^m(x){\partial\over\partial x^m}f(x)
}
The expression on the right hand side is called a @bold{first order differential operator}. It is
easy to check that it satisfies the Leibniz rule:
@equation{
{\cal L}_v (f(x)g(x)) = ({\cal L}_v f(x))g(x) + f(x) {\cal L}_v g(x)
}
and therefore is a @bold{derivation of the algebra @f{C^{\infty}(M)}}.

@bold{Exercise @ex-num{DerIsFirstOrder}*:} Prove that any derivation of @f{C^{\infty}(M)} is of the
form (@ref{DefLieDer}) for some vector field @f{v(x)}.

}

@slide["Three faces of the same object" #:tag "ThreeFaces" #:showtitle #t]{
We are lead to the conclusion, that @bold{the following three objects are actually the same thing}:
@itemlist[#:style 'ordered
@item{One-parameter group of diffeomorphisms of @f{M}}
@item{Vector field on @f{M}}
@item{Derivation of @f{C^{\infty}(M)}}
]
}

@slide["Commutator of derivations" #:tag "CommutatorOfDers" #:showtitle #t]{
Consider an algebra @f{A} and two derivations @f{{\cal D}_1} and @f{{\cal D}_2} of @f{A}. 
Notice that their simple product @f{{\cal D}_1{\cal D}_2} is not a derivation, because the Leibniz
rule is not satisfied:
@equation{
{\cal D}_1{\cal D}_2 (fg) = ({\cal D}_1{\cal D}_2 f)g + f{\cal D}_1{\cal D}_2g + 
({\cal D}_1f){\cal D}_2g +({\cal D}_2f){\cal D}_1g
}
— the last two terms are problematic! However, being problematic, they are also symmetric in @f{1\leftrightarrow 2}.
Therefore they cancel if instead of a simple product we consider the @bold{commutator}:
@equation{
[ {\cal D}_1\;,\;{\cal D}_2 ] = {\cal D}_1{\cal D}_2 - {\cal D}_2{\cal D}_1
}
This is again a derivation!
}

@slide["Commutator of vector fields" #:tag "CommutatorOfVs" #:showtitle #t]{
Let us apply this general construction to the case when @f{A = C^{\infty}(M)} and @f{{\cal D}_1} and
@f{{\cal D}_2} are Lie derivatives along two different vector fields @f{v_1} and @f{v_2}.
We will get:
@equation{
[{\cal L}_v, {\cal L}_w] f = \left[v^m{\partial\over\partial x^m}, w^n{\partial\over\partial x^n}\right] f =
\left(v^m{\partial\over\partial x^m} w^n - w^m{\partial\over\partial x^m} v^n\right){\partial\over\partial x^n}f
}
We observe that the @bold{right hand side is a first order linear differential operator}.
Therefore it must be the operator of Lie derivative along some vector field. That vector field
is called @f{[v,w]}:
@equation[#:label "CommutatorInCoordinates"]{
[v,w] = v^m{\partial\over\partial x^m} w^n - w^m{\partial\over\partial x^m} v^n
}

Let us rewrite this formula using (@ref{DefLv}):
@equation{
[{\cal L}_v, {\cal L}_w] f = \left.{d\over dt}\right|_{t=0} \left.{d\over ds}\right|_{s=0} 
\left( g^{t*}_{[v]} \; g^{s*}_{[w]} \; f - g^{s*}_{[w]} \; g^{t*}_{[v]} \; f \right)
}
or equivalently:
@equation{
[{\cal L}_v, {\cal L}_w] f(x) = \left.{d\over dt}\right|_{t=0} \left.{d\over ds}\right|_{s=0} 
\left( f(g^s_{[w]}\;\;(g^t_{[v]}\;\;(x))) - f(g^t_{[v]}\;\;(g^s_{[w]}\;\;(x))) \right)
}
The result of taking derivative will not change if we replace 
@f{f\;\;\mapsto\;\; f\circ g^{-s}_{[w]}\circ g^{-t}_{[v]}}. Therefore:
@equation[#:label "CommutatorAlgebraVsCommutatorGroup"]{
[{\cal L}_v, {\cal L}_w] f(x) = \left.{d\over dt}\right|_{t=0} \left.{d\over ds}\right|_{s=0} 
f(g^{-s}_{[w]}\;\circ g^{-t}_{[v]}\;\circ g^s_{[w]}\;\; \circ g^t_{[v]}\;\;(x))
}
@centered{@image{snapshots/comm-of-flows.png}}

}

@slide["Flows and diffeomorphisms" #:tag "FlowsAndDiffeo" #:showtitle #t]{
A smooth map @f{M\to N} does not generally speaking act on vector fields, there is no such thing 
as @f{f_*v}. 

@comment{
If two points @f{x\in M} and @f{y\in M} are mapped to the same point @f{z = f(x) = f(y)}, then it is 
not clear what to do with @f{(f_*(v))(z)} — should it be defined as @f{f_*(v(x))}? or @f{f_*(v(y))}? 
or maybe @f{f_*(v(x)) + f_*(v(y))}?
}

But @f{f_*(v)} is well-defined when @f{f} is a diffeomorphism. In this case, the definition is:
@equation[#:label "DefPushForwardVectorField"]{
(f_*(v))(z) := f_*(v(f^{-1}(z)))
}
This is sometimes called the ``@bold{direct image}'', or alternatively the ``@bold{push-forward}'' 
of a vector field @f{v} under a map @f{f}.

From the definition (@ref{DefPushForwardVectorField}) follows:
@equation{
g^t_{[f_*v]} \;\;\;\;= f\circ g^t\circ f^{-1}
}
@(fsize+ (- 3))
@comment{
Our notations are somewhat ``overloaded'', as @f{f_*} can stand for two different things:
@tg[ol]{
@tg[li]{For a @bold{vector} @f{v\in T_xM}, we denote @f{f_*v \in T_{f(x)}M} the action of the derivative map @f{f_*:TM\to TM}}
@tg[li]{For a @bold{vector field} @f{v\in \Gamma(TM)}, we denote @f{f_*v \in \Gamma(TM)} the push-forward vector
field as defined in Eq. (@ref{DefPushForwardVectorField})} 
}
But these are the standard notations. We hope that this does not lead to a confusion. Moreover, these two things
are essentially the same: the push-forward of the vector field @f{v} by @f{f\;:\;M\to M} is point-wize the action
of the derivative of @f{f}; this means that for any point @f{x\in M}:
@equation{
(f_*v)(x)  = f_*(v(f^{-1}(x)))
}
}
@(fsize=)

Now let us return to Eq. (@ref{CommutatorAlgebraVsCommutatorGroup}). We can either first
evaluate  @f{d\over dt} and then @f{d\over ds}, or first evaluate @f{d\over ds} and then @f{d\over dt}.
This gives two equalities:
@equation[#:label "CommutatorViaDerivativeOfMap"]{
[v,w] = \left.{d\over ds}\right|_{s=0}g_{[w]*}^{s}\quad v = 
-\left.{d\over dt}\right|_{t=0}g_{[v]*}^{t}\quad w 
}

}

@slide["Tangent space is a functor" #:tag "TangentSpaceAsFunctor" #:showtitle #t]{
For any smooth manifold @f{M} we can construct another smooth manifold: the tangent bundle @f{TM}. Moreover,
for a smooth map @f{f\;:\;M\to N} there is a natural map @f{Tf\;:\;TM\to TN} defined as follows. For a point
@f{(v,x)\in TM} (where @f{x\in M} and @f{v\in T_xM}) we define:
@equation{
Tf(v,x) = (f(x)_*v,f(x))
}
--- a point in @f{TN}.

So, we defined the construction ``@f{T}'' on both object and morphisms. 

@comment{
Exercise: check that @f{Tf\circ Tg = T(f\circ g)}
}

This means that @f{T} is a functor from the category of smooth manifolds to itself. @smaller{(Actually to the
category of vector bundles, but we will not need that refinement for now; any vector bundle is, in
particular, a manifold.)}

@comment{
Exercise *: can you make sense of a cotangent space @f{T^*} as a functor? Would it be covariant or contravariant?
}
}

@slide["Lie derivative on differential forms and vectors" #:tag "LieDerivativeOnForms" #:showtitle #t]{
@section{Differential form is a function}
We can view a @f{p}-form as a function
@equation{
\omega\;:\; TM\to {\bf R}
}
polilinear in the fiber. Or, better to say, a function  @f{\Lambda^pTM\to {\bf R}} @bold{linear in the fiber}:
@equation{
\omega\;:\; \Lambda^pTM\to {\bf R}
}
A point of @f{\Lambda^p TM} is a pair: @f{\left(\sum_j v_1^{(j)}\wedge\cdots\wedge v_p^{(j)}\;,\;x\right)}. We will write:
@equation{
\omega(v_1\wedge\cdots\wedge v_p\;,\;x)
}

A vector field @f{v\in \mbox{Vect}(M)} defines a one-parameter family of diffeomorphisms @f{g^t\;:\;M\to M}.
As we explained, the construction of the tangent space is a covariant functor. Similarly, @f{M\mapsto \Lambda^pTM} is also
a covariant functor. Let us apply it to the morphism @f{g^t}. We get a new morphism, which should obviously be
called @f{\Lambda^pTg^t}:
@align[r.l
 @list[
@f{\Lambda^pTg^t\;:\;} @f{\Lambda^pTM\to \Lambda^pTM}
]@list[
@f{\Lambda^pTg^t(x,\; v_1\wedge\cdots\wedge v_p)\;=\;} @f{(g^t(x),\;g^t(x)_*v_1\wedge\cdots\wedge g^t(x)_*v_p)}
]
]

@section{Lie derivative on differential forms}
It is defined as follows:
@equation[#:label "DefLieDerivativeOnForms"]{
{\cal L}_v \omega = \left.{d\over dt}\right|_{t=0} \omega\circ \Lambda^pTg^t
}
In other words:
@equation{
({\cal L}_v\, \omega)(w_1\wedge\ldots\wedge w_p\;,\;x) = 
\left.{d\over dt}\right|_{t=0} \omega\left(\,g^{t}_{[v]}(x)_*\,w_1\wedge\ldots\wedge g^{t}_{[v]}(x)_*\,w_p\;,\;g_{[v]}^t(x)\right)
}
@bold{Theorem @th-num{LieOnDiffForm}:} 
@equation[#:label "LieOfOmegaOfV"]{
{\cal L}_v(\omega(w)) = ({\cal L}_v\,\omega)(w) + \omega([v,w])
}
@bold{Proof:} Let us consider the case of a 1-form @f{\omega}, as the general case is completely
analogous. Remember that a 1-form can be thought of as a function @f{TM\to {\bf R}}. It is useful
to introduce the notation @f{\omega(v(x)|x)}:
@equation{
(\omega(v))(x)\; = \;\omega(v(x)|x)\quad \mbox{\small where } x\in M\;,\; v(x)\in T_xM
}
This is to empasize that @f{(\omega(v))(x)} depends on the point @f{x\in M} and the tangent vector 
@f{v(x)} at this point. Notice that the dependence on the tangent vector @f{v(x)} (but not on @f{x}!)
is linear:
@equation{
\omega(v(x) + w(x)\;|\;x) = \omega(v(x)|x) + \omega(w(x)|x)
}
By definition (@ref{DefLieDerivativeOnForms}), we have:
@equation{
({\cal L}_v\, \omega)(w) = \left.{d\over dt}\right|_{t=0} \;\omega(g^t_{[v]*}\;\;\;w)
}
Let us write this in more detail, using our new notation:
@equation{
(({\cal L}_v\, \omega)(w))(x) = \left.{d\over dt}\right|_{t=0} \; \omega(g^t_{[v]*}\;\;\;(w(x)), g^t(x))
}
(Remember that since @f{w(x)\in T_xM}, we have @f{g^t_{[v]*}\;\;\;(w(x))\in T_{g^t(x)}M}.)
Let us further rewrite it like this:
@equation{
(({\cal L}_v\, \omega)(w))(x) = 
\left.{d\over dt}\right|_{t=0} \omega(g^t_{[v]*}\;\;\;(w(x))- w(g^t(x))\;,\; g^t(x)) \;+\;
\left.{d\over dt}\right|_{t=0} \omega(w(g^t(x))\;,\;\; g^t(x)) 
}
The second term on the right hand side equals @f{{\cal L}_v (\omega(w))}. To calculate the first term,
notice that the value of the derivative @f{d\over dt} at @f{t=0} will not change if we replace 
@f{x\to g^{-t}(x)}. Taking into account Eq. (@ref{CommutatorViaDerivativeOfMap}), this results 
in the following expression:
@equation{
\omega\left( \left.{d\over dt}\right|_{t=0} g^t_{[v]*}\;\;\;(w(g^{-t}(x)))\right) = - \omega([v,w])
}
This proves (@ref{LieOfOmegaOfV}).

The general formula:
@align[r.c.l.n @list[
@f{{\cal L}_v(\omega(v_1,\ldots,v_p)) =} @hspace[1] @f{({\cal L}\omega)(v_1,\ldots,v_p)} 
@label{LOmegaPForm}
]@list[
"" @hspace[1] @f{+ \;\omega([v,v_1],v_2,\ldots,v_p) + \omega(v_1,[v,v_2],\ldots,v_p) +\ldots +\omega(v_1,v_2,\ldots,[v,v_p])}
""
]]
is proved similarly.

@bold{Theorem @th-num{ThLieAndIota}:}
@equation[#:label "LieAndIota"]{
{\cal L}_v(\iota_u \omega) = \iota_{[v,u]}\omega + \iota_u {\cal L}_v\omega
}
@bold{Proof:} The following formula:
@fsize+[5]
@equation[#:label "PullBackWithIota"]{
g^{t*}_{[v]}(\iota_{g^t_{[v]*}\hspace{14pt} u}\omega) = \iota_u\, g^{t*}_{[v]}\, \omega
}
@fsize=[]
is geometrically obvious, and in fact true for any diffeomorphism @f{F}: 
@equation{
F^*(\iota_{F_*\hspace{4pt} u}\omega) = \iota_u\, F^*\, \omega
}
@div[comment]{
If you don't see it, try to draw a picture! This is even true when @f{F:M\to N} where @f{M} and @f{N} are two different
manifolds (but @f{F} should be a diffeomorphism).
}
Eq. (@ref{LieAndIota}) follows from (@ref{PullBackWithIota}) by applying @f{\left.{d\over dt}\right|_{t=0}}. 

@div[comment]{
A direct examination of Eq. (@ref{LOmegaPForm}) shows that it is in fact equivalent to Eq. (@ref{LieAndIota}),
so it was actually sufficient to prove just one of them.
}


@section{Lie derivative on vector fields} 
For two vector fields @f{v} and @f{w}, we @bold{define}:
@equation{
{\cal L}_v w := [v,w]
}
@bold{Exercise @ex-num{SignInLieDerOfVectorField}:} Try to come up with an intuitive explanation of
why we need a minus sign in Eq. (@ref{CommutatorViaDerivativeOfMap}) but no minus sign
in (@ref{DefLv}). Remember we are trying to define the @bold{derivative along the flow}.

}

@slide["Some properties of Lie derivative" #:tag "PropertiesOfLieDerivative" #:showtitle #t]{
@bold{Exercise @ex-num{LieDerivativeIsNotLinearInV}:} For a vector field @f{w}, a 1-form @f{\omega},
and a function @f{f}:
@align[r.c.l 
 @list[
@f{{\cal L}_{fv}\;w =} @hspace[1] @f{ f {\cal L}_v\,w \;-\; ({\cal L}_w f)v}
]@list[
@f{{\cal L}_{fv}\;\omega =} @hspace[1] @f{ f{\cal L}_v\,\omega \;+\; \omega(v)df}
]
]
We have so far defined the Lie derivative of forms only for 1-forms. The case of @f{p}-forms is
completely analogous.

@bold{Exercise @ex-num{LieDerivativeOnProductOfForms}:} Show that for the product of a @f{p}-form 
@f{\omega_1} and a @f{q}-form @f{\omega_2}, the Lie derivative is:
@equation{
{\cal L}_v\;(\omega_1\wedge\omega_2) = 
({\cal L}_v\;\omega_1)\wedge\omega_2 + \omega_1\wedge {\cal L}_v\;\omega_2
} 
}

@slide["Algebraic approach to exterior derivative" #:tag "AlgebraicExteriorDerivative" #:showtitle #t]{
It is possible to compute the exterior derivative of a @f{p}-form, using only the following two operations:
@itemlist[#:style 'ordered
@item{Lie derivative of a function}
@item{Commutator of vector fields}
]
To understand this, let us first look at the low-rank forms. A @bold{zero-form} is a function @f{f}, and
its exterior derivative is just the differential of a function:
@equation{
(df)(v) = {\cal L}_v f
}
For a @bold{one-form}:
@equation[#:label "AlgebraicExtDerivativeForOneForm"]{
(d\omega)(v_1,v_2) = {\cal L}_{v_1}(\omega(v_2)) - {\cal L}_{v_2}(\omega(v_1)) - \omega([v_1,v_2])
}
This can be proven as follows. First, we observe that the right hand side of 
(@ref{AlgebraicExtDerivativeForOneForm})  satisfies the following property. For any function @f{f},
we can multiply @f{v_1} by @f{f} and we observe @bold{linearity}:
@equation{
(d\omega)(fv_1,v_2) = f(d\omega)(v_1,v_2)
}
@bold{(check this!)} Similarly with @f{v_2\mapsto fv_2}, also linearity. After linearity is
established, it is enough to check the formula in coordinates on the constant vector fields
@f{v_1 = {\partial\over\partial x^{m_1}}} and @f{v_2 = {\partial\over\partial x^{m_2}}}.

For a @bold{two-form}, we have:

@align[r.c.l.n 
 @list[
@v+[1 @f{(d\omega)(v_1,v_2,v_3) = } ]@hspace[1]
@f{{\cal L}_{v_1}(\omega(v_2,v_3)) - {\cal L}_{v_2}(\omega(v_1,v_3)) + {\cal L}_{v_3}(\omega(v_1,v_2)) -}
""
]@list[
"" @hspace[1]
@f{- \omega([v_1,v_2],v_3) - \omega([v_2,v_3],v_1) - \omega([v_3,v_1],v_2)}
@label{ExteriorDerivativeOfThreeForm}
]
]

And the general formula (for @f{\omega} a @f{p-1}-form) is:
@align[r.c.l.n
 @list[
@f{(d\omega)(v_1,\ldots,v_p) = }
@hspace[1]
@f{\sum\limits_{1\leq n \leq p} (-1)^{p+1} {\cal L}_{v_k}(\omega(v_1,\ldots,\widehat{v_j},\ldots,v_p)) -}
""
]@list[
""
""
@f{\sum\limits_{1\leq i<j\leq p} \omega([v_i,v_j], v_1,\ldots,\widehat{v_i},\ldots,\widehat{v_j},\ldots,v_p)}
@label{AlgebraicExtDerivative}
]
]
where hat over the letter means that the letter is omitted. The proof is the same as for (@ref{AlgebraicExtDerivativeForOneForm}).

@bold{Exercise @ex-num{ProveAlgebraicDIsNilpotent}:} Use Eq. (@ref{AlgebraicExtDerivative}) to prove that @f{d^2=0}.

Eq. (@ref{AlgebraicExtDerivative}) is remarkable for the followins reason: it allows us to compute the
exterior derivative only knowing two operations:
@itemlist[#:style 'ordered
@item{Lie derivative of a function (do not need to know Lie derivative of a form)}
@item{Commutator of vector fields}
]
Moreover this equation allows to define a nilpotent operation @f{d} whenever we have a @bold{Lie algebra}
(commutator) and its @bold{representation} (in this case acting by the Lie derivative). This leads to
the subject of @bold{Lie algebra cohomology}.
}

@slide["How to prove the Poincaré lemma using Lie derivative" #:tag "PoincareLemmaAndLieDerivative" #:showtitle #t]{
The Euler vector field is:
@equation{
E = x^{\mu}{\partial\over\partial x^{\mu}}
}
If @f{\omega} is a @bold{closed} @f{k}-form, then:
@equation{
d\iota_E\omega = {\cal L}_E\omega
}
Therefore we can formally write:
@equation{
\omega = d\left( {1\over {\cal L}_E} \iota_E \omega \right)
}
but we have to explain what is @f|{{1\over {\cal L}_E}\iota_E\omega}|. One can define for 
{\bf any} @f{k}-form @f{\alpha}: 
@equation{
{1\over {\cal L}_E}\alpha \; = \int_0^{\infty}dt\; (g_t^{[-E]})^* \alpha
}
Indeed, we get:
@align[r.c.l
 @list[
@f{{\cal L}_E \left( (g_t^{[-E]})^* \alpha \right) =} 
@hspace[1]
@f{\left. {d\over ds}\right|_{s=0} \left( g_s^{[E]} (g_t^{[-E]})^* \alpha \right) =}
]@list[
@f{=} 
@hspace[1]
@f{-\left. {d\over dt}\right|_{t=0} \left( (g_t^{[-E]})^* \alpha \right)}
]]
and therefore:
@equation{
{\cal L}_E\left( 
   \int_0^{\infty}dt\; (g_t^{[-E]})^* \alpha
\right) = (g_0^{[-E]})^* \alpha - (g_{\infty}^{[-E]})^* \alpha =
\alpha
}
@bold{Exercise @ex-num{ConeVsLie}:}
Remember the @hyperlink["../diff-forms/index.html"]{cone of a singular chain} which was denoted @f{p(c)}
One can similarly define a cone of a differential form @f{\omega}, which we will also denote @f{p(\omega)}.
By definition, for an @f{n}-form @f{\omega}:
@equation{
(p(\omega))(x|v_1,\ldots,v_{n-1}) = 
\int_0^1 \; dt\; \omega\left(tx\left| x^m{\partial\over\partial x^m}\right., tv_1,\ldots,tv_{n-1}\right)
}
Show that:
@equation{
{1\over {\cal L}_E} \iota_E\omega = p\omega
}

}

@slide["Normal form of a vector field" #:tag "NormalFormOfVectorField" #:showtitle #t]{
A generic vector field on an @f{n}-dimensional manifold @f{M} is, in coordinates, given by the expression:
@equation{
v(x) = v^1(x){\partial\over\partial x^1} + \ldots + v^n(x){\partial\over\partial x^n}
}
@bold{Theorem @th-num{CanonicalVF}:} In a small neighborhood of a point @f{x_0\in M} where 
@f{v(x_0)\neq 0}, it is possible to change coordinates @f{(x^1,\ldots,x^n)\mapsto (y^1,\ldots,y^n)} 
so that in new coordinates @f{v} is given by a very simple expression:
@equation{
v(x) = {\partial\over\partial y^1}
}
@bold{Proof:} 
@tabular[@list[
 @list[
@para{
The picture shows the proof in the two-dimensional case (@f{n=2})
Let us choose a line transversal to the trajectories of @f{v}.
Choose a parametrization of this line. Let us define the
coordinate @f{y_1}  of the point @f{x} as the time needed to reach
this point from a point on the transversal line, and the coordinate
@f{y_2} as the parameter of the corresponding point of departure
on the transversal line.
}
@hspace[2]
@(image (string->path "snapshots/rectifying-vector-field.png"))
]
]]

}



@; ---------------------------------------------------------------------------------------------------

@disconnect[formula-database]
