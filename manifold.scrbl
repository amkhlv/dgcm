#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require "defs.rkt" bystroTeX/common bystroTeX/slides (for-syntax bystroTeX/slides_for-syntax))
@(require (only-in db/base disconnect))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(define bystro-conf   
   (bystro (bystro-connect-to-server (build-path (find-system-path 'home-dir) ".config" "amkhlv" "latex2svg.xml"))
           "manifold/formulas.sqlite"  ; name for the database
           "manifold" ; directory where to store .png files of formulas
           24  ; formula size
           (list 255 255 255) ; formula background color
           (list 0 0 0) ; formula foreground color
           2   ; automatic alignment adjustment
           0   ; manual alignment adjustment
           ))
@(set-bystro-extension! bystro-conf "svg")
@; This controls the single page mode:
@(define singlepage-mode #f)
@; ---------------------------------------------------------------------------------------------------
@(bystro-def-formula "formula-enormula-humongula!")
@; ---------------------------------------------------------------------------------------------------
@(bystro-inject-style "misc.css" "no-margin.css")

@title[#:style '(no-toc no-sidebar)]{Manifolds}

@bystro-toc[]

@linebreak[]
@linebreak[]
@hyperlink["../index.html"]{go back to main page}


@slide["What is manifold?" #:tag "DefManifoldIntro" #:showtitle #t]{
 This part is boring, but necessary to get through. 

@itemlist[
@item{Classical mechanics speaks ``smooth manifolds''.
@bold{We have to learn this language.}}
@item{This staff @bold{is} boring and largely 
tautological. You will not particularly like it, that's normal. But it is necessary to
go through it.  If you want to communicate, you must learn the language.}
@item{I will try my best to make it less boring.}
]

The purpose of @bold{manifold} is to formalize the intuition behind a smooth surface:

@tabular[@list[
 @list[
@(image #:scale 0.5 (string->path "snapshots/surface-embedded-in-3d.png"))
@hspace[3] 
@para{This picture shows a @bold{two-dimensional manifold},  a surface. 
But together with some extra data which we dont need. What extra data? We see a 2d
surface @bold{embedded} in a 3d space. But what if we dont care about the 3d space,
we only care about the surface itself? Is there some @bold{intrinsic} structure,
which we could study and formalize?}
]]]
It turns out that it is possible, but somewhat nontrivial.
}



@slide["Manifold is a set, and also a topological space" #:tag "ManifoldIsTopSpace" #:showtitle #t]{

@tabular[@list[
 @list[
@(image #:scale 0.5 (string->path "snapshots/set-of-points.png"))
@hspace[3]
@para{Manifold is a @spn[attn]{set of points}!}
]
]]
Moreover, it is a @spn[attn]{topological space}. 
@bold{General topology.} 
is a mathematical game, which captures the notion of @bold{continuity}.
A topological space is a @bold{set with some additional structure}, which allows to say
which functions are continuos and which are not.

@div[comment]{
The solution is somewhat surprizing: in order to describe continuity, one defines the notion of
@bold{open set}. The additional structure (called ``topology'') describes a subset of the set of all 
subsets of @f{X}, which are considered @bold{open}. This subset should satisfy certain properties:
@tg[ol]{
@tg[li]{The union of open sets is an open set.}
@tg[li]{The finite intersection of open sets is an open set.}
@tg[li]{@f{X} and the empty set @f{\emptyset} are open sets.}
}
For example, the real line @f{\bf R} (a set) becomes a topological space if we declare that open
sets are unions of open intervals @f{a<x<b}. Function @f{f} is considered @bold{continuous} iff
@f{f^{-1}(\mbox{open set}) = \mbox{open set}}.

@tg[table]{
@tg[tr]{
@tg[td]{For example, this function is @bold{not} continuous, because 
@f{f^{-1}((-\infty,b)) = (-\infty,a]} — not an open set!}
@tg[td]{@(image #:scale 0.5 "snapshots/not-a-continuous-function.png")}}
}
}
}

@slide["Smoothness" #:tag "Smoothness" #:showtitle #t]{
We explained how to formalize @larger{@spn[attn]{continuity}}, but we need more: we need 
@larger{@spn[attn]{smoothness}}. 
@tabular[@list[
 @list[
@para{For example, this function is @bold{continuous, but not smooth}:} 
@(image #:scale 0.5 (string->path "snapshots/continuous-but-not-smooth.png"))
]
]]

@div[comment]{
Why do we need to formalize obvious things?
@linebreak[]
Of course, the standard answer is: @bold{in order to be mathematically rigorous}.
@linebreak[]
But I think there is also another reason, somewhat psychological.
There are foundational concepts, which come up again and again in our study of the nature.
It is useful to ``package'' them in a concise and standartized form:
@tg[ol]{
@tg[li]{So that researchers have common language}
@tg[li]{By training ourselves in using these ``packages'' as standard blocks, we could
perhaps learn to think faster}
}
Of course it is important to return to these fundamental concepts, because sometimes 
there are surprizes. It turns out that there are infinitely many smooth manifolds which
are all topologically equivalent to @f{{\bf R}^4}.
}
}

@slide["Smooth structure" #:tag "SmoothStructure" #:showtitle #t]{
``Smooth structure'' should allow to @bold{take derivatives of functions}.

On a smooth manifold, we can define a concept of @bold{tangent space at a point}, which is
a linear space, consisting of @bold{tangent vectors}. For every vector we should be able to
compute @bold{the derivative of a function at a point along a vector}:
@equation{
(v\cdot \partial) f
}
}

@after-pause{

@tabular[@list[
 @list[
@para{The concept of tangent space is obvious for an embedded surface @f{S\subset {\bf R}^3}.
@linebreak[]
@linebreak[]
It is literally, @bold{the} tangent space, i.e. the space of all vectors in 
@f{{\bf R}^3} which are tangent to our surface.
@linebreak[]
@linebreak[]
A function @f{f} is smooth, if it is a restriction of some smooth function 
@f{\hat{f}} on @f{{\bf R}^3}. It is easy to differentiate such a function:
@linebreak[]
@f{(v\cdot \partial) f = v^m{\partial\over\partial x^m}\hat{f}}
@linebreak[]
@bold{Exercise @ex-num{TangentSpaceEmbeddingLift}:}
Prove that this does not depend on how we choose @f{\hat{f}}
}
@hspace[3]
@(image (string->path "snapshots/tangent-space-for-embedded.png"))
]
]]

}

@slide["Definition of manifold" #:tag "DefManifold" #:showtitle #t]{
@table-of-contents[]
@section{Main idea}
The idea is to start with @f{{\bf R}^n} and construct a manifold by @bold{gluing together pieces} 
of @f{{\bf R}^n}. This is perhaps somewhat ugly, but will do the job. The @f{{\bf R}^n} does have
a smooth structure, we know how to differentiate functions on @f{{\bf R}^n}.

@spn[attn]{Smooth manifold} is a topological space equipped with an additional structure:
@itemlist[
@item{An atlas of charts}
]
We have to define a @bold{chart} and an @bold{atlas of charts}.

@section{Definition of chart}
Pick a copy of @f{{\bf R}^n} and an open subset @f{U\subset {\bf R}^n}. 
@tabular[@list[
 @list[
@nested{A @spn[attn]{chart}
is a continuous map @f{\phi: U\to M} such that:
@itemlist[#:style 'ordered
@item{The image @f{\phi(U)} is an open set in @f{M}}
@item{@f{\phi} is a homeomorphism between @f{U} and @f{\phi(U)}}
]}
@hspace[3]
@(image #:scale 0.75 (string->path "snapshots/def-chart.png"))
]
]]

@section{Definition of two charts being compatible}
Suppose that we have two charts:
@equation|{
\phi_1\;:U_1\;\to M \mbox{ and } \phi_2\;:U_2\;\to M
}|
@tabular[@list[
 @list[
@para{Consider the intersection @f{\phi_1(U_1)\cap \phi_2(U_2)\subset M}. If this intersection is 
empty, then the condition of compatibility is considered automatically satisfied. Otherwize, consider
the ``pre-images'' @f{V_1 = \phi_1^{-1}(\phi_1(U_1)\cap \phi_2(U_2))} and 
@f{V_2 = \phi_2^{-1}(\phi_1(U_1)\cap \phi_2(U_2))}.}
@hspace[2]
@(image #:scale 0.8 (string->path "snapshots/intersection-of-charts.png"))
]
]] 

There is a natural map @f{\tau_{12}: V_1\to V_2} defined as follows:
@equation[#:label "DefTau"]{\tau_{12} = \phi_2^{-1}\circ \phi_1}
@comment{
Here @f{f\circ g} denotes the @bold{composition of maps}:
@equation{ (f\circ g)(x) = f(g(x)) }}

We will say that  @spn[attn]{two charts are compatible} if this map is bijective and smooth.

@section{Definition of atlas}
An atlas of @f{M} is a collection of charts @f{\phi_i\;:U_i\to M} such that: 
@itemlist[#:style 'ordered
@item{They are all pairwize compatible}
@item{The union of all @f{\phi_i(U_i)} covers the whole @f{M}}
]

@section[#:tag "Potato"]{Example: surface of a potato is a 2d manifold}
Here is an example of an atlas (five charts are shown):

@image[#:scale 0.5]{svg/surface-of-potato.svg}
}

@slide["Equivalence relation on atlases" #:tag "EquivalenceOfAtlases" #:showtitle #t]{
As you can see from @seclink["Potato"]{the potato picture}, an atlas is a rather elaborate
(cumbersome?) construction.

But actually, we only need a very limited piece of information from it. The atlas in all its detail
is way too much. So, we will introduce some sort of equivalence relation on atlases: two atlases
are considered @bold{equivalent}, when they provide the @bold{same essential data}. 

So, what is this essential data?

The atlas should be able to tell us which functions @f{M\to {\bf R}} are smooth and which are
not. That's all! 

@div[redbox]{
The function @f{f\;:\;M\to {\bf R}} is called smooth if for any chart @f{\phi\;:\; U\to M} the
composition @f{f\circ \phi\;:\; (U\subset {\bf R}^n)\to {\bf R}} is smooth (as a function of @f{n} variables)
} 

We will @spn[attn]{restrict ourselves} to only use atlas for one single purpose: 

@itemlist[
@item{to be able to say if a given function is smooth or not}
]

@centered{@spn[redbox]{@spn[attn]{warning}: All other uses of atlas will be considered @italic{a priori} illegal!}}

It is easy to see that two atlases are @bold{equivalent} if and only if their sum is also a valid atlas.
}

@slide["Smooth maps between manifolds" #:tag "SmoothMaps" #:showtitle #t]{
A map 
@equation{
F:\;M\to N
}
is called @bold{smooth} if for any smooth function @f{f:\;N\to {\bf R}} the composition
@f{f\circ F\;:\;M\to {\bf R}} is a smooth function.

Particularly important are smooth maps @f{{\bf R}\to M}; such a map is called ``a smooth path''.
(More precisely, a @bold{parametrized smooth path}). We will also use the term ``trajectory''.
}

@slide["The notion of o-little" #:tag "OLittle" #:showtitle #t]{
@table-of-contents[]
@section{For real-valued functions}
@tabular[@list[@list[
@nested{Consider a point @f{m_0\in M} and a function @f{f\;:\;M\to {\bf R}}.
Let us ask ourselves a question: is it true that for a point @f{m} approaching @f{m_0}:
@equation[#:label "olittle"]{f(m) = f(m_0) + o(m-m_0)\;\;?}
@italic{A priori} this question does not make much sense, because we don't know how to
make sense of @f{m-m_0}. A manifold is not a linear space (neither affine space) so we cannot
take a difference of two points.

Instead, we will give the following definition. We will say that Eq. (@ref{olittle}) holds true if
and only if ∀ path @f{\sigma\;:\; {\bf R}\to M} such that @f{\sigma(0)=m_0} we have:
@equation{
f(\sigma(t)) - f(m_0) = o(t)
}

Similarly, for any @f{n>0} we define:
@equation{
f(m) = f(m_0) + o((m-m_0)^n) ⇔ (∀ \sigma: {\bf R}\to M:\; \sigma(0)=m_0 ⇒ f(\sigma(t))=f(m_0)+o(t^n))
}
}
@image{svg/point-in-potato.svg}]]]

@section[#:tag "sec:OLittleForMaps"]{For maps of smooth manifolds}
Let @f{F:\; M\to N} and @f{G:\; M\to N} be two maps from @f{M} to @f{N}, which map the point
@f{m_0\in M} to the same point @f{n_0\in N}:
@equation{
F(m_0) = G(m_0) = n_0 \in N
}
We will say that:
@equation{
F(m) = G(m) \;\;\mbox{mod}\;\; o(m-m_0)
}
if for any smooth function @f{h:\;N\to {\bf R}}:
@equation{
h(F(m)) = h(G(m))\;\;\mbox{mod}\;\; o(m-m_0) \;\;
}

}


@slide["Cotangent space" #:tag "CotangentSpace" #:showtitle #t]{

Given a point @f{m_0\in M}, we will define a linear space @f{T^*_{m_0}M}. Consider the linear
space @f{L} of all smooth functions @f{\phi:\;M\to {\bf R}} such that @f{\phi(m_0)=0}.

Consider the subspace @f{L_o\subset L} consisting of such functions which also satisfy @f{\phi(m) = o(m-m_0)}.

@bold{Definition:} the cotangent space to @f{M} at the point @f{m_0\subset M} is:
@equation{
T^*_{m_0}M = L/L_o
}
--- all functions vanishing at the point @f{m_0} modulo those which ``vanish faster than linear''.

This is, by definition, a linear space.
}


@slide["Tangent space" #:tag "TangentSpace" #:showtitle #t]{
We will define the tangent space to @f{M} at the point @f{m_0\in M} as the dual to the cotangent space:
@equation{
T_{m_0}M = (T^*_{m_0}M)^*
}
Remember that the @seclink["CotangentSpace"]{definition of the cotangent space} used the notion
of @f{o(m-m_0)}, which in turn @seclink["OLittle"]{uses the trajectories passing through @f{m_0}}.
Not surprizingly, the tangent space can be defined in terms of trajectories. Let us introduce
an equivalence relation on the space of trajectories passing through the point @f{m_0\in M}.
For two trajectories @f{\sigma_1 :\; {\bf R}\to M} and @f{\sigma_2:\; {\bf R}\to M} such that
@f{\sigma_1(0)=m_0} and @f{\sigma_2(0)=m_2} we say that
@f{\sigma_1\equiv \sigma_2} iff:
@equation{
\sigma_1(t) = \sigma_2(t)\;\;\mbox{mod}\;\; o(t)
}
where @f{o(t)} is @seclink["sec:OLittleForMaps"]{in the sense of maps from @f{\bf R} to @f{M}}.

When @f{\sigma_1\equiv\sigma_2}, we will say that ``@f{\sigma_1} and @f{\sigma_2} have @bold{the same velocity}
at the point @f{m_0}''.

In this sense:

@centered{@spn[redbox]{Tangent space is the space of velocities of trajectories}}

Notice that we gave two equivalent definitions: as the space dual to @f{T^*_{m_0}M} and as the space of equivalence
classes of trajectories. The second definition is somewhat more geometrical, but the first one 
exposes the @bold{linear structure}:

@centered{@spn[redbox]{Both @f{T^*_{m_0}M} and @f{T_{m_0}M} are linear spaces}}
}


@slide["Tangent bundle" #:tag "TangentBundle" #:showtitle #t]{
For any point @f{x\in M}, we can consider the corresponding tangent space @f{T_xM}.
Let us consider @bold{the union} (in set theory) of all these @f{T_xM} over all @f{x\in M}.
This is some set, which we will call @f{TM}:
@equation{
TM = \bigcup\limits_{x\in M} T_xM  \;\; \mbox{\small --- the tangent bundle of }M
}
@tabular[@list[@list[
@nested{
A point of @f{TM} is a pair @f{(m,\sigma)} where @f{m} is a point of @f{M} 
@linebreak[] 
and @f{\sigma} a trajectory passing through @f{m} modulo @f{o(t)}.}
@hspace[5]
@image{svg/m-and-sigma.svg}]]]

It turns out that @f{TM} is a @bold{smooth manifold of dimension} @f{2n}, the ``tangent bundle of @f{M}''.
@smaller{(the word ``bundle'' will be explained later)}.

In which sense @f{TM} is a manifold?  It comes naturally equipped with @bold{an atlas}!

Let us consider some atlas of the manifold @f{M}. It consists of some collection of maps:
@equation{\phi_i\;:\;U_i\to M}
Now we will cook from it some atlas for @f{TM}. 

@tabular[@list[@list[
@nested{Let us consider the collection of open
sets:
@equation{
(U_i\times {\bf R}^n) \subset {\bf R}^{2n}\;\;\;\mbox{("cylinder over }U_i\mbox{")}
}
and the following maps @f{\Phi_i:\;(U_i\times {\bf R}^n)\to TM}:
@equation{
\Phi_i(x^1,\ldots,x^n,v^1,\ldots,v^n)=(m,\sigma)
}
where @f{m= \phi(x^1,\ldots,x^n)} (a point in @f{M}) and
@f{\sigma} is the following trajectory: 
@equation{
\sigma(t) = \phi(x^1 + tv^1,\ldots,x^n + tv^n)
}
modulo @f{o(t)}
} 
@image{svg/cylinder-over-U.svg}]]]


}

@slide["Tangent bundle: transition functions" #:tag "TangentBundleTransitionFunctions" #:showtitle #t]{
Let us consider the case when some point @f{m} is covered by two charts @f{U,\phi} and @f{\widetilde{U},\widetilde{\phi}}.

@section{Gluing functions intersections of charts of @f{M}}
We know that the two points @f{(x^1,\ldots,x^n)\in U} and @f{(\widetilde{x}^1,\ldots,\widetilde{x}^n)\in \widetilde{U}}
actually define the same point in @f{M} when:
@equation{
\phi(x^1,\ldots,x^n) = \widetilde{\phi}(\widetilde{x}^1,\ldots,\widetilde{x}^n)
}
In other words, this happens under the following condition:
@equation{
(\widetilde{x}^1,\ldots,\widetilde{x}^n) = \widetilde{\phi}^{-1}(\phi(x^1,\ldots,x^n))
}
We will therefore define the ``gluing function'':
@equation{
\tau = \tilde{\phi}^{-1}\circ\phi
}
This @f{\tau} is a function from @f{U\in {\bf R}^n} to @f{\widetilde{U}\in {\bf R}^n}, therefore it
has @f{n} components @f{\tau^1,\ldots,\tau^n}, each component being a function of @f{n} variables:
@equation{
\tilde{x}^1 = \tau^1(x^1,\ldots,x^n)\;,\;\ldots\;,\;
\tilde{x}^n = \tau^n(x^1,\ldots,x^n)
}
@comment{when these equalities hold, @f{(x^1,\ldots,x^n)} and @f{(\tilde{x}^1,\ldots,\tilde{x}^n)} are
actually the same point of @f{M}}

@section{Gluing charts of @f{TM}: transition functions}
Now let us consider the corresponding two charts of @f{TM}, namely the cylinders @f{U\times {\bf R}^n}
and @f{\widetilde{U}\times {\bf R}^n}. Let us ask ourselves the quesion:
@itemlist[
@item{when do
@f{(x^1,\ldots,x^n,v^1,\ldots,v^n)} and @f{(\tilde{x}^1,\ldots,\tilde{x}^n,\tilde{v}^1,\ldots,\tilde{v}^n)}
determine the same point in @f{TM}?}]
In other words, when 
@f{\Phi(x^1,\ldots,x^n,v^1,\ldots,v^n)  = \widetilde{\Phi}(\tilde{x}^1,\ldots,\tilde{x}^n,\tilde{v}^1,\ldots,\tilde{v}^n)}?

A straightforward considerations gives the following condition:
@align[r.l.n
 @list[
@f{\left(\begin{array}{c}\tilde{x}^1\cr \ldots \cr \tilde{x}^n\end{array}\right)\;=}
@f{\left(\begin{array}{c}\tau^1(\vec{x})\cr\ldots\cr\tau^n(\vec{x})\end{array}\right)\;}
""
]@list[
@f{\left(\begin{array}{c}\tilde{v}^1\cr \ldots \cr \tilde{v}^n\end{array}\right)\;=}
@f{\left(\begin{array}{c}\sum_{\mu=1}^n{\partial\tau^1(\vec{x})\over\partial x^{\mu}}v^{\mu}
\cr\ldots\cr
\sum_{\mu=1}^n{\partial\tau^n(\vec{x})\over\partial x^{\mu}}v^{\mu}\end{array}\right)\;}
""
]
]
@comment{when these equalities hold, @f{(x^1,\ldots,x^n,v^1,\ldots,v^n)} and 
@f{(\tilde{x}^1,\ldots,\tilde{x}^n,\tilde{v}^1,\ldots,\tilde{v}^n)} are
actually the same point of @f{TM}}

@centered{@spn[redbox]{notice that the relation between @f{v^{\mu}} and @f{\tilde{v}^{\mu}} is linear}}
We can write it in matrix notations:
@equation{
\tilde{v}^{\mu} = {\partial \tau^{\mu}\over\partial x^{\nu}}v^{\nu}
}
The matrix @f{{\partial \tau^{\mu}\over\partial x^{\nu}}} is the @bold{transition function} of the tangent bundle.
}

@slide["Consistency condition on the transition functions" #:tag "TransitionFunctionsConsistency" #:showtitle #t]{
@tabular[@list[
 @list[
@para{Now suppose that @f{x\in M} is covered by three different charts @f{U}, @f{\widetilde{U}} and @f{\widehat{U}}.

We have now three pairs of intersections: @f{U\cap \widetilde{U}}, @f{\widetilde{U}\cap\widehat{U}} and
@f{\widehat{U}\cap U}. This means that we have three transition functions @f{\tau_{[U\to \widetilde{U}]}}, 
@f{\tau_{[\widetilde{U}\to\widehat{U}]}} and @f{\tau_{[\widehat{U}\to U]}}.}
@hspace[2]
@(image #:scale 0.75 (string->path "svg/three-charts.svg"))
]
]]
This results in @bold{three} identification rules! The first two tell us that:
@tbl[#:orient 'hor @list[
 @list[
@elem{@f{(x^i,v^i)} using @f{\phi:U\to M}}
@elem{is same as}
@elem{@f{\left({\tau^i}_{[U\to \widetilde{U}]}(x),{\partial{\tau^i}_{[U\to \widetilde{U}]}\over\partial x^k}v^k\right)}
using @f-3{\widetilde{\phi}:\widetilde{U}\to M}}
]@list[
@elem{@f{(\tilde{x}^i,\tilde{v}^i)} using @f{\tilde{\phi}:\tilde{U}\to M}} 
@elem{is same as}
@elem{@f{\left({\tau^i}_{[\widetilde{U}\to\widehat{U}]}(\tilde{x}),
{\partial{\tau^i}_{[\widetilde{U}\to\widehat{U}]}\over\partial \tilde{x}^j}\tilde{v}^j\right)}
using @f-3{\widehat{\phi}:\widehat{U}\to M}}
]
]]
At the same time, the third identification rule tells us that
@tbl[@list[
 @list[
@elem{@f{(\hat{x}^i,\hat{v}^i)} using @f-4{\hat{\phi}:\widehat{U}\to M}}
@elem{is same as}
@elem{@f{\left({\tau^i}_{[\widehat{U}\to U]}(x),{\partial{\tau^i}_{[\widehat{U}\to U]}\over\partial \hat{x}^k}\hat{v}^k\right)}
using @f{\phi:U\to M}}
]
]]
@bold{Exercise @ex-num{TMTripleIntersection}:} show that the third gluing rule is compatible with the first two
}

@slide["Derivatives of maps" #:tag "DerivativeOfMap" #:showtitle #t]{
@section{Derivative of a smooth map}
For a smooth map @f{F:M\to N}, we will now define the @bold{derivative map} @f{F_*:TM\to TN}.

A point of @f{TM} is a pair @f{(x,v)}, where @f{x\in M} and @f{v\in T_xM}. We define:
@equation{
F_*((x,v)) = (F(x),\; w)
}
where @f{w\in T_{F(x)}N} is defined as follows. Let @f{f(t)} be a trajectory in @f{M}, starting at @f{x}
with the velocity @f{v}. We define @f{w} as the velocity vector of the trajectory @f{F(f(t))}.

@bold{Exercise @ex-num{CorrectnessOfDefDerivativeMap}:} Show that this definition does not depend on
the choice of a trajectory @f{f(t)} representing the velocity vector @f{v}; @italic{i.e.} any other trajectory 
@f{g(t)} satisfying @f{g(t)=f(t)+o(t)} would result in the same @f{w}.

@section{Formula for derivative in coordinates}
Now, suppose that @f{x\in M} is covered by a chart @f{\phi:U\to M} and @f{F(x)\in N} is covered
by a chart @f{\psi:V\to M}. Then we can calculate the coordinates of @f{w} 
and express them through the coordinates of @f{v}. We get:
@equation[#:label "DerivativeMapInCoordinates"]{
F_*((x,v)) = \left(F^1(x),\ldots,F^n(x)\;,\; \sum\limits_k {\partial F^1\over \partial x^k} v^k,
\ldots, \sum\limits_k {\partial F^1\over \partial x^k} v^k\right)
}
Notice that @f{w} depends on @f{v} @bold{linearly}.
}

@slide["Category of smooth manifolds" #:tag "CategoryOfSmoothManifolds" #:showtitle #t]{
Now suppose that we have three smooth manifolds, @f{M,N,L} and smooth maps @f{F:M\to N}, @f{G:N\to L}.
Then the composition @f{G\circ F} is again a smooth map. It follows immediately from our definition
of the derivative map, that:
@equation[#:label "DerivativeRespectsComposition"]{
(G\circ F)_* = G_* \circ F_*
}

@bold{Exercise @ex-num{DerivativeRespectsComposition}:} Prove (@ref{DerivativeRespectsComposition}) in 
coordinates, @italic{i.e.} using (@ref{DerivativeMapInCoordinates}).

@linebreak[]

@spn[attn]{Category} is a very important mathematical notion. It is similar to a group, but a bit weaker.

A @bold{group} is a set of elements which can be multiplied; if @f{f} and @f{g} are two elements of a group,
then we can form their product @f{f\circ g}. The product is associative:
@equation{
f\circ(g\circ h) = (f\circ g)\circ h
}
There is a unit element @f{\bf 1} such that @f{{\bf 1}\circ f = f}.  Also, for every @f{g} there is
an inverse @f{g^{-1}} such that @f{g^{-1}\circ g={\bf 1}}.

A @bold{category} @f{\cal C} has a weaker definition than a group, in the following way. 
Not any two elements can be multiplied. In fact, every
element of a category carries two labels, an in-label and and out-label. A product (or composition)
@f{F\circ G} is only defined when the in-label of @f{F} is the same as the out-label of @f{G}.

The elements of the category are called ``morphisms''.
The labels run over a set which is called @f{{\rm Ob}\;{\cal C}} --- ``the set of objects''.
The set of all morphisms which have in-label @f{A} and out-label @f{B} is called  @f{{\rm Mor}(A,B)}.
Therefore, if @f{F\in {\rm Mor}(A,B)} and @f{G\in {\rm Mor}(B,C)}, @bold{then} we can take their
product @f{G\circ F}. There should exist @f{{\bf 1}_{A}\in {\rm Mor}(A,A)} for every @f{A\in{\rm Ob}\;{\cal C}}.
 
@div[comment]{
An example of a category is the set of all rectangular matrices. 
In this case @f{{\rm Ob}\;{\cal C} = \{1,2,3,\ldots\}} and @f{{\rm Mor}(m,n)} is
the set of all matrices with @f{n} rows and @f{m} columns. The composition is the product of matrices.
}

A @bold{functor} between two categories is the analogue of @bold{homomorphism} between two groups:
@tabular[@list[
 @list[
@hspace[4]@f{{\cal F}: {\rm Ob} {\cal C} \to {\rm Ob} {\cal D}} ""
]@list[
@hspace[4]@f{{\cal F}: {\rm Mor}(A,B) \to {\rm Mor}({\cal F}(A),{\cal F}(B))} @elem{@hspace[3](the same letter @f{{\cal F}} is used)}
]
]]
such that @f{{\cal F}(F\circ G) = {\cal F}(F)\circ {\cal F}(G)}.

@linebreak[]
Smooth maps between manifolds form a category, let us denote it @f{{\cal M}}. In this case
@f{{\rm Ob}\;{\cal M}} is the set of all smooth manifolds, and 
@f{{\rm Mor}\;(M,N)} is the set of all smooth maps between @f{M} and @f{N}.

The construction of tangent bundle is an example of a functor, from @f{{\cal M}} to @f{{\cal M}}
(in fact, it is a functor from the category of smooth manifolds to the category of vector bundles;
we will discuss vector bundles later). For every manifold @f{M}, we get @f{TM}. And for every
map @f{F}, we get @f{F_*} which would be better to denote @f{TF}:

@div[redbox]{
For a derivative of a smooth map @f{F}, we will sometimes use the notation @f{TF\;} instead of @f{F_*}
This operation respects the composition of morphisms, as a functor should --- see Eq. (@ref{DerivativeRespectsComposition})
}
}



@; ---------------------------------------------------------------------------------------------------

@disconnect[formula-database]
