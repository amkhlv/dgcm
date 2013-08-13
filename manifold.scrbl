#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require (for-syntax (planet amkhlv/bystroTeX/slides_for-syntax)))
@(require (planet amkhlv/bystroTeX/common) (planet amkhlv/bystroTeX/slides))
@(require (only-in (planet jaymccarthy/sqlite) close))

@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(begin
   (set-bystro-formula-processor! bystro-conf (find-executable-path "amkhlv-java-formula.sh"))
   (set-bystro-formula-database-name! bystro-conf "formulas.sqlite")
   (set-bystro-formula-dir-name! bystro-conf "formulas")
   (set-bystro-formula-size! bystro-conf 25)
   (set-bystro-autoalign-adjust! bystro-conf 2)
   (set-bystro-manual-base-alignment! bystro-conf 0)
   )
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
@(define formula-database (bystro-initialize-formula-collection))
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
@; @(define exercise-number 0)
@; @(define ex-hash (make-hash))
@; @(define (ex-num label) 
@;    (set! exercise-number (+ 1 exercise-number))
@;    (hash-set! ex-hash label exercise-number)
@;    (elemtag label (number->string exercise-number)))
@; @(define (ex-ref label)
@;    (elemref label (string-append "Exercise " (number->string (hash-ref ex-hash label)))))

@title{Manifolds}
Here we will review basic notions from the theory of smooth manifolds.

@bystro-toc[]


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
@para{A @spn[attn]{chart}
is a continuous map @f{\phi: U\to M}:}
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
Here @f{f\circ g} denotes the @bold{composition of maps}:
@equation{ (f\circ g)(x) = f(g(x)) }
We will say that they are @bold{compatible if this map is a diffeomorphism.}

@section{Definition of atlas}
An atlas of @f{M} is a collection of charts @f{\phi_i\;:U_i\to M} such that: 
@itemlist[#:style 'ordered
@item{They are all pairwize compatible}
@item{The union of all @f{\phi_i(U_i)} covers the whole @f{M}}
]

@section{Definition of two atlases being equivalent}
Two atlases are considered equivalent, if their union is again an atlas.

@section{Definition of a smooth manifold}
A smooth manifold is  a set @f{M} together with the following additional structure:
@itemlist[
@item{@bold{An equivalence class of atlases}}
]

}

@slide["Tangent space: how to define it?" #:tag "TangentSpaceEquivalence" #:showtitle #t]{
We have to introduce the notion of a @bold{tangent vector}. Arnold uses a very physical definition:
@itemlist[
@item{tangent vector is the velocity vector of a particle}
]
But we have not yet defined what is ``vector''. So, the definition is circular. How do we break out of
this circle? 

@div[redbox]{
We will proceed in a round-about manner. Instead of saying directly what the vector of velocity is,
we just @bold{explain how to figure out if two trajectories have the same velocity}. 
}

This is a mathematical method of defining a property through the concept of 
@bold{equivalence classes}.

@fsize+[@-[5]]
@div[comment]{
Let @f{X} be a set. A @bold{relation} between elements of @f{X} is a subset @f{\Delta \subset X\times X}.
A relation is called an @bold{equivalence} if it is:
@tg[ol]{
@tg[li]{Refelxive: @f{(x,x)\in \Delta}}
@tg[li]{Symmetric: @f{((x,y)\in \Delta) \Leftrightarrow ((y,x)\in \Delta)}}
@tg[li]{Transitive: @f{((x,y)\in \Delta \mbox{ and } (y,z)\in\Delta) \Rightarrow ((x,z)\in \Delta)}}
}
}
@fsize=[]

@div[redbox]{
To describe the value of a property is the same as to describe the equivalence class of elements
having the same value
}
}

@slide["Tangent space: trajectories" #:tag "TangentSpaceTrajectories" #:showtitle #t]{
@tabular[@list[
 @list[
@para{
A trajectory originating at the point @f{x\in M} is a @bold{smooth map} from 
the interval @f{f\;:\;[0,\infty) \to M} such that @f{f(0) = x}
@linebreak[]
@smaller{
(``smooth map'' means that for any chart @f{\phi:U\to M} which covers @f{x\in M},
the map @f{\phi^{-1}\circ f} should be smooth)
}
}
@hspace[3]
@(image (string->path "snapshots/trajectories-in-manifold-with-U.png"))
]
]]
Let us consider the composition:
@equation|{
\phi^{-1}\circ f \;:\; I \to {\bf R}^n
}|
This takes values in @f{{\bf R}^n}, @italic{i.e.} the value is a vector. We will introduce
the following notaions for this vector:
@equation[#:label "VectorComponentNotation"]|{
(\phi^{-1}\circ f)(t) = 
\left(
\begin{array}{c} f^1(t) \cr f^2(t) \cr \ldots \cr f^n(t) \end{array}
\right)
}|
Here @f{f^1(t),\ldots f^n(t)} are called the @bold{components} of the map @f{f}.
}

@slide["Tangent space: velocity of a trajectory" #:tag "TangentSpaceVelocity" #:showtitle #t]{
Now we are ready to answer the question: 
@itemlist[
@item{When two trajectories starting at the point @f{x} have the same velocity at @f{x}?}
]
The answer is:
@div[redbox]{
We @spn[attn]{declare} that two trajectories @f{f(t)} and @f{g(t)}, both starting at @f{x} 
have the same velocity iff:
@linebreak[]
@linebreak[]
@hspace[5]@f+0+5{
f^k(t) - g^k(t) = o(t) \;\;\mbox{\small when }\; t\to 0\;\;,\;\; k\in\{1,\ldots,n\}
}@hspace[5]@label{oSmallOft}
}
Notice that the component notation (@ref{VectorComponentNotation}) uses a chart @f{\phi}. 

@bold{Exercise @ex-num{VelocityDoesNotDependOnChart}} Suppose that the point @f{x} is covered by more
than one chart. Prove that the equivalence relation which we have just described does not depend on a 
which chart we use.

Now you know what is tangent vector. If somebody asks you: ``give me an example of a tangent 
vector to a manifold @f{M} at a point @f{x\in M}'', you should do the following:
@tabular[@list[
 @list[
@(image #:scale 0.75 (string->path "snapshots/trajectories-in-manifold-starting.png"))
@hspace[2]
@itemlist[#:style 'ordered
@item{Draw some trajectory @f{f(t)} starting at @f{x} }
@item{Say that this trajectory @bold{defines} a tangent vector to @f{M} at the point @f{x}}
@item{But dont forget to say, that any other trajectory @f{g(t)} which is close to @f{f(t)} in the 
sense of (@ref{oSmallOft}) would define the same tangent vector}
]
]
]]
}

@slide["Tangent space: coordinates" #:tag "TangentSpaceInCoordinates" #:showtitle #t]{
First of all, a bit of notations. As we explained, any smooth trajectory @f{f(t)} defines a
velocity vector, as an equivalence class of this @f{f(t)} modulo the equivalence relations (@ref{oSmallOft}). 
The equivalence class is usually denoted @f{[f]}. But we find this notation ugly in our case.
Instead, we will write:
@equation[#:label "VectorDotNotation"]{
\dot{f}(0)\;\;\mbox{ \small or even }\;\; {df\over dt}(0)
}
--- this is the same as @f{[f]}, @italic{i.e.} the velocity vector corresponding to the trajectory.

Let us study the component notations (@ref{VectorComponentNotation}) in some detail. The components
of the vector on the RHS of (@ref{VectorComponentNotation}) is simply the coordinates of the 
point @f{\phi^{-1}(f(t))}. A more interesting object is @spn[attn]{coordinates of the vector}:
@equation[#:label "CoordinatesOfVector"]{
\dot{f}(0) = \left(
\begin{array}{c}
\left.{df^1(t)\over dt}\right|_{t=0} \cr \ldots \cr \left.{df^n(t)\over dt}\right|_{t=0}
\end{array}
\right)
}
To fix a tangent vector, we have to fix @f{n} components 
@equation[#:label "VComponents"]{
v^1 = \left.{df^1(t)\over dt}\right|_{t=0},\;\;\ldots\;\;,\; v^n = \left.{df^n(t)\over dt}\right|_{t=0}
}
--- these numbers @f{v_1,\ldots,v_n} are called ``components of the vector''.
}

@slide["Tangent space is a linear space" #:tag "TangentSpaceLinearStructure" #:showtitle #t]{
The set of all tangent vectors to @f{M} at the point @f{x} has a natural structure of a linear space,
@italic{i.e.} we can define the operation of summation:
@equation[#:label "LinearStructure"]{
[f_1] + [f_2] \stackrel{\rm\small def}{=} [\phi( \phi^{-1}\circ f_1 + \phi^{-1}\circ f_2 )] 
}
The right hand side is the definition of the opearation ``@f{+}''. When we do ``@f{+}'' on the right
hand side we just sum the coordinates of @f{\phi^{-1}\circ f_1} and @f{\phi^{-1}\circ f_2} 
@italic{i.e.} we compute the sum of components @f-2{f_1^k + f_2^k} as defined in 
(@ref{VectorComponentNotation}).

@bold{Potential problem:} 
On the left hand side, we are trying to define something depending on @bold{equivalence class}
of @f{f_1} and @bold{equivalence class} of @f{f_2}. However, the right hand side @italic{a priori}
depends on @f{f_1} and @f{f_2} themselves (and not just on their corresponding equivalence classes!)
That's why we need:

@bold{Exercise @ex-num{LinearStructureCorrectlyDefined}:} 
Show that the expression on the right hand side, actually, does not change if we replace @f{f_1}
and @f{f_2} with @f{g_1} and @f{g_2} where @f{g_1} is equivalent to @f{f_1} and @f{g_2} to @f{f_2}
in the sense of (@ref{oSmallOft}).

Also, show that this definition of ``@f{+}'' does not change if we pass from a chart
@f{\phi} to another chart @f{\tilde{\phi}}.

@bold{In terms of components} @f{v^1,\ldots,v^n} defined as in (@ref{VComponents}) the 
operation ``@f{+}'' is obvious:
@equation{
\left(\begin{array}{c} 
v_1^1 + v_2^1 \cr
v_1^2 + v_2^2 \cr
v_1^3 + v_2^3 \cr
\ldots \cr
v_1^n + v_2^n
\end{array}\right)
}

There is a very similar definition of the multiplication by a number @f{\lambda\in {\bf R}}:
@equation{
\lambda\cdot[f] \stackrel{\rm\small def}{=} [\phi( \lambda\phi^{-1}\circ f)]
}
which becomes @f{(\lambda v^1,\ldots,\lambda v^n)} in components.
}

@slide["Tangent bundle" #:tag "TangentBundle" #:showtitle #t]{
For any point @f{x\in M}, we can consider the corresponding tangent space. It is denoted:
@equation{
T_xM  \;\; \mbox{\small --- the tangent space to }M\mbox{ \small at the point }x
}
Let us consider the union (as in set theory) of all these @f{T_xM} over all @f{x\in M}.
This turns out to be a @bold{smooth manifold of dimension} @f{2n}, the ``tangent bundle of @f{M}'':
@equation{
TM = \bigcup\limits_{x\in M} T_xM  \;\; \mbox{\small --- the tangent bundle of }M
}
The point of @f{TM} is a pair @f{(x,v)} where @f{x\in M} and @f{v\in T_xM}.

If we have a chart @f{\phi:U\to M}, covering @f{x}, then we can parametrize @f{(x,v)} by @f{2n}
numbers:
@equation[#:label "XandV"]{
(x^1,\ldots x^n\;,\;\; v^1,\ldots,v^n)
}
Indeed, the definitions of @f{x^i} and @f{v^i} are:
@equation{
\left(\begin{array}{c}
x^1\cr \ldots \cr x^n
\end{array}\right) = \phi(x) \;\;\mbox{ and } \;\;
\left(\begin{array}{c}
v^1\cr\ldots \cr v^n
\end{array}\right) = 
\left(\begin{array}{c}
\left.{\partial f^1(t)\over \partial t}\right|_{t=0}
\cr
\ldots
\cr
\left.{\partial f^n(t)\over \partial t}\right|_{t=0}
\end{array}\right)
} 
This gives a straightforward description of a part of @f{TM}:
@equation[#:label "TMOverChart"]{
\bigcup\limits_{x\in \phi(U)}T_xM = U\times {\bf R}^n
}
Notice that @f{M} is totally covered by charts @f{U_i}. And, as we have just explained, for every
chart we have a fairly straightforward description (@ref{TMOverChart}) of @f{TM}. 
Therefore we obtain a fairly concrete description of @f{TM}, except for one important detail which 
we have to figure out:

@div[redbox]{
There are points @f{x} which are covered by two (or even more than two!) charts. For such points
there is more than one description in terms of (@ref{XandV}). We have to explain @bold{when 
two different 
descriptions correspond to the same point.} 
}

In other words, given two charts, @f{\phi:U\to M} and @f{\widetilde{\phi}:\widetilde{U}\to M}: 

what is the relation between 
@f{(x^1,\ldots,x^n,\;v^1,\ldots v^n)}  and 
@f{(\tilde{x}^1,\ldots,\tilde{x}^n,\;\tilde{v}^1,\ldots,\tilde{v}^n)}

when @f{(x^1,\ldots,x^n,\;v^1,\ldots v^n)} and 
@f{(\tilde{x}^1,\ldots,\tilde{x}^n,\;\tilde{v}^1,\ldots,\tilde{v}^n)} are actually the same
point of @f{TM}, only from the point of view of two different charts?

}

@slide["Tangent bundle: transition functions" #:tag "TangentBundleTransitionFunctions" #:showtitle #t]{
Investigation of this issue leads to the notion of ``transition functions''. 

The relation between @f{(x^1,\ldots x^n)} and @f{(\tilde{x}^1,\ldots,\tilde{x}^n)} we already know.
When we have two charts @f{\phi:U\to M} and @f{\widetilde{\phi}:\widetilde{U}\to M}, we get a ``gluing function'' 
@f{\tau} as in Eq. (@ref{DefTau}):
@equation{
\tau = \tilde{\phi}^{-1}\circ\phi
}
This @f{\tau} is a function from @f{U\in {\bf R}^n} to @f{\widetilde{U}\in {\bf R}^n}, therefore it
has @f{n} components @f{\tau^1,\ldots,\tau^n}, each component being a function of @f{n} variables. We have:
@equation{
\tilde{x}^1 = \tau^1(x^1,\ldots,x^n)\;,\;\ldots\;,\;
\tilde{x}^n = \tau^n(x^1,\ldots,x^n)
}
But what is the relation between @f{(v^1,\ldots,v^n)} and @f{(\tilde{v}^1,\ldots \tilde{v}^n)}? 
Suppose we have a trajectory @f{f(t)}, and the coordinate functions of the trajectory  in the chart @f{U}
are @f{f^1(t),\ldots,f^n(t)}. The components @f{v^j} of the corresponding tangent vector are given by
Eq. (@ref{VComponents}).
Then the coordinate functions of the same trajectory in the chart @f{\widetilde{U}}
are:
@equation[#:label "TildeFt"]{
\tilde{f}^j(t) = \tau^j(f^1(t),\ldots,f^n(t))
}
The components @f{\tilde{v}} are, again, given by (@ref{VComponents}):
@equation{
\tilde{v}^j = \left.{d\over dt}\right|_{t=0}\tilde{f}(t)
}
Applying the chain rule to (@ref{TildeFt}) we get:
@equation{
\tilde{v}^j = \sum\limits_{k=1}^n {\partial\tau^j\over\partial x^k} v^k
}
To summarize:
@div[redbox]{
@f{(x^1,\ldots,x^n,\;v^1,\ldots v^n)} on the chart @f{\phi: U\to M} and
@f{(\tilde{x}^1,\ldots,\tilde{x}^n,\;\tilde{v}^1,\ldots,\tilde{v}^n)} on the chart @f{\tilde{\phi}:\widetilde{U}\to M}
actually represent the same point in @f{TM} when:
@linebreak[]
@f+0+4{\tilde{x}^j = \tau^j(x^1,\ldots,x^n)} and 
@f-3+4{\tilde{v}^j = \sum\limits_{k=1}^n {\partial\tau^j(x^1,\ldots,x^n)\over\partial x^k} v^k}
}
}

@slide["Consistency condition on the transition functions" #:tag "TransitionFunctionsConsistency" #:showtitle #t]{
@tabular[@list[
 @list[
@para{Now suppose that @f{x\in M} is covered by three different charts @f{U}, @f{\widetilde{U}} and @f{\widehat{U}}.

We have now three pairs of intersections: @f{U\cap \widetilde{U}}, @f{\widetilde{U}\cap\widehat{U}} and
@f{\widehat{U}\cap U}. This means that we have three transition functions @f{\tau_{[U\to \widetilde{U}]}}, 
@f{\tau_{[\widetilde{U}\to\widehat{U}]}} and @f{\tau_{[\widehat{U}\to U]}}.}
@hspace[2]
@(image #:scale 0.75 (string->path "snapshots/three-charts.png"))
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

@slide["Smooth maps and their derivatives" #:tag "DerivativeOfMap" #:showtitle #t]{
@section{Definition of a smooth map between manifolds}
A @bold{smooth map} @f{F:M\to N} between two manifolds is a set-theoretical map whose coordinate
functions @f{F^j(x^1,\ldots,x^m)} are smooth functions. (This is independent of the choice of a chart.)

Given two smooth maps @f{F:M\to N} and @f{G:N\to L}, the composition @f{G\circ F} is again a smooth map.
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
@f{g(t)} satisfying (@ref{oSmallOft}) would result in the same @f{w}.

@section{Formula for derivative in coordinates}
Now, suppose that @f{x\in M} is covered by a chart @f{\phi:U\to M} and @f{F(x)\in N} is covered
by a chart @f{\psi:V\to M}. Then we can calculate the coordinates of @f{w} using (@ref{VComponents})
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

@slide["Eraseme" #:tag "Eraseme" #:showtitle #t]{

See @ex-ref{VelocityDoesNotDependOnChart}.

}


@; ---------------------------------------------------------------------------------------------------
@close[formula-database]
