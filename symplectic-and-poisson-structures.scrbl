#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require (for-syntax "defs_for-syntax.rkt" (planet amkhlv/bystroTeX/slides_for-syntax)))
@(require "defs.rkt" (planet amkhlv/bystroTeX/common) (planet amkhlv/bystroTeX/slides))
@(require (only-in (planet jaymccarthy/sqlite) close))
@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(define bystro-conf   
   (bystro (find-executable-path "amkhlv-java-formula.sh")
           "symplectic-and-poisson-structures_formulas.sqlite"  ; name for the database
           "symplectic-and-poisson-structures" ; directory where to store .png files of formulas
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
   (define-syntax (syntax-setter x) (defines-syntax-for-formulas x))                
   (syntax-setter defineshiftedformula)
   (defineshiftedformula "formula-enormula-humongula!"))
@; ---------------------------------------------------------------------------------------------------


@(bystro-inject-style "misc.css" "no-margin.css")

@title[#:style '(no-toc no-sidebar)]{Symplectic and Poisson geometry}

@table-of-contents[]


@linebreak[]
@hyperlink["../index.html"]{go back to main page}

@slide["Symplectic manifolds" #:tag "SymplecticManifolds" #:showtitle #t]{
@bold{Symplectic manifold} is a smooth manifold @f{M} with a 2-form @f{\omega} which satisfies two
properties:
@itemlist[#:style 'ordered 
@item{Is closed: @f{d\omega = 0}}
@item{Is non-degenerate: 
@f{\underbrace{\omega\wedge\omega\wedge\cdots\wedge\omega}_{{{\rm dim}\hspace{2pt}M\over 2} \hspace{5pt}{\rm times}} \neq 0}}
]
The non-degeneracy condition imlies that for any function @f{H\in C^{\infty}(M)} (``the Hamiltonian'') exists 
the so-called @spn[attn]{Hamiltonian vector field} @f{\xi_H} such that:
@equation{
\iota_{\xi_H}\omega = dH
}
(this formula is the definition of @f{\xi_H}). In particular, this equation, together with @f{d\omega =0}, implies that:
@equation{
{\cal L}_{\xi_H}\omega = 0
}
In other words, @f{\omega} is invariant under the flow of @f{\xi_H}:
@equation{
g^{t*}_{\xi_H}\;\omega = \omega
}
We can say that ``any @spn[attn]{Hamiltonian flow} is a symmetry of the symplectic form''. This means that
@spn[attn]{symplectic manifolds have infinitely many symmetries}. Any function @f{H\in C^{\infty}(M)}
generates a symmetry. 

@div[comment]{
For those of you who have already studied General Relativity, let us compare with the definition of
the Riemannian manifold. The definition of a Riemannian manifold is very similar. The only difference is that,
instead of considering an antisymmetric 2-tensor @f{\omega_{mn}(x)}, we consider a symmetric 
2-tensor @f{g_{mn}(x)} (``the metric''). This little detail leads to great differences between the theories
of these two classes of manifolds. The first thing to observe is that Riemannian manifolds have 
very few symmetries (typically no symmetries at all). But symplectic manifolds, as we have just seen,
always have infinitely many symmetries. 
}
}

@slide["Poisson bracket" #:tag "PoissonBracket" #:showtitle #t]{
The @spn[attn]{Poisson bracket} of two functions @f{H_1} and @f{H_2} is defined as follows:
@equation{
\{H_1,H_2\} = -\omega(\xi_{H_1},\xi_{H_2})
}
It follows immediately from this definition, that for any @f{F\in C^{\infty}(M)}:
@equation{
{\cal L}_{\xi_H}\;F  = \{H,F\;\}
}

@bold{Theorem @th-num{ThFluxOfPB}} 
@equation[#:label "FluxOfPB"]{
\xi_{\{H_1,H_2\}} = [\xi_{H_1},\xi_{H_2}]
}
@bold{Proof} Taking into account that the form @f{\omega} is closed and 
@f{{\cal L}_{\xi_{H_1}}\omega = {\cal L}_{\xi_{H_2}}\omega = 0}
@align[r.l @list[
@f{d\{H_1,H_2\} =\;} 
@f{-d\omega(\xi_{H_1},\xi_{H_2}) = -d\iota_{\xi_{H_2}}\;\iota_{\xi_{H_1}}\;\omega}
]@list[
@f{=\;} 
@f{- {\cal L}_{\xi_{H_2}}\;\iota_{\xi_{H_1}}\;\omega}
]]
Now we use the formula which we  @hyperlink["../vector-fields/LieDerivativeOnForms.html#(elem._.Lie.And.Iota)"]{proven some time ago}:
@equation[#:label "LieAndIota"]{
{\cal L}_v(\iota_u \omega) = \iota_{[v,u]}\omega + \iota_u {\cal L}_v\omega
}
This gives:
@equation{
d\{H_1,H_2\} =\; \iota_{[\xi_{H_1},\xi_{H_2}]}\;\omega
}
which is equivalent to (@ref{FluxOfPB}).

Now the Jacobi identity for the vector fields:
@equation{
[\xi_{H_1},[\xi_{H_2},\xi_{H_3}]] = [[\xi_{H_1},\xi_{H_2}],\xi_{H_3}] + [\xi_{H_2},[\xi_{H_1},\xi_{H_3}]]
}
implies the following @bold{corollary}:
@equation[#:label "JacobiIdentity"]{
\{H_1,\{H_2,H_3\}\} = \{\{H_1,H_2\},H_3\} + \{H_2,\{H_1,H_3\}\}
}

}

@slide["Poisson structure" #:tag "PoissonStucture" #:showtitle #t]{
Consider the Poisson bracket of two functions @f{H_1} and @f{H_2}:
@equation{
\{H_1,H_2\} = {\cal L}_{\xi_{H_1}}\;H_2 = -{\cal L}_{\xi_{H_2}}\;H_1 
}
Notice that:
@itemlist[
@item{For a fixed @f{H_1}, this is a first order linear differential operator acting on @f{H_2}:
@equation{\{H_1, FG\} = \{H_1,F\}G + F\{H_1,G\}}
}
@item{Similarly, for a fixed @f{H_2} this is a first order linear differential operator acting on @f{H_1}}
@item{It is antisymmetric in @f{H_1\leftrightarrow H_2}}
]
In coordinates:
@equation{
\{H_1,H_2\}(x) = \pi^{mn}(x)\partial_m H_1(x)\partial_n H_2(x)
}
Where @f{\pi^{mn}(x)} is antisymmetric in @f{m\leftrightarrow n} and @bold{transforms as a product of two vectors}
under the changes of coordinates:
@equation{
\pi^{mn}(\tilde{x})  = {\partial \tilde{x}^m\over\partial x^p} {\partial \tilde{x}^n\over\partial x^q}
\pi^{pq}(x)
}
Such an object is called a @spn[attn]{bivector}:
@equation{
\pi\in \Gamma(\Lambda^2TM)
} 

}

@slide["Schouten bracket of polyvector fields" #:tag "SchoutenBracketPolyvector" #:showtitle #t]{
We have just seen that the Poisson structure is a @bold{bivector}. Bivectors are particular case of
more general @bold{polyvector fields}. A polyvector field @f{\psi} of the rank @f{p} is an element of 
@f{\Gamma(\Lambda^p TX)}:
@equation{
\psi \in \Gamma(\Lambda^p TX)
}
We will now explain that @f{\Gamma(\Lambda^p TX)} has two interesting operations, @bold{exterior product}
and @bold{Schouten bracket}, and this structure is directly relevant to understanding the Jacobi 
identity of the Poisson bracket.

@table-of-contents[]
@section{Exterior product}
The first @bold{natural operation} on @f{\Gamma(\Lambda^p TX)} is the @bold{exterior product}. 
If @f{\psi\in\Gamma(\Lambda^p TX)} and @f{\phi\in\Gamma(\Lambda^p TX)}, then we can form their
exterior product @f{\psi\wedge\phi}:
@equation{
\psi\wedge\phi \in \Gamma(\Lambda^{p+q} TX)
}

@section{Schouten bracket}
The definition of the Schouten bracket @f{@(gbl),@(gbr)} follows from its main properties:
@itemlist[#:style 'ordered
@item{It is a bilinear operation:
@align[r.l.n @list[
@f{@(gbl) \psi \; , \;\phi_1 + \phi_2 @(gbr) \;=\;}
@f{@(gbl) \psi ,\phi_1@(gbr) +  @(gbl)\psi,\phi_2@(gbr)}
@label{GbrLin2}
]@list[
@f{@(gbl)\psi_1 + \psi_2\;,\; \phi @(gbr) \;=\;}
@f{@(gbl)\psi_1,\phi @(gbr) + @(gbl)\psi_2,\phi @(gbr)}
@label{GbrLin1}
]
]
}
@item{It is a derivation of the wedge-product:
@align[r.l.n @list[
@f{@(gbl)\psi , \phi_1\wedge\phi_2 @(gbr) \;=\; }
@f{@(gbl)\psi,\phi_1@(gbr)\wedge \phi_2 + (-1)^{({\rm rk}(\psi)+1){\rm rk}(\phi_1)} \phi_1\wedge @(gbl)\psi,\phi_2@(gbr)}
@label{GbrDer2}
]@list[
@f{@(gbl)\psi_1\wedge\psi_2 , \phi @(gbr) \;=\; }
@f{\psi_1\wedge @(gbl)\psi_2 , \phi @(gbr) + (-1)^{{\rm rk}(\psi_2)({\rm rk}(\phi)+1)} @(gbl)\psi_1,\phi@(gbr)\wedge\psi_2}
@label{GbrDer1}
]
]
}
@item{When @f{{\rm rk}(v) = {\rm rk}(w) = 1} (@italic{i.e.} for two vector fields), the Schouten bracket
is the commutator of the vector fields:
@align[r.l.n @list[@f{@(gbl)v,w@(gbr) \;=\;} @f{[v,w]} @label{GbrVecs}]]
}
]
@(fsize+ (- 5))
@div[comment]{
The sign @f{(-1)^{({\rm rk}(\psi)+1){\rm rk}(\phi_1)}} in (@ref{GbrDer2}) may appear strange. We are pushing the @italic{operation}
@f{@(gbl)\psi,\bullet@(gbr)} ``through'' @f{\phi_1}. The operation @f{@(gbl)\psi,\bullet@(gbr)} is of the rank @f{{\rm rk}(\psi) + 1}.
}
@fsize=[]
These defining properties allows to calculate the Schouten bracket for aritrary wedge products of vectors:
@equation[#:label "BracketOfMonomials"]{
@(gbl)v_1\wedge\cdots\wedge v_p\;,\;w_1\wedge\cdots\wedge w_q@(gbr) = \ldots
}
For example:
@align[r.l.n @list[
@f{@(gbl) v_1\wedge v_2\;,\;w_1\wedge w_2 @(gbr) \;=\;} 
@f{v_1\wedge [v_2,w_1]\wedge w_2 - v_1\wedge [v_2,w_2]\wedge w_1 \;-}
""
]@list[
"" 
@f{-\; v_2\wedge [v_1,w_1]\wedge w_2 + v_2\wedge [v_1,w_2]\wedge w_1}
""
]
]
In the most general case, any elements @f{\psi\in \Gamma(\Lambda^pTM)} and @f{\phi\in \Gamma(\Lambda^qTM)} 
can be written as a sum:
@align[r.l @list[
@f{\psi\; =\;} @f{\sum_{j=1}^k v_1^{(j)}\wedge \cdots\wedge v_p^{(j)}}
]@list[
@f{\phi\; =\;} @f{\sum_{j=1}^l w_1^{(j)}\wedge \cdots\wedge w_q^{(j)}}
]
]
and then we use linearity (@ref{GbrLin2}), (@ref{GbrLin1}) to reduce the computation of the Schouten bracke
@f{@(gbl)\psi,\phi@(gbr)} to the bracket of monomials of the form (@ref{BracketOfMonomials}).

@section{Properties of the Schouten bracket}
Besides the already discussed properties (@ref{GbrLin2}), (@ref{GbrLin1}), (@ref{GbrDer2}) and (@ref{GbrDer1}),
the Schouten bracket satisfies the following additional properties:
@itemlist[#:style 'ordered 
@item{The rank behaves as follows:
@equation[#:label "SchoutenGradedRank"]{
{\rm rk}\;@(gbl) \psi,\phi @(gbr) \;= \; {\rm rk}(\psi) + {\rm rk}(\phi) - 1 
}
}
@item{Symmetry property with strange signs:
@equation[#:label "SchoutenGradedAsymmetry"]{
@(gbl) \psi,\phi @(gbr) = (-1)^{({\rm rk}\psi +1)({\rm rk}\phi + 1) + 1} @(gbl) \phi,\psi @(gbr)
}
}
@item{Jacobi identity with strange signs:
@equation[#:label "SchoutenGradedJacobi"]{
@(gbl)\psi, @(gbl)\phi,\chi@(gbr) @(gbr) = 
@(gbl)@(gbl)\psi, \phi@(gbr),\chi@(gbr) + 
(-1)^{({\rm rk}\phi + 1)({\rm rk}\psi + 1)}@(gbl)\phi, @(gbl)\psi,\chi@(gbr)@(gbr)
}
}
]
@bold{Exercise @ex-num{ExAnotherSchoutenGradedJacobi}:}
Show that (@ref{SchoutenGradedJacobi}), taking into account other properties, can be rewritten as follows:
@equation{
  (-1)^{({\rm rk} \psi + 1)({\rm rk} \chi + 1)}@(gbl)\psi, @(gbl)\phi,\chi@(gbr) @(gbr) 
+ (-1)^{({\rm rk} \phi + 1)({\rm rk} \psi + 1)}@(gbl)\phi, @(gbl)\chi,\psi@(gbr) @(gbr) 
+ (-1)^{({\rm rk} \chi + 1)({\rm rk} \phi + 1)}@(gbl)\chi, @(gbl)\psi,\phi@(gbr) @(gbr)
= 0
}

@section{Jacobi identity for the Poisson bracket and Schouten bracket of the Poisson structure}
@bold{Exercise @ex-num{JacobiIdentityFromSchouten}:}
Given a bivector @f{\pi\in \Gamma(\Lambda^2TM)}, let us define a Poisson bracket as follows:
@equation{
\{F,G\}_{\pi}(x) \stackrel{\rm def}{=} \pi^{mn}(x){\partial F(x)\over\partial x^m}{\partial G(x)\over\partial x^n} 
}
Show that the Jacobi identity of the form (@ref{JacobiIdentity}):
@equation{
\{H_1,\{H_2,H_3\}_{\pi}\}_{\pi} = \{\{H_1,H_2\}_{\pi},H_3\}_{\pi} + \{H_2,\{H_1,H_3\}_{\pi}\}_{\pi}
}
 is satisfied if and only if:
@fsize+[20]
@equation[#:label "PiPiZero"]{ @(gbl) \pi,\pi @(gbr) = 0 }
@fsize=[]
@bold{Hint:} Consider first the case when @f{\pi = v_1\wedge v_2} where @f{v_1} and @f{v_2} are two vector
fields. In this case, (@ref{PiPiZero}) follows by a direct computation.
}

@slide["Schouten bracket of differential forms" #:tag "SchoutenBracketForms" #:showtitle #t]{
@bold{This slide is a side remark.}

@div[redbox]{
On a Poisson manifold, it is possible to define a Schouten bracket on differential forms. 
}
For any polyvector field @f{v_1\wedge\cdots\wedge v_k} we can define @f{\iota_{v_1\wedge\cdots\wedge v_k}} as
follows:
@fsize+[8]
@equation{
\iota_{v_1\wedge\cdots\wedge v_k} = \iota_{v_1}\cdots\iota_{v_k}
}
@fsize=[]
Let us consider the operator:
@equation[#:label "PoissonHomologyOperator"]{
\partial_{\pi} = [d,\iota_{\pi}]
}
Let us start by observing that for two functions @f{f\in C^{\infty}M} and @f{g\in C^{\infty}M}:
@equation{
d\{f,g\} = d\iota_{\pi}(df\wedge dg)
}
@spn[attn]{TODO:} correct possible sign errors...

Motivated by this formula, let us define the Schouten bracket of two exact 1-forms @f{df} and @f{dg}
as follows:
@equation{
@(gbl) df, dg @(gbr) = [d,\iota_{\pi}]df\wedge dg
}
(Notice that the second form in the expansion of the commutator, namely @f{-\iota_{\pi}d(df\wedge dg)}
is identically zero.)

Let us look closer at the operator @f{\partial_{\pi}} of Eq. (@ref{PoissonHomologyOperator}). The first
observation is that, when @f{@(gbl)\pi,\pi@(gbr) = 0}, this operator is nilponent:
@align[r.c.l @list[
@f{\partial_{\pi}^2 = 0} @elem{ @hspace[1]@smaller{when}@hspace[1] } @f{@(gbl)\pi,\pi@(gbr) = 0}
]]

Now we can define @f{@(gbl)\alpha,\beta@(gbr)} for any 1-forms @f{\alpha}, @f{\beta} (not necessarily
closed) as follows:
@equation[#:label "KarasevBracket"]{
@(gbl)\alpha,\beta@(gbr) = \partial_{\pi}(\alpha\wedge\beta) - (\partial_{\pi}\alpha)\wedge \beta + \alpha\wedge (\partial_{\pi}\beta)
}
Notice that for a 1-form @f{\alpha}, we have @f{\partial_{\pi}\alpha = -\iota_{\pi}d\alpha}.

We will also define @f{@(gbl)f,\beta@(gbr)} for a function @f{f} and a 1-form @f{\beta} as follows:
@equation[#:label "KarasevFnForm"]{
@(gbl)f,\beta@(gbr) = - \left(
                    \partial_{\pi}(f\beta) - f\partial_{\pi}\beta
\right)
}
Moreover, we could define for any two forms @f{\omega_1} and @f{\omega_2}:
@equation[#:label "KarasevGeneral"]{
@(gbl)\omega_1,\omega_2 @(gbr) = (-1)^{{\rm rk}(\omega_1)}\left(
\partial_{\pi}(\omega_1\wedge\omega_2) - (\partial_{\pi}\omega_1)\wedge\omega_2 - 
(-1)^{{\rm rk}(\omega_1)}\omega_1\wedge\partial_{\pi}\omega_2
\right)
}

@bold{Exercise @ex-num{JacobiIdentityForForms}*:} Prove the Jacobi identity for 1-forms:
@equation[#:label "JacobiForOneForms"]{
@(gbl)\alpha,@(gbl)\beta,\gamma@(gbr)@(gbr) + 
@(gbl)\beta,@(gbl)\gamma,\alpha@(gbr)@(gbr) + 
@(gbl)\gamma,@(gbl)\alpha,\beta@(gbr)@(gbr) = 0
}
@bold{Hint:} 
Notice that for two 1-forms @f{\alpha} and @f{\beta} both @f{\partial_{\pi}\alpha} and @f{\partial_{\pi}\beta}
are functions. First prove that @f{\partial_{\pi}} is the derivation of @f{@(gbl),@(gbr)}, in the following sense:
@equation{
\partial_{\pi}@(gbl)\alpha,\beta@(gbr) = @(gbl)\partial_{\pi}\alpha,\beta@(gbr) + @(gbl)\alpha,\partial_{\pi}\beta@(gbr)
}
Then prove the following identity, valid for three 1-forms @f{\alpha}, @f{\beta} and @f{\gamma}:
@align[r.l.n @list[
@f{\partial_{\pi}(\alpha\wedge\beta\wedge\gamma) \;=\;}
@f{(\partial_{\pi}(\alpha\wedge\beta))\wedge\gamma +
(\partial_{\pi}(\beta\wedge\gamma))\wedge\alpha + 
(\partial_{\pi}(\gamma\wedge\alpha))\wedge\beta\; -}
""
]@list[
""
@f{- (\partial_{\pi}\alpha)\beta\wedge\gamma -
(\partial_{\pi}\beta)\gamma\wedge\alpha -
(\partial_{\pi}\gamma)\alpha\wedge\beta }
@label{OnlyTwoLegs}
]]
This identity follows from the fact that the action of @f{\partial_{\pi}}, as we defined it in (@ref{PoissonHomologyOperator}),
is ``essentially pairwise''. It ``cannot entangle all three @f{\alpha}, @f{\beta} and @f{\gamma}'', but only ``entangles'' them pairwise. 

Also, there is a similar identity involving a two forms and a function:
@align[r.l.n @list[
@f{\partial_{\pi}(f\alpha\wedge\beta)\;=\;}
@f{\partial_{\pi}(f\alpha)\wedge\beta - \partial_{\pi}(f\beta)\wedge\alpha + f\partial_{\pi}(\alpha\wedge\beta) \; -}
""
]@list[
"" 
@f{- f(\partial_{\pi}\alpha)\wedge \beta + f(\partial_{\pi}\beta)\wedge\alpha}
@label{OnlyTwoLegsFnFmFm}
]]
and two functions and a form:
@align[r.l.n @list[
@f{\partial_{\pi}(fg\alpha) \;= \;}
@f{\partial_{\pi}(f\alpha)g + \partial_{\pi}(g\alpha)f - fg\partial_{\pi}\alpha}
@label{OnlyTwoLegsFnFnFm}
]]
(There are similar equations for the action of @f{\partial_{\pi}} on the product of three higher-rank forms,
they look the same as (@ref{OnlyTwoLegs}) but signs are more complicated.)

Then continue:
@align[r.l @list[
@f{0 = \partial_{\pi}^2(\alpha\wedge\beta\wedge\gamma) \;=\;} 
@f{\partial_{\pi}\left(
\{\alpha,\beta\}\wedge\gamma + (\partial_{\pi}\alpha)\beta\wedge\gamma + \mbox{cycl}(\alpha,\beta,\gamma)
\right)=\ldots}
]]
Notice that Eqs. (@ref{OnlyTwoLegs}) is crucial for the derivation of the Jacobi identity (@ref{JacobiForOneForms}).
Suppose that we do not want to postulate the form (@ref{PoissonHomologyOperator}) of @f{\partial_{\pi}}. 
(But still do require @f{\partial_{\pi}} to be nilpotent.) Then the Jacobi identity will receive correction involving 
the ``triple interaction'', which is called ``Jacobiator''. Such structures appear in string theory.

@spn[attn]{Read more} about these things @hyperlink["http://en.wikipedia.org/wiki/Batalin–Vilkovisky_formalism"]{here}.
}

@slide["Integrable distributions of planes" #:tag "IntegrableDistributions" #:showtitle #t]{
Here we will introduce the notion of integrable distribution of planes, which we will need in the
discussion of the Hamiltonian reduction.

Let @f{M} be a smooth manifold, @f{{\rm dim}(M)=n}. 

Suppose that we are given, at every point @f{x\in M}, a linear subspace @f{L_x \subset T_xM}, of
the dimension @f{{\rm dim}(L_x) = k} (the dimension of @f{L_x}  should not depend on @f{x}!)

This is called a @bold{distribution of planes}, with the abbreviation:
@fsize+[10]
@equation{
L\subset TM
}
@fsize=[]
A vector field @f{v} is called ``tangent to @f{L}'', if for any point @f{x\in M}: @f{v(x)\in L_x}.
In this case we will write: @f{v\in L}.

A submanifold @f{N\subset M} is called the @bold{integral manifold} of @f{L} if @f{T_xN = L_x}. 

It turns out, that the existence of such an integral manifold is @bold{not guaranteed}. 
If such an integral manifold exists, then the distribution @f{L} is called @bold{integrable}.

@bold{Theorem @th-num{FrobeniusCriterium} (the criterium of Frobenius)}: 
A distribution @f{L} is integrable if and only if the following is true:
@itemlist[
@item{For any two vector fields @f{v} and @f{w} tangent to @f{L}, their commutator @f{[v,w]} is
also tangent to @f{L}}
]

@bold{Theorem @th-num{FrobeniusCriteriumSimplified} (particular case)}: 
Suppose that the distribution @f{L} is defined as the @bold{kernel of a 1-form} @f{\alpha}:
@equation{
L_x = \{v\in T_xM \; | \; \alpha(v) = 0\}
}
Such distribution has the dimension @f{k=n-1} (``the distribution of @bold{hyperplanes}'').
Such distribution is integrable if and only if:
@equation[#:label "IntegrabilityOfHyperplanes"]{
\alpha\wedge d\alpha = 0
}
}

@slide["Degenerate Poisson structures" #:tag "DegeneratePoissonStructures" #:showtitle #t]{
Armed with the Frobenius criterium, we will now consider @bold{degenerate} Poisson structures.
Given a smooth manifold @f{M} with a Poisson structure given by a bivector @f{\pi}, consider:
@equation{
{\rm im}_x(\pi) = \{ \xi\in T_xM \;|\; \exists\; \alpha\in T_x^*M: \; \xi^m= \pi^{mn}\alpha_n\}
}
Notice that @f{\pi} can be interpreted as a map from @f{T^*M} to @f{TM}, then @f{{\rm im}(\pi)} is
the image of this map. 

If @f{{\rm det}(\pi)\neq 0}, then @f{{\rm im}_x(\pi) = T_xM}. But suppose that @f{{\rm det}(\pi) = 0}
--- this is the @bold{degenerate case}. In this case:
@equation{
{\rm im}_x(\pi) \subset T_xM
}
--- a @bold{distribution of planes}.
@centered{
@tabular[@list[@list[@hspace[7]
@div[redbox]{Equation @f{@(gbl)\pi,\pi @(gbr) = 0} implies that this distribution is @bold{integrable}}
@hspace[7]]]]
}
To prove this, let us consider two vector fields @f{\xi_{\alpha}} and @f{\xi_{\beta}} both tangent to @f{{\rm im}(\pi)}:
@equation{
\xi_{\alpha}^m = \pi^{mn}\alpha_n \;\mbox{ \tt and }\; \xi_{\beta}^m = \pi^{mn}\alpha_n
}
To satisfy the Frobenius criterium, we need to prove that their commutator is also tangent to  @f{{\rm im}(\pi)}.
It turns out that:
@fsize+[5]
@equation{
@(gbl)\xi_{\alpha},\xi_{\beta} @(gbr) = \xi_{@(gbl)\alpha,\beta @(gbr)}
}
@fsize=[]
Here on the right hand side @f{@(gbl)\alpha,\beta@(gbr)} is a Schouten bracket of 1-forms.
This means that the Frobenius criterium is satisfied, and therefore @f{{\rm im}(\pi)} is an integrable
distribution.

The integral manifolds of @f{{\rm im}(\pi)} are called @spn[attn]{symplectic leaves}. 

@bold{Theorem @th-num{SymplecticLeaf}:}
Each symplectic leaf has a natural Poisson structure, which is non-degenerate.

@bold{Proof:} Let @f{N\subset M} be a symplectic leaf. 
We want to show that @f{\pi \in \Gamma(\Lambda^2T^*M)} defines a bivector @f{\pi_N} on @f{N},
which also satisfies @f{@(gbl)\pi_N,\pi_N@(gbr) = 0}. Specifying such a bivector  is equivalent to
specifying a bracket operation @f{\{,\}_N} on functions on @f{N}, satisfying @f{\{F,G\}_N = -\{G,F\}_N} and
@f{\{F,GH\}_N = \{F,G\}_NH + G\{F,H\}_N} for any three smooth functions @f{F}, @f{G} and @f{H} on @f{N}. 
Such an operation will necessarily be of the form 
@f{\{F,G\}_N(x) \; = \; \pi_N^{mn}(x){\partial F\over\partial x^m}{\partial G\over\partial x^n}}
for some @f{\pi_N^{mn}(x)}, which defines @f{\pi_N^{mn}}. Let us define @f{\{F,G\}} as follows. 
Pick a pair of functions @f{\widehat{F}}, @f{\widehat{G}} on @f{M} such that their restrictions on @f{N} are 
@f{F} and @f{G}:
@equation{
\widehat{F}|_N = F\;,\quad \widehat{G}|_N = G
}
Then define:
@equation{
\{F,G\}_N = \{\widehat{F},\widehat{G}\}_N 
}
It is easy to see that this definition will not depend on how we choose @f{\widehat{F}}. 
The Jacobi identity on @f{\{,\}_N} follows from the Jacobi identity on @f{\{,\}}, and therefore
@f{@(gbl)\pi_N,\pi_N@(gbr) = 0}.

}

@slide["Degeneration of symplectic structure" #:tag "DegenerateSymplectic" #:showtitle #t]{
When we defined the notion of symplectic structure, we required that @f{{\rm det}(\omega)} be nonzero.
What happens if we relax this condition?

Let us denote:
@equation{
{\rm ker}_x(\omega) = \{\xi\in T_xM\;|\; \omega(\xi,\eta) = 0\;\forall \;\eta\in T_xM\}
}
If @f{{\rm det}(\omega)\neq 0}, then @f{{\rm ker}(\omega) = 0}. But let us consider degenerate @f{\omega}.
(Such structure is sometimes called ``presymplectic''.)

Again, we can prove that @f{{\rm ker}(\omega)} is an @bold{integrable distribution}. Indeed, suppose
that two vector fields @f{\xi} and @f{\eta} belong to @f{{\rm ker}(\omega)}. We have to prove that
@f{[\xi,\eta]} are also in  @f{{\rm ker}(\omega)}. This means that we have to prove that @f{\omega([\xi,\eta],\zeta) = 0}
for any vector field @f{\zeta}. This follows from the formula which we 
@hyperlink["../vector-fields/AlgebraicExteriorDerivative.html"]{have previously proven}:
@equation{
(d\omega)(\xi,\eta,\zeta) = {\cal L}_{\xi}(\omega(\eta,\zeta)) - \omega([\xi,\eta],\zeta) + \mbox{cycl}(\xi,\eta,\zeta)
}

Consider the following equivalence relation @f{\sim} on @f{M}: two points @f{x\in M} and @f{y\in M} are considered
equivalent, if they belong to the same integral manifold of @f{{\rm ker}(\omega)}. In other words,
@f{x\sim y}  if exists a trajectory connecting @f{x} to @f{y} such that the velocity of the trajectory 
belongs to @f{{\rm ker}(\omega)}.

@tabular[@list[@list[@hspace[7]
@div[redbox]{The factor-manifold @f{M/\sim} has a (non-degenerate) symplectic structure}
@hspace[7]]]]

Let us denote @f{p:\;M\to M/\sim} the projection. For two vectors @f{\xi\in T_y(M/\sim)} and @f{\eta\in T_y(M/\sim)},
let us define the symplectic form @f{\omega(\xi,\eta)} as follows. First, let us pick a point @f{x\in M} such that
@f{y = p(x)}. (There are infinitely many such points, the set of all such points is an integral manifold of 
@f{{\rm ker}(\omega)}. Let us pick one.) Then find vectors @f{\widehat{\xi}\in T_xM} and @f{\widehat{\eta}\in T_xM}
such that:
@align[r.l.n @list[
@f{p(x) \;=\;} @f{y} ""
]@list[
@f{p_*(\widehat{\xi}) \;=\;} @f{\xi} @label{LiftOfVectors}
]@list[
@f{p_*(\widehat{\eta}) \;=\;} @f{\eta} ""
]
]
We then define:
@equation{
\omega(\xi,\eta) = \omega_M(\widehat{\xi},\widehat{\eta})
}
It remains to prove that this definition does not depend on how we choose @f{x}, 
@f{\widehat{\xi}} and @f{\widehat{\eta}} satisfying (@ref{LiftOfVectors}).

For a fixed @f{x}, independence of the choice of @f{\widehat{\xi}} and @f{\widehat{\eta}} is obvious,
because the difference between different choices is in @f{{\rm ker}_x(\omega)}.


@tabular[@list[@list[
@nested{
Independence of the choice of @f{x} can be proven as follows. Given another point @f{x'} on the
same symplectic leaf, let us connect it to @f{x} by a trajectory whose velocity vector 
is always in @f{{\rm ker}(\omega)}, see the picture. Since @f{\omega} is closed,
@equation{
\int_{ABCDA'B'C'D'}d\omega =0
}
Now let us use the Stokes theorem. Observe that the restrictions of @f{\omega} 
on @f{ADD'A'}, @f{ABB'A'}, @f{DCC'D'} and @f{BCC'B'} are zero. This implies that 
@f{\omega(\xi,\eta) = \omega(\xi',\eta')}.}
@(image (string->path "snapshots/degenerate-omega.png"))
]]]

}

@slide["Darboux theorem" #:tag "DarbouxTheorem" #:showtitle #t]{
The Darboux theorem belongs to the class of ``@bold{normal form}'' theorems.
Such theorems typically say that some geometrical object can be simplified 
(usually only locally) by choosing appropriate
coordinates. One example we have already seen: 
the @hyperlink["../vector-fields/NormalFormOfVectorField.html"]{normal form of a vector field}.

The @bold{Darboux theorem} says that for any symplectic form @f{\omega} on a manifold @f{M} of the dimension @f{2n}, 
it is possible (@bold{locally!}) 
to choose coordinates @f{(p^1,\ldots,p^n,q^1,\ldots,q^n)}  so that:
@equation[#:label "DefDarbouxCoordinates"]{
\omega = \sum_{i=1}^n dq_i\wedge dp_i
}
A very geometrical proof of the Darboux theorem can be found in the Arnold's book.

We will here sketch a different proof. Consider an infinitesimal deformation of the symplectic form:
@equation[#:label "DeformationOfTheSymplecticForm"]{
\omega \mapsto \omega + \varepsilon\Delta\omega
}
with a small parameter @f{\varepsilon}. We require that the deformed form be closed, @italic{i.e.} we 
need @f{d(\Delta\omega) = 0}.  We have to prove that this deformation can be ``undone''
by an infinitesimal change of variables:
@equation{
x \mapsto x + \varepsilon v(x)
}
Here @f{v(x)} should be tought of as a @bold{vector field} specifying our infinitesimal change of
variables. In order to cancel @f{\delta\omega}, we have to find such @f{v(x)} that:
@equation[#:label "EquationForChangeOfVariables"]{
{\cal L}_v \omega = \Delta\omega
}
As @f{d(\Delta\omega)=0}, we can locally find a 1-form @f{\alpha} such that @f{\Delta\omega = d\alpha}. 
Then, let us define @f{v} by the equation:
@equation{
\iota_v \omega = \alpha
}
Notice that the existence of such @f{v} follows from @f{\omega} being non-degenerate. On the other
hand, it follows from @f{{\cal L}_v = \iota_v d + d \iota_v} and @f{d\omega = 0} that so defined
@f{v} solves (@ref{EquationForChangeOfVariables}).

Now, the Darboux theorem follows immediately. Any symplectic form @f{\omega} can be connected
the canonical 2-form (@ref{DefDarbouxCoordinates}) by a sequence of small deformations, and every
small deformation can be undone by a change of variables.

}


@; ---------------------------------------------------------------------------------------------------

@close[formula-database]
