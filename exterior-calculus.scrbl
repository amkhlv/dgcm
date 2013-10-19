#lang scribble/base
@(require racket scribble/core scribble/base scribble/html-properties)
@(require (for-syntax (planet amkhlv/bystroTeX/slides_for-syntax)))
@(require (planet amkhlv/bystroTeX/common) (planet amkhlv/bystroTeX/slides))
@(require (only-in (planet jaymccarthy/sqlite) close))


@; ---------------------------------------------------------------------------------------------------
@; User definitions:
@(define bystro-conf 
   (bystro (find-executable-path "amkhlv-java-formula.sh")
           "exterior-calculus_formulas.sqlite"  ; name for the database
           "exterior-calculus_formulas" ; directory where to store .png files of formulas
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

@title{Exterior calculus}

@bystro-toc[]
@linebreak[]
@linebreak[]
@hyperlink["../index.html"]{go back to main page}

@slide["Grassmann algebra" #:tag "GrassmannAlgebra" #:showtitle #t]{
@table-of-contents[]

@section{@f{\theta}-words}
Let us consider a set of letters: 
@f{\theta^1}, … , @f{\theta^n}. Consider words composed of these letters, of the form:
@equation{
\theta^{i_1}\cdots \theta^{i_k} \;\;\mbox{ \small satisfying the condition: }\; 
i_1<i_2<\ldots<i_k
}
@bold{Exercise @ex-num{NumberOfWords}:} show that there are @f{2^n} such words.

@section{Algebra of @f{\theta}-words}
@subsection{Linear combination of words}
Consider formal linear combinations of @f{\theta}-words:
@equation[#:label "LinearCombinationOfThetaWords"]{
\Omega = \sum_{i_1<i_2<\ldots<i_k} a_{i_1\ldots i_k} \theta^{i_1}\cdots\theta^{i_k}
}
where @f{a_{i_1\ldots i_k}} are some real numbers.

Such formal linear combinations have a natural structure of a linear space, because we can add them:
@equation{
\left(\sum_{i_1<i_2<\ldots<i_k} a_{i_1\ldots i_k} \theta^{i_1}\cdots\theta^{i_k}\right) +
\left(\sum_{i_1<i_2<\ldots<i_k} b_{i_1\ldots i_k} \theta^{i_1}\cdots\theta^{i_k}\right) =
\left(\sum_{i_1<i_2<\ldots<i_k} (a_{i_1\ldots i_k} + b_{i_1\ldots i_k}) \theta^{i_1}\cdots\theta^{i_k}\right) 
}
and multiply by numbers:
@equation{
c\left(\sum_{i_1<i_2<\ldots<i_k} a_{i_1\ldots i_k} \theta^{i_1}\cdots\theta^{i_k}\right) =
\left(\sum_{i_1<i_2<\ldots<i_k} ca_{i_1\ldots i_k} \theta^{i_1}\cdots\theta^{i_k}\right) 
}

@subsection{Multiplication}
Now we will @spn[attn]{define the operation of multiplication}. We will start by defining the
multiplication of single words, and then extend this operation to the linear combinations
of the form (@ref{LinearCombinationOfThetaWords}) by linearity. For single words, we define the product
as follows:
@equation{
\theta^{i_1}\cdots \theta^{i_p}\;\cdot\; \theta^{j_1}\cdots \theta^{j_q} =
\left\{
\begin{array}{cl}
0 & \mbox{ if } i_m = j_n \mbox{ for some } m \mbox{ and } n \cr
(-1)^{\varepsilon} \theta^{k_1\ldots k_{p+q}} & \mbox{ otherwize }
\end{array}
\right\}
}
Here @f{\varepsilon} and @f{(k_1,\ldots,k_{p+q})} are defined as follows. Let us consider a sequence
of natural numbers:
@equation[#:label "SequenceWrongOrder"]{
(i_1,i_2,\ldots,i_p,j_1,j_2,\ldots,j_q)
}
The sequence @f{(k_1,\ldots,k_{p+q})} is defined as the result of the ordering @f{k_1<\ldots<k_{p+q}} 
of (@ref{SequenceWrongOrder}). Let @f{\pi\in S_{p+q}} denote the permutation which brings (@ref{SequenceWrongOrder})
into @f{(k_1,\ldots,k_{p+q})}. We define @f{(-1)^{\varepsilon}} as the sign of this permutation.

@bold{Exercise @ex-num{Associativity}:} Prove the associativity:
@equation{
(\theta^{i_1}\cdots\theta^{i_p}\;\cdot\;\theta^{j_1}\cdots\theta^{j_q})\;\cdot\;\theta^{k_1}\cdots\theta^{k_r}
=
\theta^{i_1}\cdots\theta^{i_p}\;\cdot\;(\theta^{j_1}\cdots\theta^{j_q}\;\cdot\;\theta^{k_1}\cdots\theta^{k_r})
}

}

@slide["Exterior forms" #:tag "ExteriorForms1" #:showtitle #t]{
@table-of-contents[]

@section{Polylinear functions and exterior forms}
Let @f{L} be a @bold{finite-dimensional linear space}, and @f{L^*} its dual space.

@div[comment]{
The @spn[attn]{dual space} to @f{L} is the space of all linear functions on @f{L} with values in @f{\bf R}, 
@italic{i.e.} functions @f{f(v)} such that @f{f(v+w)=f(v) + f(w)} and @f{f(cv) = cf(v)} for @f{c\in {\bf R}}.
}

A @spn[attn]{polylinear function} of the order @f{k} on @f{L}
is a function @f{f:\;\underbrace{L\times\ldots\times L}_{k \hspace{10pt}\;\;\;{\rm \small times}}\to {\bf R}} which
is linear in each argument separately.

@div[comment]{
A simplest example of a polilinear functions is a product of @f{k} linear functions, @italic{i.e.}
@f{k} elements of @f{L^*}
}

An @spn[attn]{exterior form} is a polylinear function which is antisymmetric under the exchange of any pair
of its arguments:
@equation{
f(v_1,\ldots,v_p,\ldots,v_q,\ldots,v_k) = - f(v_1,\ldots,v_q,\ldots,v_p,\ldots,v_k)\;\;\; \forall 1<q<p<k
}
Exterior forms of the order @f{k} form a linear space of the dimension @f{{(\mbox{dim }L)!\over k!(\mbox{dim }L -k)!}}
which is denoted:
@centered{
@spn[redbox]{@f-3+4{\Lambda^k L^*}
}}
(Why it is denoted like this will become clear later.)


@section[#:tag "ExteriorVsGrassmann"]{Exterior forms and Grassmann algebra}
We will denote @f{L^*} the dual linear space to @f{L},
@italic{i.e.} the space of linear functions on @f{L}. Let us choose a basis @f{\{e_i|i\in \{1,\ldots,\mbox{dim}L\}\}}
in @f{L}. Let us denote @f{\theta^i} the @bold{dual basis} in @f{L^*}. ``Dual basis'' means that:
@equation{
\theta^i(e_j) =\delta^i_j
}
Let us form a Grassmann algebra on the letters @f{\theta^i}. 

For every word @f{\theta^{i_1}\cdots\theta^{i_k}}
we will define the @bold{corresponding exterior form} (which we will also denote @f{\theta^{i_1}\cdots\theta^{i_k}})
as follows:
@equation{
\theta^{i_1}\cdots\theta^{i_k}\;\;(v_1,\ldots,v_p,\ldots,v_q,\ldots,v_k) := 
\sum_{\sigma\in S_k}\mbox{sign}(\sigma)\theta^{i_1}(v_{\sigma(1)})\cdots \theta^{i_k}(v_{\sigma(k)})
}
It is easy to see that the words @f{\theta^{i_1}\cdots\theta^{i_k}} form a basis in the space of exterior
forms on @f{L}. 

Therefore @spn[boldred]{exterior forms are elements of the Grassmann algebra}:
@equation{
\omega = \sum \omega_{i_1\ldots i_k} \theta^{i_1}\cdots\theta^{i_k}
}

We can therefore take the product @f{\omega_1\cdot\omega_2} of
two exterior forms @f{\omega_1} and @f{\omega_2} (as elements of the Grassmann algebra).
There is a traditional notation for this product:
@centered{
@spn[redbox]{@f-1+4{\omega_1\wedge\omega_2}} @hspace[5] @label["ExtProductFromGrassmann"]
}
(This means that instead of writing @f{\omega_1\cdots\omega_2} we will write @f{\omega_1\wedge\omega_2}.)
}


@slide["Tensor product of linear spaces" #:tag "TensorProducts" #:showtitle #t]{
@table-of-contents[]
@section{Crazy definition}
The constructions starts with considering the space of all formal finite linear combinations of the elements of 
@f{L_1\times\ldots\times L_k}. Notice that this space is @bold{crazy big}. It consists of all the 
expressions of the form:
@equation{
\sum_{i=1}^N c_i \;\cdot\; (v^{(i)}_1,\ldots,v^{(i)}_k)
}
This is @spn[attn]{not} @f{c_i} @bold{evaluated} on @f-2{(v^{(i)}_1,\ldots,v^{(i)}_k)}, but @f{c_i} @bold{multiplied} by a 
list @f-2{(v^{(i)}_1,\ldots,v^{(i)}_k)} of elements @f-2{v^{(i)}_j\in L}. Notice that the elements of each list are
@bold{not} the basis elements of @f{L}, but @bold{arbitrary} elements. This space is infinite-dimensional, because
it is generated by an infinite set: the set of all lists @f{(v_1,\ldots,v_k)} of elements of @f{L}.

This looks crazy, but do not be afraid! We will now ``tame'' this space by imposing the equivalence relations.
For any @f{j\in \{1,2,\ldots,n\}} we consider the following elements equivalent:
@equation{
(v_1,\ldots,a v_j^{(1)} + b v_j^{(2)},\ldots,v_k) \simeq 
a(v_1,\ldots,v_j^{(1)},\ldots,v_k)  + b(v_1,\ldots,v_j^{(2)},\ldots,v_k)
}
If we identify such elements, then our space becomes finite-dimensional, of the dimension @f{\Pi_{i=1}^k \mbox{ dim } L_i}.
It is denoted like this:
@equation{
L_1\otimes L_2\otimes\cdots\otimes L_k
}
The equivalence class of a list @f{(v_1,\ldots,v_k)} is usually denoted @f{v_1\otimes\cdots\otimes v_k}.

@section{The most important property of the tensor product}
Any @spn[attn]{polylinear function} @f{f:\; L_1\times\ldots\times L_k \;\to \;{\bf R}} can be represented
as a @spn[attn]{linear} function on @f{L_1\otimes\cdots\otimes L_k}. More precisely, for any @bold{polilinear} @f{f} we can
find a @bold{linear} function  @f{\tilde{f}:\;L_1\otimes\cdots\otimes L_k\to {\bf R}} such that:
@equation{
f(v_1,\ldots,v_k) = \tilde{f}(v_1\otimes\cdots\otimes v_k)
}
Therefore the construction of tensor product allows to @bold{reduce polylinear functions to ordinary linear functions}.


@section{Polylinear functions as elements of @f{L_1^*\otimes\cdots \otimes L_k^*}}
The simplest example of a polylinear function is the product of @f{k} linear functions; given @f{k} linear 
functions @f{f_1,\ldots,f_k}, we define the polylinear function @f{F} to be their product:
@equation[#:label "ProductOfLinearFunctions"]{
F(v_1,\ldots,v_k) := f_1(v_1)\cdots f_k(v_k)
}
This gives us a recipe for cooking a polylinear function of rank @f{k} out of @f{k} elements of @f{L^*},
and this recipe is @bold{polylinear} in each of the @f{k} elements of @f{L^*}. This means that there is
a map:
@equation{
L_1^*\otimes\cdots\otimes L_k^* \to \mbox{ \small [ polylinear functions on } L_1\times\cdots\times L_k \mbox{\small ]}
}
or equivalently:
@equation{
L_1^*\otimes\cdots\otimes L_k^* \to \mbox{ \small [ linear functions on }
L_1\otimes\cdots\otimes L_k
 \mbox{\small ]}
}
In fact, this map is an @bold{isomorphism} (bijection). In other words:
@equation[#:label "DualityAndTensorProduct"]{
\Big(L_1\otimes\cdots\otimes L_k\Big)^* \simeq
L_1^*\otimes\cdots\otimes L_k^*
}
The space @f{L_1^*\otimes\cdots\otimes L_k^*} consists of linear combinations of @f{f_1\otimes \cdots\otimes f_k}
where @f{f_j\in L_j^*}. Eq. (@ref{DualityAndTensorProduct}) tells us that for every @f{f_1\otimes \cdots\otimes f_k}
there is a corresponding polylinear function of rank @f{k} on @f{L}. This function is defined as in (@ref{ProductOfLinearFunctions}),
@italic{i.e.}:
@equation[#:label "PairingBetweenTLDualAndTL"]{
f_1\otimes \cdots\otimes f_k\;\;\;(v_1\otimes\cdots\otimes v_k) = f_1(v_1)\cdots f_k(v_k)
}
Eq. (@ref{PairingBetweenTLDualAndTL}) defines the ``pairing'':
@equation{
\Big(L_1^*\otimes\cdots\otimes L_k^*\Big)
\;\;\otimes\;\;
\Big(L_1\otimes\cdots\otimes L_k\Big)
\;\;\longrightarrow\;\; {\bf R}
}

}



@slide["Exterior product of linear spaces" #:tag "ExteriorProducts" #:showtitle #t]{
@table-of-contents[]

@section[#:tag "DefExteriorAsAntisymTensors"]{Definition 1: subspace of @f{L\otimes\cdots\otimes L}}
Now suppose that @f{L_1=\ldots =L_k} — all linear spaces are the same, @italic{i.e.} just @f{k} copies of
one linear space @f{L}. We can define the ``action'' of the symmetric group  @f{S_k} on 
@f{L\otimes\cdots\otimes L}, in the following way. For any permutation @f{\pi\in S_k}, we define:
@equation{
\pi \;.\; v_1\otimes\cdots\otimes v_k = v_{\pi(1)}\otimes\cdots\otimes v_{\pi(k)}
} 
Consider a subspace of @f{L\otimes\cdots\otimes L}, consisting of those tensors which are ``antisymmetric''
under this action, in the following sense:
@equation[#:label "DefAntisymmetricTensor"]{
\pi\;.\; t = {\rm sign}(\sigma)\;t
}
where @f{{\rm sign}(\sigma)} is the sign of the permutations, which is either @f{1} or @f{-1}
depending on whether the permutation involves odd or even number of transpositions. Such a subspace
is called  @spn[attn]{exterior product}. There is a special notation for it:
@equation{
\Lambda^k L\subset \underbrace{L\otimes\cdots\otimes L}_{k \hspace{10pt}\;\;\;{\rm \small times}}
}
The elements of this subspace are @spn[attn]{antisymmetric tensors}. 

@section[#:tag "DefExteriorAsFactorspace"]{Definition 2: factorspace of @f{L\otimes\cdots\otimes L}}
There is also an alternative definition of @f{\Lambda^k L}. Let us consider the subspace @f{R\subset L\otimes L} generated
by symmetric tensors. Given a basis @f{\{e_i\}} in @f{L}, this subspace is generated by the tensors of the form:
@equation{
\sum c^{ij} e_i\otimes e_j \; \in L \; \otimes L \;\; \mbox{ \small where } \; c_{ij} = c_{ji}
}
Consider the factorspace:
@equation[#:label "AntisymmetricAsFactorspace"]{
\frac{L\otimes \cdots \otimes L}{R\otimes L\otimes L\otimes\cdots\otimes L + L\otimes R\otimes L\otimes\cdots\otimes L +
\ldots + L\otimes L\otimes\cdots\otimes L\otimes R}
}
For example, for @f{k=4}:
@equation{
\frac{L\otimes L\otimes L\otimes L}{R\otimes L\otimes L + L\otimes R\otimes L + L\otimes L\otimes R}
}
This factorspace is isomorphic to @f{\Lambda^k L}. 

@section{Equivalence of two definitions}
By the second definition (@ref{AntisymmetricAsFactorspace}), we define @f{\Lambda^k L} as the space of tensors
@f{T\in L\otimes\cdots\otimes L} @bold{modulo equivalence relations}: 
@itemlist[
@item{Two tensors @f{T_1} and @f{T_2} are considered equivalent in @f{\Lambda^k L}, iff their difference is 
a sum of tensors, symmetric under the transposition of  @bold{some} neighbors (but could be different pairs of
neighbors in different elements of the sum)}
]
The crucial observation is:
@itemlist[
@item{Given a tensor @f{T\in L\otimes\cdots\otimes L}, we can always find an antisymmetric tensor @f{A}, such that
the difference @f{T-A} is in the denominator of (@ref{AntisymmetricAsFactorspace}):
@equation{
T - A \;\;\in\;\; {R\otimes L\otimes L\otimes\cdots\otimes L + L\otimes R\otimes L\otimes\cdots\otimes L +
\ldots + L\otimes L\otimes\cdots\otimes L\otimes R}
}
}
]
In other words any tensor is equivalent, in that sense, to some antisymmetric tensor.
}

@slide["Tensor algebra" #:tag "secTensorAlgebra" #:showtitle #t]{
@table-of-contents[]
@section{Definition of algebra}
Definition of @spn[attn]{algebra} can be found @hyperlink["http://en.wikipedia.org/wiki/Algebra_over_a_field#Definition"]{on Wikipedia}.
This is a linear space with an associative operation of multiplication, which (besides associativity) should also satisfy the 
distributivity law:
@equation{
x\cdot (y+z) = x\cdot y + x\cdot z \quad,\quad (y+z)\cdot x = y\cdot x + z\cdot x
}

@section{Tensor algebra}
@bold{Tensors form an algebra}, which is denoted @f{T(L)}. As a linear space:
@equation{
T(L) = \bigoplus\limits_{k=0}^{\infty} T^k(L)\quad \mbox{\small where} \quad T^k(L) = 
\underbrace{L\otimes\cdots\otimes L}_{k \hspace{10pt}\;\;\;{\rm \small times}}
}
--- this is the space of @bold{finite} linear combinations of tensors of different rank.
The definition of the @bold{product} of tensors is very simple:
@equation{
(v_1\otimes\cdots\otimes v_p)\cdot (w_1\otimes\cdots\otimes w_q) = 
v_1\otimes\cdots\otimes v_p \otimes w_1\otimes\cdots\otimes w_q
}
@spn[redbox]{Tensor algebra @f{T(L)} is also called @bold{free algebra generated by} @f{L}}

}

@slide["Exterior algebra" #:tag "ExteriorAlgebra" #:showtitle #t]{
For a set of @f{k} vectors in @f{v_1,\ldots,v_k\in L} we define an element @f{v_1\wedge \cdots\wedge v_k\in \Lambda^k L}
as follows:
@itemlist[#:style 'ordered
@item{Using @secref{DefExteriorAsAntisymTensors}:
@equation{
v_1\wedge\ldots\wedge v_k = 
{1\over k!}\sum\limits_{\sigma\in S_k}\mbox{ sign}(\sigma)\; v_{\sigma(1)}\otimes\cdots\otimes v_{\sigma(k)} 
}
}
@item{Using @secref{DefExteriorAsFactorspace}:
@equation{
v_1\wedge\ldots\wedge v_k = v_1\otimes\cdots\otimes v_k \mbox{ \small mod } 
R\otimes L\otimes L\otimes\cdots\otimes L + L\otimes R\otimes L\otimes\cdots\otimes L + \ldots
}
}
]
These two definitions are equivalent.

The second definition allows us to define the  @bold{structure of associative algebra} on antisymmetric tensors. It is also denoted @f{\wedge}.
Namely, given @f{\omega_1\in \Lambda^pL} and @f{\omega_2\in\Lambda^qL}, we pick some representatives @f{\hat{\omega}_1\in T(L)} and 
@f{\hat{\omega}_2\in T(L)}, then take the tensor product and then take the equivalence class:
@equation{
\omega_1\wedge \omega_2 = \hat{\omega}_1\otimes \hat{\omega}_2 \mbox{ \small mod } 
R\otimes L\otimes L\otimes\cdots\otimes L + L\otimes R\otimes L\otimes\cdots\otimes L + \ldots
}
The right hand side does not depend on how we choose the representatives @f{\hat{\omega}_1} and @f{\hat{\omega}_2}.
Associativity follows from the associativity of the tensor algebra.
}

@slide[@elem{Subtlety in the definition of pairing between @f{\Lambda^kL^*} and @f{\Lambda^kL}} #:tag "SubtletyInPairing" #:showtitle #t]{
@table-of-contents[]
@section{Bad definition}
As we said (see @secref{DefExteriorAsFactorspace}) we can think of @f{\Lambda^kL^*} and @f{\Lambda^kL} as subspaces
of @f{L^*\otimes \cdots\otimes L^*} and @f{L\otimes\cdots\otimes L} respectively. (Those are the subspaces 
consisting of antisymmetric tensors.) There is a natural pairing between @f{L^*\otimes \cdots\otimes L^*} and @f{L\otimes\cdots\otimes L}
which is defined in Eq. (@ref{PairingBetweenTLDualAndTL}). Let us restrict this natural pairing to antisymmetric tensors.
Then we will get the following pairing between the antisymmetric tensors:
@equation[#:label "BadDefinitionOfPairing"]{
f_1\wedge\cdots\wedge f_k\;\;(v_1\wedge\cdots\wedge v_k) = {1\over k!} \sum\limits_{\pi\in S_k}
\mbox{sign}(\pi)f_1(v_{\pi(1)})\cdots f_k(v_{\pi(k)})\quad \mbox{\bf (Dont use!)}
}

@section{Good definition}
We will instead define the pairing as follows:
@equation[#:label "GoodDefinitionOfPairing"]{
f_1\wedge\cdots\wedge f_k\;\;(v_1\wedge\cdots\wedge v_k) =  \sum\limits_{\pi\in S_k}
\mbox{sign}(\pi)f_1(v_{\pi(1)})\cdots f_k(v_{\pi(k)})
}
@bold{This is the definition which we will use.} 
The difference with (@ref{BadDefinitionOfPairing}) is in the absence of @f{1\over k!} in front. 

Observations:
@itemlist[
@item{Our definition @bold{does not} agree with the natural pairing between @f{L^*\otimes \cdots\otimes L^*} and @f{L\otimes\cdots\otimes L}
which is given by (@ref{PairingBetweenTLDualAndTL}). We define  pairing between antisymmetric tensors in a special way, different
from the definition of the pairing of generic tensors.}
@item{Our definition does agree with the definition (@ref{ExtProductFromGrassmann}) which we gave earlier in the context of Grassmann algebras}
]
@bold{Exercise @ex-num{ProductOfExtForms}:}
Show that the wedge-product of a @f{p}-form and a @f{q}-form is given by:
@equation{
(\omega_p\wedge \omega_q) (v_1,\ldots,v_{p+q}) = {1\over p! q!}\sum\limits_{\pi\in S_{p+q}}\mbox{sign}(\pi)
\omega_p(v_{\pi(1)},\ldots,v_{\pi(p)})\omega_q(v_{\pi(p+1)},\ldots,v_{\pi(p+q)})
}
}

@slide["Summary" #:tag "SummaryOnExteriorAlgebra" #:showtitle #t]{
We introduced the basic notions of exterior calculus.

The main object is @seclink["GrassmannAlgebra"]{Grassmann Algebra}.

@seclink["ExteriorForms1"]{Exterior form} is an antisymetric polylinear function, and they are
@seclink["ExteriorVsGrassmann"]{in one-to-one correspondence} with elements of a Grassmann algebra.

Polylinear functions are reduced to linear functions by the construction of 
@seclink["TensorProducts"]{tensor product}. This also leads to the concept of @seclink["secTensorAlgebra"]{Free Algebra}
— the algebra of tensors.

@seclink["ExteriorProducts"]{Exterior product} of linear spaces can be defined as either a subspace,
or a factorspace, of the tensor product. These two definitions are equivalent.

When we try to @seclink["ExteriorAlgebra"]{repeat the definition of Free Algebra using exterior product} 
instead of tensor product, we get the Grassmann Algebra which we already 
@seclink["GrassmannAlgebra"]{introduced using @f{\theta}-words}.

Finally, we observe that there is a @seclink["SubtletyInPairing"]{natural duality pairing} between
the exterior product of spaces and exterior product of dual spaces. This brings us back
to @seclink["ExteriorForms1"]{antisymmetric polylinear functions}.
}

@; ---------------------------------------------------------------------------------------------------

@close[formula-database]
