---
layout: post
title: Visible dependent quantification in Haskell
---

> _This post is based off of a talk I gave on March 8, 2019, that was
> unfortunately not recorded. In lieu of video, I decided to write this blog
> post so that I could share it with others. The slides of the talk are
> available [here](../../../../talk-slides/vdq-wonks.pdf), although you do not need to
> read them in order to understand this post._

I recently implemented a new sort of kind in GHC that you couldn't write before.
Here is one example of such a kind:

{% highlight haskell %}
data T :: forall k -> k -> Type
{% endhighlight %}

No, that's not a typo—that reads `forall k -> {...}`, not `forall k. {...}`.
In other words, `forall k ->` is a _visible_, _dependent_ quantifier. What
exactly do those words mean? What does this let you do that you couldn't before?
Does this have any relationship with the fabled "Dependent Haskell" we've heard
so many rumors about? And why was I crazy enough to implement this?

Before I answer any of these questions, I want to take a tour through some
more familiar Haskell kinds in the hope that it will more easily motivate the rest
of this post.

# Kinds: a recap

Here is a data type that most Haskellers will likely be familiar with:

{% highlight haskell %}
data Either a b where
  Left  :: a -> Either a b
  Right :: b -> Either a b
{% endhighlight %}

Note that I'm using GADT syntax here instead of the "traditional" syntax of
`data Either a b = Left a | Right b`. The part that I want to draw attention to
is the kind of `Either`. If you ask GHCi what `Either`'s kind is, it will tell you:

{% highlight plaintext %}
λ> :kind Either
Either :: Type -> Type -> Type
{% endhighlight %}

As this suggets, the `Either` type constructor takes two `Type`s as arguments
and returns a `Type` as the result [[^1]]. The thing is, you have to squint a
bit at the definition of `Either` to realize this. The fact that `Either` takes
two `Type`s as arguments is implied by the two type variables in
`data Either a b`. The fact that `Either` returns a `Type` is not spelled out
at all; it's an implied consequence of `Either` being a data type.

While it is not impossible to reverse-engineer that
`Either :: Type -> Type -> Type` from the declaration `data Either a b` alone,
I prefer to be explicit about the kind of a data declaration whenever possible.
For this reason, I try to use the following, alternative syntax for GADT declarations:

{% highlight haskell %}
data Either :: Type -> Type -> Type where
  Left  :: a -> Either a b
  Right :: b -> Either a b
{% endhighlight %}

This declaration is equivalent to the one above, except now we've made it very
explicit what the kind of `Either` is. In this simple example, it's perhaps not
such a big deal, but when fancier kinds enter the picture (e.g.,
`(Type -> Type) -> Type`), then this syntax can often provide clarity that
type variables alone cannot.

To contrast these two styles of GADT syntax, I'll refer to the `data Either a b`
syntax as "type-variable style" and the `data Either :: Type -> Type -> Type`
syntax as "return-kind style". Note that these syntaxes are not mutually
exclusive, and you can combine the two styles if you wish:

{% highlight haskell %}
data Either a :: Type -> Type where
  ...
{% endhighlight %}

## Kind polymorphism

Another important thing to keep in mind in this kind of discussion is
_kind polymorphism_, which you can enable with GHC's `PolyKinds` language
extension. Here is an example of `PolyKinds` in action:

{% highlight haskell %}
data TypeRep (a :: k) where
  TRInt   :: TypeRep Int
  TRChar  :: TypeRep Char
  TRTrue  :: TypeRep True
  TRFalse :: TypeRep False
{% endhighlight %}

`TypeRep` is a stripped-down version of the data type
that can be found in the
[`Type.Reflection`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Type-Reflection.html#t:TypeRep)
module in the `base` library. One of its distinguishing characteristics is that
its argument `a` has kind `k`, where `k` is a _kind_ variable that can fill in for
any kind. In the types of `TRInt` and `TRChar`, for instance, `k` is instantiated
to be `Type`, which is the kind of both `Int` and `Char`. In the types of
`TRTrue` and `TRFalse`, however, `k` is instantiated to be `Bool`, which is the
kind of `True` and `False` [[^2]].

Note, however, that we don't spell out explicitly what `k` gets instantiated to.
That is because GHC infers what `k` should be behind the scenes, quite helpfully.
This will become important later on, so keep this in mind.

Now that we've seen how to define `TypeRep` in the type-variable style, a
question naturally arises: what is the corresponding definition in return-kind
style? As we did before, let's ask GHCi what the kind of `TypeRep` is. While
I'm at it, I'll also turn on the `-fprint-explicit-foralls` flag, since I like
to know where my variables are being bound:

{% highlight plaintext %}
λ> :set -fprint-explicit-foralls
λ> :kind TypeRep
TypeRep :: forall k. k -> Type
{% endhighlight %}

Now this is one cool kind. This says that for _all_ kinds `k`, `TypeRep` accepts
something of kind `k` as an argument and returns a `Type` as a result. Sure
enough, this is exactly what we need to define `TypeRep` in return-kind style:

{% highlight haskell %}
data TypeRep :: forall k. k -> Type where
  ...
{% endhighlight %}

Nice, we're on a roll now. Let's see if we can keep it up.

## Variables that are both types and kinds

At this point in the post, you might be inclined to believe that types and kinds
are two separate constructs in Haskell. As it turns out, however, that's not the
case! Starting with GHC 8.0, types and kinds are really the same thing. We simply
reserve the phrase "kind" to refer to a type of another type.

This type-kind distinction melts away when you realize that you can bind
something as a type variable and then later refer to it in the kind of another
type. For example, suppose that we wanted to make the `k` in `TypeRep`
something that the user has to explicitly write out themselves. How might one
do this? As is the case with many of life's problems, the Haskell solution is
"throw a newtype on it":

{% highlight haskell %}
newtype TypeRep2 k (a :: k) where
  MkTR2 :: forall k (a :: k). TypeRep a -> TypeRep2 k a
{% endhighlight %}

It's worth staring at this definition for a bit.
In `newtype TypeRep2 k (a :: k)`, the first argument
`k` is just an ordinary type variable of kind `Type`. In the second argument,
however, `k` lives a double life as the kind of `a`. To put it another way,
the kind of `a` _depends_ on `k`. Hm, there's that word "depends" again.
Depends... depend... dependent? We'll come back to that point later.

For now, let's perform the usual exercise of trying to define `TypeRep2` using
return-kind syntax. Let's ask our old friend GHCi what the kind of `TypeRep2`
is:

{% highlight plaintext %}
λ> :kind TypeRep2
TypeRep2 :: forall k -> k -> Type
{% endhighlight %}

Ooh, that's a fancy-looking kind. I wonder what that means. In any case, let's
first complete the exercise:

{% highlight haskell %}
newtype TypeRep2 :: forall k -> k -> Type where
  ...
{% endhighlight %}

At this point, I load the code back into GHC (8.6) and am greeted with...
this?

{% highlight plaintext %}
VDQ.hs:142:30: error: parse error on input ‘->’
    |
142 | newtype TypeRep2 :: forall k -> k -> Type where
    |                              ^^
{% endhighlight %}

Wat. I simply used the kind that GHC told me, and now it's giving me
parse errors? Did GHC lie to me? Something must be afoot here.

# Enter visible dependent quantification

It turns out that GHC isn't lying, but it is rather bad at communicating what
it can and can't do. `forall k -> k -> Type` is a perfectly valid kind,
but as of GHC 8.6, it can only be reasoned about within GHC's internals.
Importantly, GHC doesn't provide any way to write this kind directly in the source
syntax. It can only be expressed indirectly through declarations like
`newtype TypeRep2 k (a :: k)`, which happens to have that kind due to the way
the arguments are used. GHCi will even be cheeky and report this if you query
`:kind TypeRep2`, but this information is strictly read-only.
The nerve of some compilers, I swear...

So now that we've resolved our little misunderstanding with GHC, there's still
a lingering question: just what does `forall k -> k -> Type`
_mean_, anyway? The remarkable part of this kind is the `forall k -> {...}` bit,
since we're not used to seeing arrows immediately following `forall`ed things.
This is a _visible_, _dependent_ quantifier. Let's break down that phrase
in further detail.

## Visible

_Visibility_ refers to the property of whether something is explicitly
written out in the source language or not. A _visible_ argument to a type
constructor must be spelled out explicitly, whereas an _invisible_ argument
does not appear explicitly—it's inferred behind the scenes.

We have already seen a couple examples of visibility in action. Let's recall
our earlier `TypeRep` example:

{% highlight haskell %}
data TypeRep :: forall k. k -> Type where
  TRInt   :: TypeRep Int
  TRChar  :: TypeRep Char
  TRTrue  :: TypeRep True
  TRFalse :: TypeRep False
{% endhighlight %}

I told a minor lie in the previous section when I said that the `TypeRep` type
constructor accepts one argument. In reality, `TypeRep` accepts _two_
arguments. The first argument is an _invisible_ argument `k`, as embodied by the
`forall k.` part in the declaration. The second argument is a _visible_ argument
of kind `k`, as embodied by the `k ->` part.

This point is really driven home in each `TypeRep` constructor, as only the
visible argument is written out explicitly. For instance, the type of `TRInt`
only shows the visible argument `Int`. The invisible argument `Type`, however,
is nowhere to be seen. If you _do_ wish to see it, however, you can coax GHCi
into showing it by enabling the `-fprint-explicit-kinds` flag:

{% highlight plaintext %}
λ> :set -fprint-explicit-kinds
λ> :type TRInt
TRInt :: TypeRep Type Int
{% endhighlight %}

The takeaway from all this is that `forall` (with a dot) is how we can
quantify invisible things in Haskell (as opposed to `->`, which gives us
visible things).

## Dependent

_Dependency_ is the property that parts of a type can refer to things quantified
earlier in the type. This word famously appears in the phrase "dependent types",
but you don't need full-blown dependent types in order to have dependency. In
fact, we just saw an example of dependency in the previous section, in `TypeRep`:

{% highlight haskell %}
data TypeRep :: forall k. k -> Type where
  ...
{% endhighlight %}

In the kind of `TypeRep`, the `k -> Type` portion _depends_ on `k`, which was
quantified earlier in the kind. If you instantiate `k`, then the rest of the
kind will change accordingly. For instance, instantiating `k` to be `Bool` will
make the rest of the kind become `Bool -> Type`. On the other hand, the `Bool`
in `Bool -> Type` is non-dependent. Regardless of which `Bool` you pass as
an argument, the resulting kind will always be `Type`, since it does not depend
on _which_ `Bool` we use.

The takeaway from this is that `forall` is how we quantify dependent things,
whereas `->` gives us non-dependent things.

## Visible _and_ dependent

Now that we know what visibility and dependency are, what happens if we put
these properties together? You get the funny `forall k -> {...}` syntax that we
saw earlier. Here, `k` is visible in the sense that one must explicitly spell
out the argument to instantiate `k` with in the source code, and it is dependent
in the sense that the rest of the kind (`{...}`) can refer to `k`.

We can see both of these traits in action by using GHCi. Recall the kind of
`TypeRep2` from before:

{% highlight plaintext %}
λ> :kind TypeRep2
TypeRep2 :: forall k -> k -> Type
{% endhighlight %}

Since the first argument to `TypeRep2` is visible, we can pick the argument to
instantiate `k` with by simply passing it to `TypeRep2`. Let's try a couple of
examples:

{% highlight plaintext %}
λ> :kind TypeRep2 Bool
TypeRep2 Bool :: Bool -> Type
λ> :kind TypeRep2 Type
TypeRep2 Type :: Type -> Type
{% endhighlight %}

This also shows off the fact that `k` is dependent, since the result kind changes
depending on which argument we choose.

That's pretty much all there is to know regarding how visible dependent quantification works.
It took me a while to explain the prerequisite concepts, but when you put it all
together, it's surprisingly natural.

# So about that parse error...

How did we get into the sad situation where can talk about visible dependent
quantification, but not actually write it out? It all comes back to GHC 8.0, the
first release in which types and kinds were merged. The esteemed Richard Eisenberg,
who implemented this merger, was hesitant to add the `forall k -> {...}` syntax,
as he initially received feedback that this was poor syntax.
Ironically, he later submitted a
[GHC proposal](https://github.com/ghc-proposals/ghc-proposals/pull/81) asking for
better designs, and no one could come up with anything better than
`forall k -> {...}`. In the end, Richard got to have the last laugh on this one.

The aforementioned GHC proposal was accepted some time back, but it sat
unimplemented for a long time. Part of the reason that it remained on the
backburner for so long is that implementing Dependent Haskell would require
exposing the syntax for visible dependent quantification anyway [[^3]],
so it wasn't seen as an urgent priority.

## But I want it _now_

The thing is, visible dependent quantification—or VDQ, as I'll abbreviate
it from here on out—has a habit of appearing in unexpected places. One surprising
place where it popped up was in a
[different GHC proposal](https://github.com/ghc-proposals/ghc-proposals/pull/54)
for adding top-level kind signatures. This would allow one to write a standalone
kind signature for any type-level entity, such as in the following example:

{% highlight haskell %}
type MyEither :: Type -> Type -> Type
type MyEither = Either
{% endhighlight %}

In addition to this new bit of syntax, the proposal also suggests
that, after a certain window of time, all polymorphically recursive type-level
declarations in GHC _must_ have a top-level kind signature in order to kind-check.
This would replace GHC's current, _ad hoc_ metric that it uses to determine
when polymorphic recursion in a type-level entity is permitted [[^4]].

Unfortunately, it turns out that if this requirement were imposed on today's GHC,
then there would be existing code that would break. Here is one example:

{% highlight haskell %}
data Foo k (a :: k) where
  MkFoo :: Foo (k1 -> k2) f -> Foo k1 a -> Foo k2 (f a)
{% endhighlight %}

The definition of `Foo` is polymorphically recursive, so under the new rules,
`Foo` would require a top-level kind signature. But that signature,
`type Foo :: forall k -> k -> Type`, would
require VDQ to write! This led to the realization that this proposal
depends (har har) on VDQ existing before it can be implemented.

This isn't even the only proposal that requires VDQ. A
[separate proposal](https://github.com/ghc-proposals/ghc-proposals/pull/177)
for constrained type families would also require VDQ at certain spots to
be feasible. The writing on the wall was becoming clear: if I wanted GHC
to have nice things, then someone was going to have to implement VDQ first.

## Surely _someone_ must be working on it?

I remembered that Richard Eisenberg, the force of nature behind merging types
and kinds in GHC, also had plans to implement Dependent Haskell... soon?
If true, that would be great timing, since getting Dependent Haskell
would naturally imply getting VDQ as a consequence. I
was curious to know exactly how soon we could expect Dependent Haskell to land,
so I decided to check out
[his most recent blog post](https://typesandkinds.wordpress.com/2016/07/24/dependent-types-in-haskell-progress-report/)
about the upcoming roadmap for Dependent Haskell, in which he had this to say:

> **When can we expect dependent types in GHC?**
>
> The short answer: GHC 8.4 (2018) at the very earliest. More likely 8.6 or 8.8 (2019-20).

Hm. Both GHC 8.4 and 8.6 have already been released, neither of which had any
sign of VDQ. That must mean that Dependent Haskell is landing in GHC 8.8, right?
I checked out the
[GHC 8.8 status page](https://gitlab.haskell.org/ghc/ghc/wikis/status/ghc-8.8.1),
and while there are lots of nifty optimizations and other knick-knacks planned,
it made no mention of Dependent Haskell.

No reason to worry yet, though! After all, there could still be time to add
Dependent Haskell to the roadmap, right? Let's see how much time we have
remaining before GHC 8.8 is released:

> 15 March 2019: Final release

15 March is... today? Oh. Oh no.

I gradually realized two important lessons from all this:

1. Never trust GHC-related release dates.
2. If you want something to be implemented soon, you've got to implement it yourself.

# Implementing it myself

After lamenting the fact that Dependent Haskell (and thus VDQ) wasn't happening
any time soon, I decided that it might be faster just to implement VDQ
myself. After all, how hard could it possibly be? Clearly, GHC
had the machinery to reason about these kinds internally, so all it would take
is someone to expose this functionality in the source language.
I set out to do just that.

To my delight, my initial attempt at writing a patch to add VDQ to GHC turned
out to be shockingly simple. Here is an excerpt from my changes to GHC's
parser (I've omitted some irrelevant details):

{% highlight diff %}
--- a/compiler/parser/Parser.y
+++ b/compiler/parser/Parser.y

+forall_vis_flag :: { ForallVisFlag }
+        : '.'  { ForallInvis }
+        | '->' { ForallVis   }
+

 -- A ctype is a for-all type
 ctype   :: { LHsType GhcPs }
-        : 'forall' tv_bndrs '.' ctype             {％ ...
+        : 'forall' tv_bndrs forall_vis_flag ctype {％ ...

-                 HsForAllTy { hst_bndrs = $2
+                 HsForAllTy { hst_fvf = $3
+                            , hst_bndrs = $2
{% endhighlight %}

The important part here is that instead of hardcoding the use of a dot after
a `forall`, I replaced the dot with a new parser production that lets it use
either a dot (for invisible arguments) or an arrow (for visible ones). I also
store this information in the new `hst_fvf` field of `HsForAllTy` (GHC's AST
form for `forall` types) so that GHC can make use of it later.

From there, most of the changes I had to make to GHC were routine changes
brought about by the introduction of `hst_fvf`. The only changes that were
particularly interesting were the changes to the typechecker, but even then
they were quite small. Here is an abridged version of the typechecker-related changes:

{% highlight diff %}
--- a/compiler/typecheck/TcHsType.hs
+++ b/compiler/typecheck/TcHsType.hs

 --------- Foralls
-tc_hs_type forall@(HsForAllTy { ... })
+tc_hs_type forall@(HsForAllTy { hst_fvf = fvf, ... })
   = do { ...
-       ; let bndrs       = mkTyVarBinders Specified tvs'
+       ; let argf        = case fvf of
+                             ForallVis   -> Required
+                             ForallInvis -> Specified
+             bndrs       = mkTyVarBinders argf tvs'
{% endhighlight %}

Before, the type variable binders in a `forall` type were always set to
"`Specified`", which is GHC's internal jargon for invisible things. To support
VDQ, I simply dispatch on whether the `forall` is visible or not, and if it
is visible, I choose "`Required`", which is GHC's internal jargon for visible
things.

That's it! With those modest changes, I had finally implemented VDQ.

## ...or so I thought

Life is rarely that simple, unfortunately. There were a couple of snags that
I hit along the way that required some further thought.

### What does `forall` really mean, anyway?

Having programmed in GHC for a while, I was accustomed to thinking that `forall`
was a keyword. But I forgot that the Haskell Report actually does not give the word
"`forall`" any special meaning, and that it's really GHC that treats `forall`
specially. To make things worse, whether or not GHC treats `forall` specially depends
on what _language extensions_ are enabled. To illustrate what I'm getting at,
consider this program:

{% highlight haskell %}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
import Data.Kind

data Wat :: forall k -> k -> Type
{% endhighlight %}

This appears to use VDQ, so one might think that there's no way today's GHC could
ever parse this. In fact, I used to think this myself until I tried loading this
into GHC 8.6 and discovering that it _worked_. Baffled, I tried asking GHCi
what the kind of `Wat` was:

{% highlight plaintext %}
λ> :kind Wat
Wat :: forall {forall :: Type -> Type} {k}. forall k -> k -> Type
{% endhighlight %}

Double wat. GHC was treating the `forall` in `forall k` as if it were the name of a
type variable! I remembered that you need to enable the `ExplicitForAll` language
extension [[^5]] in order to parse `forall` specially. If you enable that, then
you at least get, erm, different results:

{% highlight plaintext %}
$ ghc ForAllWeirdness.hs -XExplicitForAll
[1 of 1] Compiling Main             ( ForAllWeirdness.hs, ForAllWeirdness.o )

ForAllWeirdness.hs:5:22: error: parse error on input ‘->’
  |
5 | data Wat :: forall k -> k -> Type
  |                      ^^
{% endhighlight %}

The good news was that the VDQ patch would make this parse error go away. The
bad news was that depending on whether a user remembered to enable
`ExplicitForAll` or not, `forall k -> k -> Type` would represent two completely
different kinds, neither of which was more general than the other. This smelled
like a disaster waiting to happen, so I decided that something needed to be
done about this predicament.

After pondering this with Richard for some time, we came to the realization that
`forall` really ought to be a keyword in GHC. In practice, almost all code
written in contemporary Haskell (i.e., GHC) assumes that `forall` is a keyword
in type signatures, given the widespread use of language extensions like
`ScopedTypeVariables`, which transitively enable `ExplicitForAll`. So Richard
submitted [a proposal](https://github.com/ghc-proposals/ghc-proposals/pull/193)
to make `forall` always a keyword in types, which was promptly accepted. This
does mean that GHC now departs slightly from the Haskell Report, but this is
nothing new, as even making `Applicative` a superclass of `Monad` technically
violates the Haskell Report.

The upshot is that since `forall` really is now a keyword in all types, if
you try compiling the above program with the VDQ patch and forget to enable
`ExplicitForAll`, then you'll get a proper error message about it:

{% highlight plaintext %}
ForAllWeirdness.hs:5:13: error:
    Illegal symbol ‘forall’ in type
    Perhaps you intended to use RankNTypes or a similar language
    extension to enable explicit-forall syntax: forall <tvs>. <type>
  |
5 | data Wat :: forall k -> k -> Type
  |             ^^^^^^
{% endhighlight %}

### What can be visibly dependent?

Unfortunately, GHC cannot support VDQ in certain places at the moment. VDQ is
fine in the kind of a type-level entity, but it is not yet usable in the type
of a term. Here is an example of something that GHC cannot do yet:

{% highlight haskell %}
blah :: forall a -> a -> a
blah _ = undefined
{% endhighlight %}

The ability to define `blah` would require implementing more pieces of Dependent Haskell
that are not in GHC at the moment. In other words, VDQ is OK in the kinds of types,
since GHC has already merged types and kinds, but it is _not_ OK in the types of
terms, since GHC has not yet merged terms and types.

On the other hand, GHC has the same parser for types and kinds, so in my initial
implementation of VDQ, GHC actually _accepted_ `blah`! Yikes. Thankfully, this problem
was simple to avoid: just throw an error if GHC encounters
VDQ in any place that is unambiguously the type of a term. Now, If you try compiling
`blah` with the VDQ patch, you'll get the following error:

{% highlight plaintext %}
Blah.hs:4:9: error:
    • Illegal visible, dependent quantification in the type of a term:
        forall a -> a -> a
      (GHC does not yet support this)
    • In the type signature: blah :: forall a -> a -> a
  |
4 | blah :: forall a -> a -> a
  |         ^^^^^^^^^^^^^^^^^^
{% endhighlight %}

# Coming soon to a GHC near you

Aside from these two minor hurdles, nothing else about the VDQ patch was especially
challenging to implement. I took the patch and submitted a
[merge request](https://gitlab.haskell.org/ghc/ghc/merge_requests/378)
to GHC about four weeks ago, and after two weeks of discussion and review, it
was finally merged. This means that VDQ will officially debut in
GHC 8.10 [[^6]], but if you want to try it out sooner than that, you can
download a prebuilt version of GHC HEAD from
[here](https://gitlab.haskell.org/ghc/ghc/-/archive/master/ghc-master.zip).

To conclude, I want to demonstrate something cool that you can do with
VDQ that you couldn't before. VDQ does bring us slightly closer to Dependent
Haskell than before, and a natural thing to wonder is if VDQ lets you write
dependently typed programs. The answer to that question is "yes"! ...But
the catch is that you can only have dependent types at the kind level :)

Here is one example of something that you can do in Agda, a dependently typed
programming language. Just like in Haskell, you can define function composition
in Agda:

{% highlight plaintext %}
_∘_ : ∀ {a : Set} {b : Set} {c : Set} →
        (b → c) → (a → b) → (a → c)
f ∘ g = λ x → f (g x)
{% endhighlight %}

Note that `Set` is (roughly) Agda's equivalent of Haskell's `Type` and that
curly braces denote invisible arguments. Agda can actually go one step further
and define _dependent_ function composition, where the types of the functions
involved can depend on their inputs:

{% highlight plaintext %}
_∘_ : ∀ {a : Set} {b : a → Set} {c : {x : a} → b x → Set} →
        (∀ {x : a} (y : b x) → c y) → (g : (x : a) → b x) →
        ((x : a) → c (g x))
f ∘ g = λ x → f (g x)
{% endhighlight %}

It turns out that with VDQ, we can define a Haskell version of dependent
function composition at the type level. Here it is, in its full glory:

{% highlight haskell %}
type DComp a (b :: a -> Type) (c :: forall (x :: a). b x -> Type)
           (f :: forall (x :: a). forall (y :: b x) -> c y)
           (g :: forall (x :: a) -> b x)
           (x :: a)
  = f (g x)
{% endhighlight %}

Admittedly, I'm cheating a bit here, since `DComp` has `a`, `b`, and `c` as
visible arguments, whereas they're invisible in the Agda version. Unlike with
data types, it's not easy to declare an argument to a type synonym to be visible,
and we would likely
need something like top-level kind signatures in order to give `a`, `b`, and `c`
the intended visibility. But the fact that you can even get this close is still
pretty amazing, in my opinion. I'm looking forward to seeing what other
interesting use cases people come up with for this feature.

-----

[^1]: If you want to very precise, then you would say that `Either` takes
      exactly one `Type` as an argument as returns something of kind
      `Type -> Type` as the result, but I'll avoid being overly pedantic for
      the sake of this post.

[^2]: I say "the kind of `True` and `False`" instead of "the type of `True` and
      `False`" here simply because I'm referring to uses of `True` and `False`
      at the type level, which is possible due to GHC's `DataKinds` extension.

[^3]: See also [this GHC proposal](https://github.com/ghc-proposals/ghc-proposals/pull/102),
      which proposes to add _every_ new quantifier that will appear in
      Dependent Haskell, not just visible dependent quantification.

[^4]: See the [users' guide section](https://downloads.haskell.org/~ghc/8.6.4/docs/html/users_guide/glasgow_exts.html#complete-user-supplied-kind-signatures-and-polymorphic-recursion)
      on **c**omplete, **u**ser-**s**pecified **k**ind signatures (or CUSKs) for more
      information on this _ad hoc_ metric in use in today's GHC.

[^5]: Or one of several other language extensions that imply `ExplicitForAll`,
      such as `ExistentialQuantification`, `RankNTypes`, or `ScopedTypeVariables`.

[^6]: Even though GHC 8.8 hasn't been released as of the time of writing, the
      window for new 8.8 features _has_ passed, so it is unfortunately too
      late to get VDQ into GHC 8.8.
