---
layout: post
title: The surprising rigidness of higher-rank kinds
---

Higher-rank types are a very widely used feature in GHC.
The `RankNTypes` language extension, which enables the use of higher-rank
types, has been around since GHC 6.8.1 (released in November 2007), and
by one metric, `RankNTypes` is the 15th most popular language extension in
use today [[^1]].

Not only can higher-rank polymorphism be used at the type level, but starting
with GHC 8.0, one can even use it at the _kind_ level. Despite the widespread
use of higher-rank types, however, it is surprisingly difficult to find uses
of higher-rank kinds in the wild. I was able to count on one hand the number
of unique Haskell projects on GitHub that made use of at least one higher-rank
kind somewhere in its code.

One explanation for higher-rank kinds' lack of adoption is the fact that they're
simply quite new, having only been available since 8.0. I don't think this tells
the full story, however, since extensions like `DerivingVia` and
`QuantifiedConstraints` are much more commonly used [[^2]],
and they've only been available since 8.6! I think there's an additional compounding
factor at play here: namely, _higher-rank kinds are more rigid
than higher-rank types are_, and this makes them trickier to use.
In this post, I will explore this claim in further detail and try to shed some
light on what I believe to be a feature of GHC that has languished in obscurity.

# A brief introduction to higher-rank kinds

Before I get too deep into the weeds, I want to quickly
recap what higher-rank kinds are. (If you already know what a higher-rank kind
is, feel free to skip to
[this section](#types-and-kinds-almost-one-and-the-same).)

First, let's start with higher-rank types.
A higher-rank type is one that has a `forall` appearing within a function's
argument type. One example of a function with a higher-rank type
is `foo` below:

{% highlight haskell %}
foo :: (forall a. a -> Bool) -> (Bool, Bool)
foo f = (f 1, f 'a')
{% endhighlight %}

Within the definition of `foo`, the `a` in `forall a. a -> Bool` can be
instantiated with any type whatsoever. This is precisely what allows us to
invoke `f` on both `1` and `'a'`, even though they have completely different
types. The flip side is that when calling `foo`, we must supply it an argument
that is polymorphic in `a`. We wouldn't be allowed to use
`foo not`, for instance, since `not` is of type `Bool -> Bool`, not
`forall a. a -> Bool`. On the other hand, invocations like `foo (const True)`
or `foo (const False)` are permissible, since the expressions `const True`
and `const False` are sufficiently polymorphic.

Just as `forall`s can appear within the argument type of a function, so too can
they appear within the result type. This is perfectly admissible in GHC:

{% highlight haskell %}
bar :: a -> forall b. b -> (a, b)
bar x y = (x, y)
{% endhighlight %}

Most people would not refer to the type of `bar` as higher-rank, however, since
it can be shown that it is isomorphic to the ordinary type `a -> b -> (a, b)`.
Still, it is worth pointing out that `forall`s can appear nested _after_ function
arrows, not just before them.

With the introduction of GHC 8.0, the type and kind parsers were combined. One
consequence of this change is that it now becomes possible to use higher-rank
polymorphism in kinds. Here is one example of a data type with a
higher-rank kind:

{% highlight haskell %}
data Foo :: (forall a. a -> Type) -> Type where
  MkFoo :: forall (f :: forall a. a -> Type).
           f Int -> f Maybe -> Foo f
{% endhighlight %}

Note that `f` is applied to both `Int` and `Maybe`, even though their kinds are
completely different. Just like when using the term-level `foo`, in order to use the
data type `Foo` we must pass it an argument type whose kind is sufficiently polymorphic.
We could use `Foo Proxy`, for instance, since `Proxy :: forall a. a -> Type`,
but `Foo Maybe` would be forbidden.

# Types and kinds: (almost) one and the same

The GHC users' guide makes a very bold claim in its
[Overview of Type-in-Type](https://downloads.haskell.org/~ghc/8.6.5/docs/html/users_guide/glasgow_exts.html#overview-of-type-in-type)
section:

> GHC 8 extends the idea of kind polymorphism by declaring that types and kinds are indeed one and the same.
> Nothing within GHC distinguishes between types and kinds.

While this statement is generally true, there are a handful of places where
GHC does in fact distinguish between types and kinds. One of the places where
type-kind differences leak through can be found in GHC's treatment of higher-rank
types versus higher-rank kinds. To see how this works, let us first consider
the following example of higher-rank types at work:

{% highlight haskell %}
ex1 :: (forall a b. a -> b -> Bool) -> Bool
ex1 f = f 1 'a'

giveMeTrue :: a -> b -> Bool
giveMeTrue _ _ = True

true :: Bool
true = ex1 giveMeTrue
{% endhighlight %}

Nothing about these definitions is particularly exciting—it's just a rather
indirect way of computing `True`. What is worth noting is that there is another
way to write the type of `ex1`. Instead of quantifying both `a` and `b` upfront
in the type `forall a b. a -> b -> Bool`, one can instead use a nested `forall`
to quantify `b` later than `a`:

{% highlight haskell %}
ex2 :: (forall a. a -> forall b. b -> Bool) -> Bool
ex2 f = f 1 'a'
{% endhighlight %}

Aside from the different placement of the inner `forall`, the type of `ex2` is
basically the same as the type of `ex1`. In fact, one can swap out the use of `ex1`
for `ex2` in `true`:

{% highlight haskell %}
true :: Bool
true = ex2 giveMeTrue
{% endhighlight %}

After this swap-out, `true` will still type-check. Nice!

Let's conduct a similar experiment, but this time using higher-rank kinds. First,
we need a counterpart for `ex1`. Let's use this data type with a higher-rank kind:

{% highlight haskell %}
data Ex1 :: (forall a b. a -> b -> Type) -> Type where
  MkEx1 :: forall (f :: forall a b. a -> b -> Type).
           f Int Maybe -> Ex1 f
{% endhighlight %}

Next, we'll need to pick a type that has inhabits the kind
`forall a b. a -> b -> Type`. A favorite example of mine is the following
`Equal` data type [[^3]]:

{% highlight haskell %}
data Equal :: forall a b. a -> b -> Type where
  Refl :: Equal t t
{% endhighlight %}

With these two types and hand, we can combine them like so:

{% highlight haskell %}
type ExEqual = Ex1 Equal
{% endhighlight %}

Sure enough, that kind-checks. So far, so good.

`ExEqual` is the rough analog of `true` in our previous experiment, since it
demonstrates an application of something with a higher-rank kind to an argument.
If we want to complete our current experiment, however, there is one more step
we must perform. We need to conjure up a type with a higher-rank kind that uses
a nested `forall`, just like we did with `ex2` before. Just as `ex2` was a slight
modification of `ex1`, so too can we slightly tweak `Ex1` to produce our desired
type:

{% highlight haskell %}
data Ex2 :: (forall a. a -> forall b. b -> Type) -> Type where
  MkEx2 :: forall (f :: forall a. a -> forall b. b -> Type).
           f Int Maybe -> Ex2 f
{% endhighlight %}

Again the only difference between `Ex1` and `Ex2` is that the latter uses
`forall a. a -> forall b. b -> Type`, in contrast to the former's
`forall a b. a -> b -> Type`. Now, we can wrap up by swapping out
`Ex2` for `Ex1` in `ExEqual`...

{% highlight haskell %}
type ExEqual = Ex2 Equal
{% endhighlight %}

...or so we thought. At this point, something goes horribly wrong, since GHC
complains that `ExEqual` no longer kind-checks:

{% highlight plaintext %}
    • Expected kind ‘forall a. a -> forall b. b -> Type’,
        but ‘Equal’ has kind ‘forall b. a0 -> b -> Type’
    • In the first argument of ‘Ex2’, namely ‘Equal’
      In the type ‘Ex2 Equal’
      In the type declaration for ‘ExEqual’
{% endhighlight %}

Yikes!

# `forall`: more than meets the eye

Why were we able to use `ex1` and `ex2` interchangeably but not use
`Ex1` and `Ex2` interchangeably? The answer ultimately lies in how GHC
typechecks things with `forall`s in their types (or kinds). As it turns
out, GHC spends a surprising amount of effort to make types with `forall`s
work smoothly, and this can be difficult to appreciate without seeing
an example or two of this work being done.

## `forall`s in Core

Earlier, I waved my hands and claimed that `forall a b. a -> b -> Bool` and
`forall a. a -> forall b. b -> Bool` were basically the same type. When
talking about source Haskell, this is a reasonable approximation. When
GHC compiles Haskell code, however, it turns it into a typed intermediate
language called Core. At the level of Core, these two types are very much
distinct entities. How, then, can GHC so effectively blur the distinction
between these types at the source level?

To answer this question, let's revisit the definition of `true`:

{% highlight haskell %}
true :: Bool
true = ex1 giveMeTrue
{% endhighlight %}

`true` is nice because its definition in source Haskell is almost exactly the
same as its corresponding definition in Core. We can see for ourselves what `true`
looks like in Core by compiling it with the `-ddump-simpl` flag so that GHC prints
out all compiled Core. We will also enable a handful of other flags to make this
slightly easier to read:

* __`-fmax-simplifier-iterations=0`__: This disables inlining
  (otherwise, GHC would simplify `ex1 giveMeTrue` to `True`).
* __`-dsuppress-uniques`__: This avoids printing out the unique identifier for
  each variable in Core so that we get things like `f` instead of `f_a1vT`.
* __`-dsuppress-module-prefixes -dsuppress-idinfo`__: This prevents `-ddump-simpl`
  from printing out extra debugging information that we don't care about for the
  purposes of this post.

With this combination of flags, we get the following:

{% highlight plaintext %}
$ ghc Foo.hs -ddump-simpl -fmax-simplifier-iterations=0 -dsuppress-uniques -dsuppress-module-prefixes -dsuppress-idinfo
[1 of 1] Compiling Foo              ( Foo.hs, Foo.o )

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 97, types: 109, coercions: 2, joins: 0/0}

...

-- RHS size: {terms: 5, types: 9, coercions: 0, joins: 0/0}
ex1 :: (forall a b. a -> b -> Bool) -> Bool
ex1
  = \ (f :: forall a b. a -> b -> Bool) ->
      f @ Integer @ Char 1 (C# 'a'#)

-- RHS size: {terms: 5, types: 6, coercions: 0, joins: 0/0}
giveMeTrue :: forall a b. a -> b -> Bool
giveMeTrue = \ (@ a) (@ b) _ _ -> True

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
true :: Bool
true = ex1 giveMeTrue
{% endhighlight %}

Just like I claimed earlier, we have exactly `true = ex1 giveMeTrue`. There
are some other things worthy of attention as well. For instance, notice how types are
explicitly applied as arguments using the `@ Ty` syntax (e.g., `f @ Integer @ Char` in `ex1`),
which is reminiscient of GHC's
[`TypeApplications`](https://downloads.haskell.org/~ghc/8.6.5/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications)
extension.

Also notice how type variables are explicitly abstracted using the
lambda-esque `\(@ v) -> ...` syntax (e.g., `\ (@ a) (@ b) _ _ -> True` in `giveMeTrue`).
GHC does not have any kind of syntax like this [[^4]], so this is one of the the
more unusual things to get used to when reading Core. Just as `\x -> ...` is used
to construct a something with a function type, `\ (@ v) -> ...` is used to construct
something with a `forall` type. If you see a `forall` in the type signature of a Core
definition, there's a good chance you'll see a `\ (@ v) -> ...` in its implementation
(see `giveMeTrue`, for instance).

Although GHC did not do so above, we could implement `true` in Core
using explicit type variable abstractions if we wanted to:

{% highlight haskell %}
true = ex1 (\ (@ a) (@ b) (x :: a) (y :: b) -> giveMeTrue @ a @ b x y)
{% endhighlight %}

This is equivalent to `ex1 giveMeTrue`, but with the `giveMeTrue`
subexpression eta-expanded.

## Swizzling `forall`s

Now let's go back and take a closer look at `ex2`, which uses a slightly
different order of `forall`s:

{% highlight haskell %}
ex2 :: (forall a. a -> forall b. b -> Bool) -> Bool
{% endhighlight %}

As I mentioned earlier, this type is _not_ the same as `ex1`'s type in Core.
Despite this, GHC has no problem typechecking `true = ex2 giveMeTrue` in source
Haskell. To see how GHC pulls this off, let's examine what `ex2 giveMeTrue`
looks like in Core with `-ddump-simpl`:

{% highlight haskell %}
-- RHS size: {terms: 6, types: 7, coercions: 0, joins: 0/0}
true :: Bool
true = ex2 (\ (@ a) (dk :: a) (@ b) -> giveMeTrue @ a @ b dk)
{% endhighlight %}

Interestingly, GHC does not produce `true = ex2 giveMeTrue` in Core this time
around. Instead, it uses a lambda abstraction to rearrange the type
and term variable arguments from the order that `ex2` expects:

{% highlight plaintext %}
forall a. a ->  forall b. b ->  Bool
------------------------------------
type      term  type      term
------------------------------------
@ a       dk    @ b
{% endhighlight %}

To the order that `giveMeTrue` expects:

{% highlight plaintext %}
forall a. forall b. a ->  b ->  Bool
------------------------------------
type      type      term  term
------------------------------------
@ a       @ b       dk
{% endhighlight %}

Note that there is
no need to explicitly refer to the second term variable, since it appears in
the same position in both places (and is therefore eta-contracted away). The
`@ a` argument also appears in the same position in both places, but since we
have to rearrange arguments that come after it, we end up needing to refer to it by name.

This process of swizzling variables around is accomplished in a part of
type inference called _regeneralization_. GHC does quite a bit of regeneralization
behind the scenes to take care of tiny impedance mismatches, such as differently
ordered `forall`s, so that the programmer does not have to.

## Can kinds regeneralize?

We have now seen how `ex2 giveMeTrue` typechecks, thanks to the power of
regeneralization. Can the same trick be used at the kind level? Let's look
once more at `ExEqual`:

{% highlight haskell %}
type ExEqual = Ex2 Equal
{% endhighlight %}

As before, there is no hope of compiling this to Core without at least some amount of
behind-the-scenes rearranging, since the order of `forall`s in the kinds of `Ex2` and
`Equal` do not line up. What we would need is a hypothetical type-level
lambda syntax, which I'll invent some notation for:

{% highlight haskell %}
type ExEqual = Ex2 (/\ (@ a) (dk :: a) (@ b) -> Equal @ a @ b dk)
{% endhighlight %}

If you're wondering why I'm using words like "hypothetical" and "invent", that's
because there is no such thing as `/\ (@ a) -> ...`, neither in the source language nor in Core.
Nor could GHC easily support it, since
adding a type-level lambda could potentially threaten the soundness of type
inference [[^5]]. The full details are beyond the scope of this post—see my other post
[_On the arity of type families_](../../../../2019/05/26/on-the-arity-of-type-families)
(in particular,
[this section](../../../../2019/05/26/on-the-arity-of-type-families#aside-1-why-saturation))
for more on this topic.

Because there are no type-level lambdas, GHC lacks the ability to regeneralize at the
kind level. This is what I mean when I say that higher-rank kinds are rigid: due to the
lack of kind-level regeneralization, higher-rank kinds must be instantiated in _exactly_
the order that their `forall`s prescribe. This rigidness is exactly the reason why
`Ex2 Equal` fails to kind-check.

# Is there hope for more flexibility?

To be honest, the lack of regeneralization at the kind level is kind of a bummer. It
means that types and kinds aren't quite on the same playing field in terms of expressiveness.
This difference is surprising enough that people have filed GHC issues claiming that this is
a bug ([here](https://gitlab.haskell.org/ghc/ghc/issues/13399), for instance), only to be
told that GHC is working as expected.

To work around the lack of regeneralization, one often has to jump through some hoops in order
to make the kinds align in just the right way. For instance, we saw earlier that `Ex2 Equal`
won't kind-check, since it would be like trying to fit a square peg into a round hole.
It is possible to create another version of `Equal` that does fit into a round hole, however:

{% highlight haskell %}
data Equal2' :: forall a. a -> forall b. b -> Type where
  Refl' :: Equal' t t
{% endhighlight %}

Now `type ExEqual = Ex2 Equal'` kind-checks. This is rather laborious, however—we had to
duplicate the entire definition of `Equal` just so that we could change its kind
slightly. Surely there ought to be a way to decrease the amount of hoop-jumping necessary?

As luck would have it, if you have a kind of the form `... -> Type` (which is often the case),
then there is a trick to make it somewhat easier to massage its arguments into a different order.
The trick is actually one of the oldest in the book—newtypes! In particular, we can create a
general-purpose newtype that rearranges the order of `forall`s, like so:

{% highlight haskell %}
newtype Push :: (forall a b. a -> b -> Type)
             -> forall a. a -> forall b. b -> Type where
  MkPush :: forall (f :: forall a b. a -> b -> Type)
                   a b (x :: a) (y :: b).
            f x y -> Push f x y
{% endhighlight %}

`Push` can be thought of as something which takes as input a type of kind
`forall a b. a -> b -> Type`, and produces as output a type of kind
`forall a. a -> forall b. b -> Type`. This is made possible by the fact that newtypes
can order kind variables however they please, just like data types can. This
trick might be more plain to see if we define `MkPush` using GHC's
[visible kind application](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0015-type-level-type-applications.rst)
syntax, which is available in GHC 8.8 or later:

{% highlight haskell %}
newtype Push :: (forall a b. a -> b -> Type)
             -> forall a. a -> forall b. b -> Type where
  MkPush :: forall (f :: forall a b. a -> b -> Type)
                   a b (x :: a) (y :: b).
            f @a @b x y -> Push f @a x @b y
{% endhighlight %}

With `Push`, we can give `ExEqual` a shove in the right direction:

{% highlight haskell %}
type ExEqual = Ex2 (Push Equal)
{% endhighlight %}

This kind-checks, and it has the distinct advantage that we did not have to make
a copy of `Equal` just to do so. Moreover, we can use this approach for _any_ type
of kind `forall a b. a -> b -> Type`. The downside is that you'll have to deal with the
`Push` newtype mixing up with your other types, but fortunately, GHC has plenty of
[machinery](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Coerce.html)
to deal with unwrapping newtypes these days.

I've actually used this very `Push` newtype (as well as other similar newtypes) in the
[`Data.Eq.Type.Hetero`](https://hackage.haskell.org/package/eq-4.2/docs/Data-Eq-Type-Hetero.html)
module that I contributed to Edward Kmett's
[`eq`](https://hackage.haskell.org/package/eq) package. The process of writing the code
in that module is what inspired me to write this post, in fact. The code in that
module would not have been possible to write without higher-rank kinds, but using them
does require some amount of thought to figure out how to massage the kinds
(using `Push` or otherwise) to make them do what you want.

-----

[^1]: According to Anish Tondwalkar's blog post
      [Popularity of Haskell Extensions](https://blog.ani.sh/post/176180419603/popularity-of-haskell-extensions).

[^2]: This is judging by the sheer number of GitHub results one gets when searching for
      [`DerivingVia`](https://github.com/search?l=Haskell&q=DerivingVia&type=Code) or
      [`QuantifiedConstraints`](https://github.com/search?l=Haskell&q=QuantifiedConstraints&type=Code).

[^3]: This is essentially the same thing as
      [`(:~:)`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Type-Equality.html#t::-126-:)
      from `base`, but redefined using GADT syntax to make its kind more obvious.

[^4]: At least, not currently. There is an
      [accepted GHC proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0050-type-lambda.rst)
      to add the ability to bind type variables in lambdas, however, which would add
      syntax quite similar to the one used in Core.

[^5]: It might be possible to add type-level lambdas to GHC by giving them _unmatchable_ kinds,
      distinct from the usual _matchable_ kinds.
      See the paper
      [_Higher-order Type-level Programming in Haskell_](https://www.microsoft.com/en-us/research/uploads/prod/2019/03/ho-haskell-5c8bb4918a4de.pdf),
      which describes the matchable-unmatchable distinction in more detail.
