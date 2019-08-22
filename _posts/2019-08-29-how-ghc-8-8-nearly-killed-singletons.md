---
layout: post
title: How GHC 8.8 nearly killed singletons
---

[`singletons-2.6`](http://hackage.haskell.org/package/singletons-2.6)
has been uploaded to Hackage, coming on the heels of GHC 8.8.1's release. There
are many interesting things to say about this particular version of
`singletons`, and I invite you to check out the
[changelog](http://hackage.haskell.org/package/singletons-2.6/changelog) if you
want a quick overview.

What the changelog _doesn't_ contain is a story about how the `singletons`
library nearly perished due to changes introduced in GHC 8.8. No, it wasn't
because GHC 8.8 introduced dependent types and made `singletons` obsolete.
(Maybe some day...) Instead, GHC 8.8 nearly made it impossible to compile
`singletons` at all! This is because GHC subtly changed the way it performs
kind inference, and as a result, the code that `singletons` generates with
Template Haskell broke in spectacular fashion, as it was not designed with
this kind inference behavior in mind.

Fear not: the fact that `singletons-2.6` was released means that the developers
of `singletons` eventually found a solution to this problem. It took the
developers (myself included) a surprising amount of time to figure out what the
solution was, however! I think there is an interesting lesson to be learned
from all this, so I wrote this post in the hopes of teaching such a lesson.
This post is partly an amusing anecdote about living on the bleeding edge
of software development, and partly a cautionary tale about how seemingly
unnoticeable changes in a compiler can have unintended consequences.

# The skinny on `singletons`

`singletons` is a library for emulating dependent types in GHC.
Whether it is a _convincing_ emulation of dependent types is somewhat up to
personal interpretation, but regardless, I find it extremely useful for
experimenting with dependently typed Haskell ideas. If you aren't familiar
with `singletons`,
[this series of blog posts](https://blog.jle.im/entry/introduction-to-singletons-1.html)
is a great introduction that assumes no prior knowledge of dependent types.
If you like reading papers, you may find
[this 2012 Haskell Symposium paper](https://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf)
about `singletons` by Richard Eisenberg and Stephanie Weirich to be of interest.

One of `singletons`' most impressive feats is Template Haskell functionality
that takes function definitions as input and produces two things as output:

* Type-level versions of the same functions (i.e., _promoted_ functions)
* "Dependently typed" versions of the same functions (i.e., _singled_ functions)

For example, if you write the following code:

{% highlight haskell %}
$(singletons [d|
  not :: Bool -> Bool
  not False = True
  not True  = False
  |])
{% endhighlight %}

Then `singletons` will automatically generate the following code as output:

{% highlight haskell %}
type family Not (x :: Bool) :: Bool where
  Not False = True
  Not True  = False

sNot :: Sing x -> Sing (Not x)
sNot SFalse = STrue
sNot STrue  = SFalse
{% endhighlight %}

Now you have access to the promoted function `Not`, a type-level version of `not`, as
well as the singled function `sNot`, which allows you to write dependently typed code that makes use
of `Not`. I'm omitting quite a few details here, but that is
the high-level view of how `singletons`' Template Haskell machinery works.

One other important thing to know about `singletons` can require a _lot_ of
language extensions to use. So many language extensions, in fact, that the
`singletons` README even includes
[an entire section](https://github.com/goldfirere/singletons/tree/4ab86907c7111c2dc4f1f2a11b178b9a97d9c041#compatibility)
about it. In this blog post, we are most concerned with the `PolyKinds`
extension, as that is where things went awry in GHC 8.8.

# The GHC commit that almost ruined everything

Let's go back in time to August 2018. GHC 8.6.1 is on the cusp of being
released, as a consequence, work on GHC 8.8 has just begun. Richard Eisenberg,
the principal author of the `singletons` library, also happens to be
a pretty big deal in the GHC developer community. Richard decided that
August 21 was the day he would push through a commit named
[`Remove decideKindGeneralisationPlan`](https://gitlab.haskell.org/ghc/ghc/commit/c955a514f033a12f6d0ab0fbacec3e18a5757ab5)
so that it could be included in a future 8.8 release.

On the surface, this commit seemed innocent enough. In fact, Richard even
left a bold comment in the commit message (emphasis mine):

> This commit fixes #15141. **As it's an internal refactor**, there is
> no concrete test case for it.

Unfortunately, nothing could have been further from the truth. Although poor
Richard didn't intend to do so, he unwittingly opened a can of worms, as this
commit ground `singletons` to a complete halt.

## When internal refactors go wrong

Fortunately, I noticed that something was afoot very quickly, thanks to the
amazing infrastructure the
[`head.hackage`](https://gitlab.haskell.org/ghc/head.hackage)
repo offers for testing libraries against GHC HEAD. All of a sudden, the GHC
HEAD configuration of `singletons`' CI setup began to fail with this mysterious
error:

{% highlight plaintext %}
    • Could not deduce (SSemigroup k) arising from a use of ‘%<>’
      from the context: SSemigroup a0
        bound by the class declaration for ‘SSemigroup’
{% endhighlight %}

Despite this rather strange-looking regression, I wasn't worried yet. After all,
it's not unprecedented for libraries to stop compiling all of a sudden
with GHC HEAD. This is simply because HEAD is a rapidly moving target, and on
occasion a commit lands which introduces bugs. Under the assumption that this
was a GHC bug, I went ahead and filed
[an issue](https://gitlab.haskell.org/ghc/ghc/issues/15472).

To my surprise, however, Richard commented that he didn't believe this to be a
bug at all. Instead, he claimed that GHC was behaving correctly in this
situation and that the code that `singletons` was generating actually _should_
be rejected. Richard knows his stuff, so I reluctantly agreed with his
assessment of the situation. Unfortunately, this meant that I would have to
figure out how to patch `singletons`—which, if you can believe it, is sometimes more
difficult than patching GHC itself. In any case, this definitely seemed like something
that was possible to debug in an afternoon or two.

The `Could not deduce (SSemigroup k)` error message above was caused by
`singletons` attempting to generate a singled version of the `Semigroup` type
class, so I tried commenting it out in an attempt to get some subset of the library
to compile on HEAD. What I could not have anticipated is how deep the rabbit
hole went. Several other parts of `singletons` (which were not connected to
`Semigroup` in any way) also mysteriously failed to typecheck for unknown
reasons, and I only managed to get `singletons` to compile by commenting out
a significant chunk of the library.

One thing had become clear: working around this issue would be no small feat.

# To generalize or not to generalize

Up to this point, I've been rather vague about what exactly the underlying
issue with `singletons` + GHC 8.8 was. Unfortunately, I'm going to continue to
be vague for a little while yet. The reason is because diving right into the
technical details of the bug would require some familiarity with the
`MonoLocalBinds` language extension, which is not one of the more well known
ones. I'll take some time here to give a brief overview of `MonoLocalBinds`
before going back to the main topic.

## `MonoLocalBinds`

`MonoLocalBinds` is a very widely used language extension. This is not because
people directly enable `MonoLocalBinds` in their code, but because enabling the
use of `GADTs` or `TypeFamilies` implies `MonoLocalBinds` automatically. What
does this `MonoLocalBinds` thing do? To explain this, consider the following
code:

{% highlight haskell %}
data R a where
  RBool :: R Bool

f :: a -> R a -> Bool
f x y = let g z = not x
        in case y of
             RBool -> g ()
{% endhighlight %}

Should `f` typecheck? One possible way to answer this question is: no, `f` should be
rejected by the typechecker. Since `not :: Bool -> Bool`, calling `not x` would be tantamount to
unifying the types `a` and `Bool`, which would be a type error.

Perhaps surprisingly, there _is_ actually a way to typecheck `f`. One can infer
the following type for `g`:

{% highlight haskell %}
g :: forall b. (a ~ Bool) => b -> Bool
{% endhighlight %}

This type signature abstracts over the requirement that `a` be equal to `Bool`.
Moreover, since `g` is a locally defined function, one only needs to provide
evidence that `a` equals `Bool` at call sites for `g`, and nowhere else. The only call site for
`g` is underneath an `RBool` pattern match, where we know precisely that
`a ~ Bool`, so everything typechecks.

Although this works out, the resulting types are rather strange. The inferred
type signature for `g` mentions `a` in an equality constraint, despite the
fact that `g` is not polymorphic in `a`.
If you find this a bit unsettling, you're not
alone. The paper
[_Let Should not be Generalised_](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/tldi10-vytiniotis.pdf)
argues that these sorts of types should not be inferred for local definitions.
The authors claim that not only is this unintuitive from a programmer's
perspective, but this behavior can also be less efficient in advanced type
inference engines like the one that GHC uses.

_Let Should not be Generalised_ proposes that when
inferring the type of a locally defined function, one should not abstract over
constraints such as `a ~ Bool`. That is to say, locally defined types should
not be generalized over [[^1]]. The `MonoLocalBinds` extension is what controls this
behavior in GHC; if enabled, the types of locally defined things are not
generalized. Because `GADTs`-heavy and `TypeFamilies`-heavy code often make use of
such constraints, both of these language extensions imply `MonoLocalBinds`.

Despite the name of the paper, `MonoLocalBinds` affects more than just
`let`-bindings. Other "local" constructs in Haskell, such as `where`-clauses
and `case` alternatives are also affected by `MonoLocalBinds`.

# The kind generalization wrinkle

The `PolyKinds` extension adds another dimension to type signature generalization.
To explain what I mean by this, let's first define some helper types:

{% highlight haskell %}
data Proxy (a :: k) = Proxy
type family Any :: k
{% endhighlight %}

`Proxy` is a simple data type that is isomorphic to `()`, but with an additional
phantom type parameter with a polymorphic kind. `Proxy` does have some
[legitimate uses cases](../../../../2019-02-06-proxy-arguments-in-class-methods),
but it is also valuable for being one of the
simplest poly-kinded data types, which makes it extremely convenient for
explaining the inner workings of `PolyKinds`. `Any` is a minimalist type family
whose primary utility is being able to inhabit any kind. You can use `Any :: Type`,
`Any :: Bool`, `Any :: Maybe (Bool -> Type)`, etc., and those are all just dandy.

Now consider the following definition:

{% highlight haskell %}
h :: Proxy Any
h = Proxy
{% endhighlight %}

What is the kind of `Any` in `Proxy Any`? Since `Any` is kind-polymorphic, it
could be any kind under the sun. In this circumstance, GHC will choose to
generalize over this kind, which makes `h :: forall k. Proxy (Any :: k)`.

It is not guaranteed that GHC will always choose to kind-generalize, however.
Take a look at this (somewhat contrived) example:

{% highlight haskell %}
proxyAnyType :: Proxy (Any :: Type)
proxyAnyType = Proxy

foo :: forall x. x -> ()
foo x =
  let bar :: x -> Proxy Any
      bar _ = proxyAnyType
  in case bar x of Proxy -> ()
{% endhighlight %}

In particular, pay attention to the locally defined function `bar`. What is the
full type of `bar`? It once again uses `Proxy Any`, so there is a choice to be
made about what the kind of `Any` should be. Should the kind of `Any` be generalized
here?

In this situation, GHC chooses _not_ to generalize over the kind of `Any`. This
is because (1) `MonoLocalBinds` is in effect (note that `Any` is a type family,
and `TypeFamilies` implies `MonoLocalBinds`), and (2) `bar` is a locally defined
function that falls under the purview of `MonoLocalBinds`. The net result is
that kind generalization is switched off when inferring the type of `bar`,
so GHC defaults the kind of `Any` to `Type`. In other words, the full inferred
type of `bar` is `forall x. Proxy (Any :: Type)`.

## GHC 8.8 just _had_ to be difficult

I lied slightly in the previous paragraph. When I said "GHC chooses _not_
to generalize over the kind of `Any`", I really meant to say "up until
GHC 8.6". GHC 8.8, as it turns out, deviates from this convention slightly.
If you try typechecking `bar` on GHC 8.8, you'll actually get a type error!

{% highlight plaintext %}
    • Couldn't match type ‘k’ with ‘Type’
      ‘k’ is a rigid type variable bound by
        the type signature for:
          bar :: forall k. x -> Proxy Any
{% endhighlight %}

Eek! What on earth just happened?

The short answer is that generalization affects constraints and kinds in
orthogonal ways, and one can perform kind generalization without needing
to perform constraint generalization. In fact, this is exactly what GHC 8.8 does
with `MonoLocalBinds`, while previous GHCs would avoid both forms of
generalization for local things.

The long answer (or rather, the slightly less short answer)
is that GHC 8.8 decided to more carefully examine which places
`MonoLocalBinds` applied to. The examples in the _Let Should not be Generalised_
paper that pose trouble for GHC's type inference do not have direct equivalents
at the kind level [[^2]]. Therefore, Richard decided that having
`MonoLocalBinds` disable kind generalization for locally defined things was
needlessly conservative, so he opted to just always generalize in kinds.

As the example above proves, this isn't a strictly backwards-compatible
change, since there will now be programs that are rejected for being _too_
kind polymorphic. If `bar`'s type is generalized to
`forall k. x -> Proxy (Any :: k)`, then using `proxyAnyType` to implement
`bar` is no longer possible, since GHC cannot unify `Type` with `k`.
In this small example, it's quite possible to repair the damage, perhaps
by explicitly writing `bar :: x -> Proxy (Any :: Type)`.

# Now back to your scheduled programming

Phew, that was quite a detour. Now that I've gone over the fundamentals
of kind generalization and `MonoLocalBinds`, I can finally talk about the
actual problem with `singletons`!
Here is a stripped-down version of a piece of code from `singletons`
that no longer typechecked on GHC 8.8:

{% highlight haskell %}
$(singletons [d|
  class Foo a where
    bar :: a -> (a -> b) -> b
    baz :: a

  quux :: Foo a => a -> a
  quux x = x `bar` \_ -> baz
  |])
{% endhighlight %}

The error lies in the code that this generates with Template Haskell.
I will reproduce the code below, but for the sake of your sanity,
I would advise you not to read the actual code too closely, since it
is needlessly convoluted, machine-generated goop.
This sort of code is primarily meant for GHC's eyes, not human eyes!

{% highlight haskell %}
type family Case x arg where
  Case x arg = Baz

sQuux :: forall a (x :: a). SFoo a => Sing x -> Sing (Quux x :: a)
sQuux (sX :: Sing x)
  = sBar sX
      ((singFun1 @(LambdaSym1 x))
         (\ sArg
            -> case sArg of {
                 (_ :: Sing arg)
                   -> (case sArg of { _ -> sBaz }) ::
                        Sing (Case x arg) }))
{% endhighlight %}

This isn't even the complete code (click
[here](https://github.com/goldfirere/singletons/issues/357)
if you want the full, gory details),
but fear not—you don't need to see it all to understand the main point.
This code will typecheck on GHC 8.6.5, but not with GHC 8.8.1, where it will
fail with the following error:

{% highlight plaintext %}
    • Could not deduce (SFoo k) arising from a use of ‘sBaz’
    • In the expression: sBaz
      In a case alternative: _ -> sBaz
      In the expression:
          (case sArg of { _ -> sBaz }) :: Sing (Case x arg)
{% endhighlight %}

As it turns out, this all goes back to `MonoLocalBinds`.
A `case` branch—such as the `...` in `case sArg of { (_ :: Sing arg) -> ... }`—
is considered "local" for the purposes of `MonoLocalBinds`. Moreover,
`Case` is a poly-kinded type family definition. Since the `:: Sing (Case x arg)`
kind annotation appears in a `case` branch, `MonoLocalBinds` kicks in on
GHC 8.6.5 and prevents the return kind of `Case x arg` from being generalized
over. As a consequence, the return kind of `Case x arg` is defaulted to `a`,
and since there is an `SFoo a` constraint in scope, all is well.

On GHC 8.8, however, `MonoLocalBinds` is less constrictive, which means that
the return kind of `Case x arg` _is_ generalized over. This means
that while the user wrote `:: Sing (Case x arg)`, it is generalized to
`:: forall k. Sing (Case x arg :: k)` during kind inference. Crucially, there is no `SFoo k`
constraint in scope, which leads to the tumultuous error message witnessed
above.

## If it's broke, why not just fix it?

At this point, you might be wondering what all the hullabaloo is about.
Yes, GHC 8.8 infers more general kinds than it did in previous releases, but
surely that's not the end of the world? After all, you could make this program
typecheck by simply adding an extra `:: a` kind annotation:

{% highlight haskell %}
sQuux (sX :: Sing x)
  = sBar sX
      (({ ... :: Sing (Case x arg :: a) }))
{% endhighlight %}

Unfortunately, this is not as simple as it may seem. Recall that this code
was generated automatically by Template Haskell, which means that in order
to correctly insert this sort of kind annotation in the places that require it,
the `singletons` library would need a way to systematically identify all
such places. In general, this problem is tantamount to performing type
inference, and trying to emulate type inference in Template Haskell is
quite difficult.

As a general rule, we try to keep the
Template Haskell code in `singletons` dumb but predictable.
Trying to cram some _ad hoc_ form of type inference into the
Template Haskell machinery would fall afoul of this principle, in my
opinion, so I avoided pursuing this line of thought. Some other ideas
that I _did_ ponder, but ultimately rejected, include:

### Use fewer kind annotations

The original problem arose because of the use of an explicit
`:: Sing (Case x arg)` kind annotation. Would the problem be fixed if we
just avoiding generating that annotation entirely? Alas, the answer is "no",
because there are other programs which _require_ them in order to typecheck.
(See [here](https://github.com/goldfirere/singletons/issues/357#issuecomment-452072771) for one example.)

### Tell `MonoLocalBinds` to get bent

Since the problem appears to involve `MonoLocalBinds`, what happens if we
explicitly disable it with `NoMonoLocalBinds`? Unfortunately, that doesn't
have any effect. GHC 8.8 will generalize the kinds of local things with or
without the presence of `MonoLocalBinds`, so we're pretty much stuck with it...

## Use visible type applications

...or _are_ we stuck with it? I was getting pretty desperate until Richard
[pointed out to me](https://github.com/goldfirere/singletons/issues/357#issuecomment-454136107)
an obscure piece of GHC trivia. There is, in fact, one place where
local kind generalization does _not_ happen: within the `@(...)` part of a
visible type application. Here is an example to better illustrate this trivium:

{% highlight haskell %}
{-# LANGUAGE TypeApplications #-}

idProxy :: Proxy (Any :: Type)
idProxy = id @(Proxy Any) proxyAnyType
{% endhighlight %}

You might worry that the `@(Proxy Any)` part of this program would kind-generalize
to `@(forall k. Proxy (Any :: k))`, leading to kind mismatches like the ones
observed earlier. Luckily, this won't happen, since the arguments of visible
type applications are simply not kind-generalized! As a result,
the kind of `Any` in `@(Proxy Any)` simply defaults to `Type`, and all is well.

This trick ends up being a convenient escape hatch from the clutches of
kind generalization. Recall that our woes started because of `singletons`'
use of explicit kind annotations of the form `... :: Sing (Case x arg)`.
Instead of using kind annotations, which always trigger kind generalization
in GHC 8.8, we can instead use `id @(Sing (Case x arg)) ...`:

{% highlight haskell %}
sQuux (sX :: Sing x)
  = sBar sX
      (({ id @(Sing (Case x arg)) ... }))
{% endhighlight %}

Now we get the benefits of having the extra `Sing (Case x arg)` kind information,
with none of the drawbacks of kind generalization! Now the kind of `Case x arg`
defaults to `a`, and the typechecker continues humming along happily. Yay!

# Fixing up `singletons`

Using the lessons that I learned during this ordeal, I committed a
[patch](https://github.com/goldfirere/singletons/commit/ccd61699716be9eac1f753383965b94a2023db5a)
to `singletons` that restored its ability to build on GHC 8.8. The main
trick was taking the bit of Template Haskell code that generates explicit
kind annotations in `case` branches and rewriting it to produce equivalent
`id @(...)` code instead.

Besides the problematic `case` branches, there were also a handful of other
local definitions whose kinds generalized too much, such as in the
following example:

{% highlight haskell %}
replicateM_ :: (Applicative m) => Nat -> m a -> m ()
replicateM_ cnt0 f =
    loop cnt0
  where
    loop cnt
        | cnt <= 0  = pure ()
        | otherwise = f *> loop (cnt - 1)
{% endhighlight %}

The singled version of the `loop` function kind generalizes more in GHC 8.8 than it
did in previous releases, resulting in error messages mentioning `SNum k1`.
Luckily, in this situation there is a very simple, non-invasive way to
fix it up: just add a type signature.

{% highlight diff %}
-replicateM_       :: (Applicative m) => Nat -> m a -> m ()
+replicateM_       :: forall m a. (Applicative m) => Nat -> m a -> m ()
 replicateM_ cnt0 f =
     loop cnt0
   where
+    loop :: Nat -> m ()
     loop cnt
         | cnt <= 0  = pure ()
         | otherwise = f *> loop (cnt - 1)
{% endhighlight %}

This works well enough that I decided not to pursue further hackery to try
and allow the original program without the type signature. I conjecture that
this won't be that big of a deal in practice, since I only discovered three
functions in all of `singletons` (including `replicateM_`) that needed extra
typing information of this sort to compile.

# Conclusions

In the end, we were able to save `singletons` from imploding, thanks in no
small part to Richard's clever ideas. (Seriously, how does he think of this
stuff?) If there's one lesson to be learned from all this, it's that
small changes to the way type inference works can sometimes have big impacts.
Or, to put it another way, make sure that your "internal refactors"
don't leak to the outside world.

If you can believe it, the `id @(...)` trick wasn't the only crazy hack I
had to use in order to make the most recent `singletons` release happen.
I think I'll save discussion of these other hacks for future blog posts.

-----

[^1]: Yes, I'm aware of the fact that I'm using both the American and British
      English spellings of generalization/generalisation in this post. I'm
      too stubborn to use one spelling consistently.

[^2]: This is my attempt at paraphrasing the commit message in
      [this commit](https://gitlab.haskell.org/ghc/ghc/commit/c955a514f033a12f6d0ab0fbacec3e18a5757ab5),
      which implemented the kind generalization change in GHC 8.8.
      Any misrepresentations are mine.
