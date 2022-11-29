---
layout: post
title: GHC curiosities&#58; Equality constraints in kinds
---

> _WARNING: This blog post describes an old GHC feature that no longer exists in
> recent versions of the compiler. See
> [GHC Proposal #547](https://github.com/ghc-proposals/ghc-proposals/pull/547),
> which lists the reasons for this feature's removal. This post is now only of
> interest for historical reasons._

In GHC, many language features available at the term level can be
straightforwardly promoted to the type level. For example, the
following term-level function:

{% highlight haskell %}
id :: a -> a
id x = x
{% endhighlight %}

Can also be defined at the type level with only minor syntactic changes [[^1]]:

{% highlight haskell %}
type Id :: a -> a
type Id x = x
{% endhighlight %}

There are still some GHC features that cannot yet be promoted, however. This
includes constraints, such as the class constraint `Eq a`. If you try to add
a constraint to the kind of a type-level definition, such as in the example
below:

{% highlight haskell %}
type EqId :: Eq a => a -> a
type EqId x = x
{% endhighlight %}

Then GHC will balk:

{% highlight plaintext %}
error:
    • Illegal constraint in a kind: forall a. Eq a => a -> a
    • In a standalone kind signature for ‘EqId’: Eq a => a -> a
  |
  | type EqId :: Eq a => a -> a
  |              ^^^^^^^^^^^^^^
{% endhighlight %}

But as it turns out, there is one exception to this rule: equality constraints.
GHC has a special case which permits kind-level equality constraints, as
evidenced by the fact that this typechecks:

{% highlight haskell %}
type BoolId :: (a ~ Bool) => a -> a
type BoolId x = x
{% endhighlight %}

In this post, we will investigate why this special case exists and explore the
things one can do with it.

# A recap of equality constraints

In GHC, an equality constraint of the form `t1 ~ t2` informs the constraint
solver that `t1` must be the same type as `t2`. A basic usage of equality
constraints can be seen in this example:

{% highlight haskell %}
boolId1 :: Bool -> Bool
boolId1 x = x

boolId2 :: (a ~ Bool) => a -> a
boolId2 x = x
{% endhighlight %}

Although `boolId1` and `boolId2` technically have different type signatures,
in practice these functions are virtually identical. This is because `boolId2`'s
equality constraint `a ~ Bool` means that the only valid way to instantiate
`a` is with `Bool`. If you try to invoke `boolId2 LT`, for instance, you will
get essentially the same error as if you invoked `boolId1 LT`:

{% highlight plaintext %}
error:
    • Couldn't match type ‘Ordering’ with ‘Bool’
        arising from a use of ‘boolId2’
    • In the expression: boolId2 LT
{% endhighlight %}

There are various situations where one might reach for equality constraints
in their GHC toolbox. When defining class instances, for example, using
equality constraints in the instance context can
[improve type inference](https://chrisdone.com/posts/haskell-constraint-trick/).
Equality constraints are also sometimes needed when defining default
implementations for class methods, as explained in
[this section](https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/exts/default_signatures.html#extension-DefaultSignatures)
of the GHC User's Guide.

# Equality constraints in kinds... well, sort of

To a first approximation, GHC permits equality constraints in the kinds of types,
just as it allows equality constraints in the types of terms. For example, GHC
will accept both of these definitions:

{% highlight haskell %}
type BoolId1 :: Bool -> Bool
type BoolId1 x = x

type BoolId2 :: (a ~ Bool) => a -> a
type BoolId2 x = x
{% endhighlight %}

If you have an equality constraint in a kind of a type, then any use site of
that type must obey the equality. For example, suppose you were to try to do
this:

{% highlight haskell %}
type U = BoolId2 LT
{% endhighlight %}

As you might expect, this will produce an error message. This time,
the error indicates a kind mismatch:

{% highlight plaintext %}
error:
    • Expected kind ‘Bool’, but ‘LT’ has kind ‘Ordering’
    • In the first argument of ‘BoolId2’, namely ‘LT’
      In the type ‘BoolId2 LT’
      In the type declaration for ‘U’
{% endhighlight %}

In this sense, equalities in the kinds of types behave much like equalities in
the types of terms. I'm refraining from saying that they behave _identically_,
however, because there are some subtle differences. Equalities in the types of
terms, for instance, permit writing definitions like this one:

{% highlight haskell %}
boolId3 :: (a ~ Bool) => a -> Bool
boolId3 x = x
{% endhighlight %}

Unlike `boolId1` and `boolId2` above, in the type of `boolId3`, the argument
type, `a`, is technically different from the result type, `Bool`. This
difference doesn't matter so much when typechecking `boolId3`, however, as
the constraint solver knows that `a` should be equal to `Bool`. As a result,
it's OK to use something of type `a` in a place where `Bool` is expected,
and vice versa.

Unfortunately, this intuition does not carry over to equalities in the
kinds of types. If you were to write the type-level version of `boolId3`:

{% highlight haskell %}
type BoolId3 :: (a ~ Bool) => a -> Bool
type BoolId3 x = x
{% endhighlight %}

Then this will _not_ kind-check:

{% highlight plaintext %}
error:
    • Expected kind ‘Bool’, but ‘x’ has kind ‘a’
    • In the type ‘x’
      In the type declaration for ‘BoolId3’
{% endhighlight %}

I found this difference to be pretty surprising when I first encountered it.
Why don't our intuitions about equalities apply here? There is a
[short answer](#the-short-explanation) and a
[long answer](#the-long-explanation).
(If you'd like, you can skip the long
answer and proceed to the rest of the post by clicking
[here](#why-the-special-treatment-anyway).)

## The short explanation

GHC's type system is simply not powerful enough at the moment to solve for
equalities at the kind level. In order to make `BoolId3` above kind-check,
GHC would need some form of type-level `case` that could decompose kind
equalities. Type-level `case` doesn't exist at the moment, however. We can
get close, as [we will see later](#faking-type-level-case-with-type-families), but for now the full power
of `case` is out of reach at the type level.

## The long explanation

The difference between GHC accepting `boolId3` and rejecting `BoolId3` lies
in the capabilities of an intermediate language that GHC uses during
compilation. This intermediate language, called Core, differs from source
Haskell in several aspects. One difference is that there is basically
no distinction between data types and constraints.
When compiling from source Haskell to Core, GHC converts constraints into
_dictionary_ data types. For instance, consider the `Bounded` type class:

{% highlight haskell %}
class Bounded a where
  minBound :: a
  maxBound :: a
{% endhighlight %}

When compiled to Core, the `Bounded` class would become the following
dictionary type:

{% highlight plaintext %}
data Bounded a = C:Bounded a a

minBound :: forall a. Bounded a -> a
minBound = \ (@a) ($dBounded :: Bounded a) ->
           case $dBounded of v { C:Bounded x y -> x }

maxBound :: forall a. Bounded a -> a
maxBound = \ (@a) ($dBounded :: Bounded a) ->
           case $dBounded of v { C:Bounded x y -> y }
{% endhighlight %}

Note that `Bounded`'s methods have now effectively become field selectors in Core. When GHC
compiles an application of a class method, it desugars the method to a
selector applied to the appropriate dictionary value. This is best
explained by example, so let's see how GHC takes this source Haskell program:

{% highlight haskell %}
minAndMax :: Bounded a => (a, a)
minAndMax = (minBound, maxBound)
{% endhighlight %}

And compiles it to Core:

{% highlight plaintext %}
minAndMax :: forall a. Bounded a -> (a, a)
minAndMax
  = \ (@a) ($dBounded :: Bounded a) ->
      (minBound @a $dBounded, maxBound @a $dBounded)
{% endhighlight %}

Here, the Core version of `minAndMax` explicitly binds `$dBounded`, a
dictionary value of type `Bounded a`. Moreover, `minAndMax` selects the
appropriate fields from `$dBounded` using `minBound` and `maxBound`.

GHC also uses the dictionary-desugaring approach when compiling equality
constraints to Core. In broad strokes, here is what an `a ~ b` constraint
looks like as a dictionary type:

{% highlight plaintext %}
data (a :: k) ~ (b :: k) = Eq# (a ~# b)

eq_sel :: forall k (a :: k) (b :: k). a ~ b -> a ~# b
eq_sel = \ (@k) (@a :: k) (@b :: k) ($d~ :: a ~ b) ->
         case $d~ of v { Eq# co -> co }
{% endhighlight %}

In Core, `a ~ b` is a data type which contains `a ~# b`, an
_unlifted equality_. The exact details of how unlifted equalities work are beyond
the scope of this post. For our purposes, it suffices to know that unlifted
equalities are special values that GHC's constraint solver makes use of to
determine when a value of one type can be safely cast to a value of a different
type. Again, this is best explained by example. Let's see where the rubber hits
the road in the `boolId3` function from before:

{% highlight haskell %}
boolId3 :: (a ~ Bool) => a -> Bool
boolId3 x = x
{% endhighlight %}

Here is the same function in Core:

{% highlight plaintext %}
boolId3 :: forall a. (a ~ Bool) -> a -> Bool
boolId3
  = \ (@a) ($d~ :: a ~ Bool) ->
      case eq_sel @Type @a @Bool $d~ of co { __DEFAULT ->
      \ (x :: a) -> x `cast` (Sub co :: a ~R# Bool)
      }
{% endhighlight %}

As the Core reveals, there's a lot happening behind the scenes in this
seemingly small function! Here is a rundown of what is going on:

* First, `boolId3` extracts the payload of the `$d~` dictionary value, an
  unlifted equality, using the `eq_sel` selector.
* Next, it `case`s on this unlifted equality to make sure it is evaluated
  strictly [[^2]]. This unlifted equality is bound to the name `co`.
* Finally, `co` is used as part of a `cast` expression which turns `x` from
  type `a` to type `Bool`.

Key to making all this work is `case`. Without `case`, we wouldn't have been
able to define `eq_sel` or `boolId3`. In today's Core, however, `case` is a
feature that is exclusively available at the term level. In contrast, Core's
sublanguage of types is rather limited, and there is no general-purpose
mechanism for pattern matching on types like what `case` offers for terms.
This is why `BoolId{1,2}` can be defined at the type level but `BoolId3`
cannot: the former can be defined without the use of `case`, while
for the latter, `case` is a requirement.

# Why the special treatment, anyway?

Given that kind-level equality constraints are so limited, one might wonder why
GHC even allows writing them in the first place. The answer is ultimately
explained in
[this Note](https://gitlab.haskell.org/ghc/ghc/-/blob/54d6b20192fe6fc244248c7766533a768c591bae/compiler/GHC/Core/TyCo/Rep.hs#L305-409)
in GHC's source code. The tl;dr version is that as a general rule, GHC tries to
make it possible to write GADT constructors using equality constraints. For
example, GHC allows writing this:

{% highlight haskell %}
data T a b where
  MkT :: T a a
{% endhighlight %}

It also allows defining `MkT` in the following way, which is essentially
equivalent:

{% highlight haskell %}
data T a b where
  MkT :: (a ~ b) => T a b
{% endhighlight %}

There is one complication with the latter version of `MkT`, however: the
`DataKinds` extension. If `MkT` is promoted to a type, then its kind would be
`(a ~ b) => T a b`, which has a kind-level equality constraint. As it turns out, however,
one can promote `MkT` to a type without running into the aforementioned issues
with type-level `case`. To support examples like `MkT`, GHC's typechecker
carves out a special case for kind-level constraints that look like `a ~ b`
or `a ~~ b`.

# They're not totally useless

Although kind-level equality constraints exist in GHC mostly due to a corner case in how
`DataKinds` interacts with GADTs, there are a handful of interesting things
that can be done with them.

## Faking type-level `case` with type families

As mentioned before, GHC doesn't have type-level `case`. But it does have
something very close in the form of type families. Ignoring some minor
differences in semantics, type families can be thought of as a way to pattern
match types at exclusively the top level, such as in this example:

{% highlight haskell %}
type Not :: Bool -> Bool
type family Not x where
  Not False = True
  Not True  = False
{% endhighlight %}

What's more, type families offer a way to use kind-level equality constraints in a
meaningful fashion. Earlier, we failed to promote the `boolId3` function to
a type synonym, but we _can_ promote it to a type family:

{% highlight haskell %}
type BoolId3 :: (a ~ Bool) => a -> Bool
type family BoolId3 x where
  BoolId3 x = x
{% endhighlight %}

Pretty cool, huh? We should be careful, however, to point out _why_ this works.
From a certain perspective, this definition of `BoolId3` is partial. This can
be seen if you examine the definition in GHCi with some additional flags
enabled:

{% highlight plaintext %}
λ> :set -fprint-explicit-kinds -fprint-explicit-coercions
λ> :info BoolId3
type BoolId3 :: forall a.
                ((a :: Type) ~ (Bool :: Type)) =>
                a -> Bool
type family BoolId3 @a @{ev} x where
    BoolId3 @Bool @{'GHC.Types.Eq# @Type @Bool @Bool <Bool>_N} x = x
{% endhighlight %}

Once again, there's a lot happening behind the scenes that isn't obvious at
first glance:

* `BoolId3` matches on `@Bool`, which means `BoolId3 x` will only reduce if
  `x` has kind `Bool`. GHC was able to figure this out by way of kind
  inference, although one could have just as well written this out explicitly
  as `BoolId3 @Bool x = x` [[^3]].
* `BoolId3` also matches on `@{'Eq# @Type @Bool @Bool <Bool>_N}` [[^4]].
  `Eq#` is the data constructor for the `(~)` dictionary data type, and
  `'Eq#` is the promoted, type-level version. Matching on this type, then,
  indicates that the `(a ~ Bool)` kind must be witnessed with a proof that
  `Bool` equals `Bool`.
* The proof that `Bool` equals `Bool` is witnessed by an unlifted equality.
  GHCi prints this unlifted equality as `<Bool>_N`.

Somewhat surprisingly, the `(a ~ Bool)` equality constraint isn't actually
required to define `BoolId3` as a type family. In fact, this is a valid
alternative definition:

{% highlight haskell %}
type BoolId4 :: a -> Bool
type family BoolId4 x where
  BoolId4 x = x
  {-
  -- Or, to be more explicit:
  BoolId4 @Bool x = x
  -}
{% endhighlight %}

The advantage of including `(a ~ Bool)`, however, is that it makes
it less likely that users will shoot themselves in the foot later. For instance,
this definition will kind-check:

{% highlight haskell %}
type UhOh = BoolId4 LT
{% endhighlight %}

This is because `LT` has kind `Ordering`, and when `BoolId4` fails to match `Ordering`
against `Bool`, it will simply become stuck. As a result, the type `BoolId4 LT` will
simply never reduce.

`BoolId3`, on the other hand, does
not suffer from this pitfall. If you attempt to write `BoolId3 LT`, then the
typechecker will throw an `Expected kind ‘Bool’, but ‘LT’ has kind ‘Ordering’`
error. GHC's constraint solver may be limited in its support for kind-level equality constraints,
but luckily, it pulls through for us in this one scenario.

## Restricting GADT return types

The only other useful application of kind-level equality constraints that I am
aware of involves GADTs. As far as I am aware, this trick originated in
[this section](https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/exts/poly_kinds.html#constraints-in-kinds)
of the GHC User's Guide, which demonstrates how to restrict the types that one
can use in a GADT constructor's return type. Here is the example from the
User's Guide:

{% highlight haskell %}
type IsTypeLit :: a -> Bool
type family IsTypeLit x where
  IsTypeLit Nat    = True
  IsTypeLit Symbol = True
  IsTypeLit x      = False

type T :: (IsTypeLit a ~ True) => a -> Type
data T x where
  MkNat    :: T 42
  MkSymbol :: T "Don't panic!"
{% endhighlight %}

Quite cleverly, the `IsTypeLit a ~ True` constraint in `T`'s kind limits its
data constructors such that they can only use return types `T x` where `x` is
either of kind `Nat` or `Symbol`. I haven't yet seen this trick used outside of
this User's Guide section, but I could envision some creative Haskellers
putting this to good use.

## Other uses?

The type family and GADT use cases are, to my knowledge, the only
not-totally-contrived situations
where one might want to reach for a kind-level equality constraint.
But then again, my imagination is somewhat limited. Perhaps you can think of
a use case that I've overlooked?

-----

[^1]: Here, the kind of `Id` is written using the `StandaloneKindSignatures`
      language extension. If you are not familiar with
      `StandaloneKindSignatures`, you may find my
      [earlier blog post](../../../../2020/01/05/five-benefits-to-using-standalonekindsignatures)
      on the topic to be informative.

[^2]: Unlike in source Haskell, `case` expressions in Core are evaluated
      strictly. Unlifted values cannot be inhabited by `⊥`, and as a result,
      they must be evaluated strictly, hence the need for a `case` in Core.

[^3]: In fact, if
      [this GHC proposal](https://github.com/ghc-proposals/ghc-proposals/pull/425)
      is accepted, then explicitly writing out the `@Bool` part would be
      required.

[^4]: Note the `@{...}` syntax, which differs from normal type application
      syntax in that it uses curly braces instead of parentheses. This syntax
      indicates a visible _dictionary_ application, which is a feature that is
      currently limited to Core.
