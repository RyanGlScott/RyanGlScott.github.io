---
layout: post
title: Leibniz equality in Haskell, part 2&#58; heterogeneous equality
---

_This is the second part in my two-part series on Leibniz equality in
Haskell. See [here](../../../../2021/08/22/leibniz-equality-in-haskell-part-1)
for part 1. Part 2 will build on ideas and concepts from part 1, so if you
haven't read part 1 yet, go do so!_

In part 1 of this blog series, we introduced Leibniz equality, an alternative
to propositional equality that converts equal things by way of explicit
substitution. Here is how to define Leibnizian equality in Haskell as a data
type:

{% highlight haskell %}
type (:=) :: k -> k -> Type
newtype a := b = Refl { subst :: forall c. c a -> c b }
{% endhighlight %}

We spent a lot of time in part 1 analyzing the second line of this code
snippet, which contains the definition of `(:=)`. In contrast, we spent very
little time talking about the first line, which declares the _kind_ of `(:=)`.
The kind `k -> k -> Type` states that if there is an equality `a := b`, then
`a` and `b` must both have the kind `k`. Because `(:=)` requires both type
arguments to have the same kind, `(:=)` is a form of _homogeneous_ equality.

Homogeneous equality is not the only game in town, however. In this part of
the blog series, we will examine _heterogeneous_ equality, a more exotic form of
equality where the type arguments are allowed to have different kinds. In
particular, we will implement a heterogeneous form of Leibniz equality. This
will put GHC's kind system to the test, as we will need higher-rank kinds
in order to define and use heterogeneous Leibniz equality.

# What exactly is heterogeneous equality?

Before diving into the "Leibniz" part of the phrase "heterogeneous Leibniz equality",
let's take a moment to understand the "heterogeneous" part. The terms
"homogeneous" and "heterogeneous" are more commonly used in the natural sciences
to refer to how uniform a substance is. A homogeneous substance is uniform
throughout, while a heterogeneous substance lacks uniformity. There are
examples of homogeneity and heterogeneity in
[physics](https://en.wikipedia.org/wiki/Homogeneity_(physics)),
[climatology](https://en.wikipedia.org/wiki/Homogenization_(climate)),
[biology](https://en.wikipedia.org/wiki/Homogenization_(biology)),
[chemistry](https://en.wikipedia.org/wiki/Mixture#Homogeneous_and_heterogeneous_mixtures),
[ecology](https://en.wikipedia.org/wiki/Species_homogeneity), and
more.

When applied to equality types, the homogeneous-heterogeneous distinction
refers to the kinds of the argument involved. To show you what I mean,
let's recall the definition of propositional equality in Haskell:

{% highlight haskell %}
type (:~:) :: k -> k -> Type
data a :~: b where
  Refl :: a :~: a
{% endhighlight %}

This is a homogeneous form of equality, as both `a` and `b` must have the same
kind. It is permissible to have equalities like `Int :~: Int` and `Maybe :~: Maybe`,
but something like `Int :~: Maybe` would be forbidden, since `Int :: Type` but
`Maybe :: Type -> Type`.

The `Data.Type.Equality` module also provides a heterogeneous version of `(:~:)`,
called [`(:~~:)`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Type-Equality.html#t::-126--126-:)
[[^1]]:

{% highlight haskell %}
type (:~~:) :: j -> k -> Type
data a :~~: b where
  HRefl :: a :~~: a
{% endhighlight %}

Remarkably, the definitions of `(:~:)` and `(:~~:)` are quite similar. Aside
from renaming `(:~:)` to `(:~~:)` and `Refl` to `HRefl` (short for "heterogeneous
`Refl`"), the only real difference is that the kind has changed from
`k -> k -> Type` to `j -> k -> Type`. That is, the kinds of the argument types
are now allowed to differ.

Despite being so similar to `(:~:)`, GHC wasn't able to define `(:~~:)` at all
until GHC 8.0. This is because prior to GHC 8.0, GADT constructors could equate
types, but not kinds. Note that the `HRefl` constructor not only equates the
types `a` and `b`, it also equates their kinds `j` and `k`. GHC 8.0 made the
treatment of types and kinds more consistent, which paved the way for both
heterogeneous propositional equality (as shown above) and heterogeneous
Leibniz equality (as we will see later).

## Why should I care about heterogeneous equality?

At this point, one might legitimately ask the question: why is heterogeneous
equality such a big deal? Upon a cursory glance, it might seem like the ability
to have argument types with different kinds isn't that useful. That would give
you the ability to write `Int :~~: Maybe` as a type, for instance, but if you
actually tried to write `HRefl :: Int :~~: Maybe`, it would fail to typecheck,
since `HRefl` ultimately requires the kinds of its arguments to be the same.

The real power of `(:~~:)` shines when its arguments are polymorphic. One such
scenario arises when dealing with length-indexed vectors, a variation of the
list data type that records how many elements are in the list at the type
level:

{% highlight haskell %}
type Vec :: Type -> Nat -> Type
data Vec a n where
  VNil :: Vec a Z
  (:>) :: a -> Vec a n -> Vec a (S n)
{% endhighlight %}

The number of elements are recorded as a `Nat`, which is a convenient way to
represent the natural numbers:

{% highlight haskell %}
type Nat :: Type
data Nat where
  Z :: Nat        -- The number zero
  S :: Nat -> Nat -- The successor of a number (i.e., n+1)
{% endhighlight %}

We can define a type family which converts from a length-indexed vector to
an ordinary list of the same length:

{% highlight haskell %}
type FromVec :: Vec a n -> [a]
type family FromVec v where
  FromVec VNil      = '[]
  FromVec (x :> xs) = x : FromVec xs
{% endhighlight %}

We can also define a type family which goes in the opposite direction [[^2]]:

{% highlight haskell %}
type ToVec :: forall a. forall (l :: [a]) -> Vec a (Len l)
type family ToVec l where
  ToVec '[]      = VNil
  ToVec (x : xs) = x :> ToVec xs
{% endhighlight %}

This makes use of an auxiliary type family to compute the length of the
argument list:

{% highlight haskell %}
type Len :: [a] -> Nat
type family Len l where
  Len '[]      = Z
  Len (_ : xs) = S (Len xs)
{% endhighlight %}

Now let's try to prove that `FromVec` and `ToVec` are inverse operations. That
is, `FromVec (ToVec l)` is equal to `l` for all lists `l`, and
`ToVec (FromVec v)` equals `v` for all length-indexed vectors `v`. The former
property can be stated using homogeneous equality: `FromVec (ToVec l) :~: l`.
If we try to state the latter property using `ToVec (FromVec v) :~: v`,
however, we encounter a roadblock:

{% highlight plaintext %}
error:
    • Couldn't match kind ‘n’ with ‘Len (FromVec v)’
      Expected kind ‘Vec a n’,
        but ‘ToVec (FromVec v)’ has kind ‘Vec a (Len (FromVec v))’
{% endhighlight %}

What is going on here? Let's inspect the type a bit more closely, making the
kinds of each type clear with an explicit `forall`:

{% highlight haskell %}
forall (a :: Type) (n :: Nat) (v :: Vec a n).
  ToVec (FromVec v) :~: v
{% endhighlight %}

The use of `(:~:)` requires that the kinds of `ToVec (FromVec v)` and `v` be
the same. The kind of `v` is `Vec a n`, while the kind of
`ToVec (FromVec v)` is `Vec a (Len (FromVec v))`. In order for these kinds to
be equal, `Len (FromVec v)` must be the same as `n`. While it is possible to
prove that this is true, GHC's typechecker does not perform this kind of
reasoning on its own. As a result, GHC conservatively assumes that
`Len (FromVec v)` is not equal to `n`.

While these kinds are technically not the same, they would become the same if
we could perform case analysis on `v`. For instance, is `v` is equal to `VNil`,
then `n` must be equal to `Z`. Moreover, `FromVec VNil` reduces to `'[]`, so
`Len (FromVec VNil)` would reduce to `Len '[]`, which is also `Z`. We can
perform similar reasoning when `v` is `(:>)`. For this reason, it's not quite
accurate to say that `ToVec (FromVec v)` doesn't equal `v`—it's just that
`(:~:)` isn't flexible enough to let us encode this fact.

This is exactly the kind of situation where heterogeneous equality shines.
Because the argument types are allowed to have different kinds in a
heterogeneous equality, the type `ToVec (FromVec v) :~~: v` _does_ kind-check.
The full details of the proof of `ToVec (FromVec v) :~~: v` are beyond the
scope of this post, but I've posted a gist
[here](https://gist.github.com/RyanGlScott/a28a17f136bf32eaa8ce02e39dff5ce3)
for curious readers who want to read more.

Another important use case for heterogeneous equality is as a building block
for the `Data.Dynamic` module, which enables the use of dynamic types in a
statically typed setting. For more details, see the paper
[_A reflection on types_](https://www.seas.upenn.edu/~sweirich/papers/wadlerfest2016.pdf).

# Heterogeneous Leibniz equality

## Definition

Now that we've seen the role that heterogeneous equality can play in a propositional
setting, let's see if we can do the same thing in a Leibniz setting. Recall
the definition of homogeneous Leibniz equality:

{% highlight haskell %}
type (:=) :: k -> k -> Type
newtype a := b
  = Refl { subst :: forall c. c a -> c b }
{% endhighlight %}

What should we change to make this heterogeneous? If we apply the same changes
as we did going from `(:~:)` to `(:~~:)`, then we end up with something like
this:

{% highlight haskell %}
type (:==) :: j -> k -> Type
newtype a :== b
  = HRefl { hsubst :: forall c. c a -> c b }
{% endhighlight %}

This time, however, just changing the kind signature isn't enough, as GHC will
reject this definition:

{% highlight plaintext %}
error:
    • Expected kind ‘j’, but ‘b’ has kind ‘k’
    • In the first argument of ‘c’, namely ‘b’
      In the type ‘forall c. c a -> c b’
      In the definition of data constructor ‘HRefl’
{% endhighlight %}

Note that `c` is applied to both `a` and `b`, which have different kinds.
Since `c` is declared without a kind, GHC must infer what its kind is.
Due to the way GHC's type inference works, when it encounters `c a -> ...`,
GHC infers that `c` has the kind `j -> Type`. Later, GHC sees `c b`, but
because `b` has kind `k` rather than `j`, it rejects the application
as a kind error.

There is a way to make this definition well-kinded, however: give `c` a
[higher-rank kind](https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/exts/poly_kinds.html#higher-rank-kinds).
Like kind equalities, higher-rank kinds is a feature that
first appeared in GHC 8.0. GHC won't infer a higher-rank kind on its own, so
we must give `c` such a kind explicitly:

{% highlight haskell %}
type (:==) :: j -> k -> Type
newtype a :== b
  = HRefl { hsubst :: forall (c :: forall i. i -> Type). c a -> c b }
{% endhighlight %}

`c` having the kind `forall i. i -> Type` indicates that it must be able to
be applied to an argument type of any kind. For example, `Proxy` (from the
[`Data.Proxy`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Proxy.html#t:Proxy)
module) is one valid instantiation of `c`, as `Proxy` can be applied to `Int`,
`Maybe`, and types of any other kind. On the other hand, `[]` would not be
a valid instantiation of `c`; while `[Int]` is well kinded, `[Maybe]` would
not be.

Because of `c`'s higher-rank kind, we now have the power to apply it to both
`a` and `b`. This power doesn't come for free, however, as we are now more
limited in what contexts we can use to instantiate `c`. As we will see
later, this adds an extra wrinkle when trying to implement functions that
use `(:==)`.

## `(:==)` is an equivalence relation

Like its homogeneous counterpart `(:=)`, the `(:==)` type is an equivalence
relation. That is, `(:==)` is reflexive, symmetric, and transitive. Let's
prove all three of these.

### Reflexivity

The proof of reflexivity for `(:==)` is essentially identical to that of `(:=)`:

{% highlight haskell %}
refl :: a :== a
refl = HRefl id
{% endhighlight %}

Just as with `(:=)`, all uses of `(:==)` essentially boil down to
the identity function.

### Symmetry

The proof of symmetry for `(:==)`, on the other hand, throws us an unexpected
curveball. Drawing upon the proof of symmetry for `(:=)` as inspiration, one
may be tempted to try this:

{% highlight haskell %}
type Symm :: j -> k -> Type
newtype Symm a b = Symm { unsymm :: b :== a }

symm :: forall a b. (a :== b) -> (b :== a)
symm aEqualsB = unsymm (hsubst aEqualsB @(Symm a) (Symm refl))
{% endhighlight %}

Surprisingly, `symm` doesn't typecheck:

{% highlight plaintext %}
error:
    • Expected kind ‘forall i. i -> Type’,
        but ‘Symm a’ has kind ‘k0 -> Type’
    • In the type ‘(Symm a)’
      In the first argument of ‘unsymm’, namely
        ‘(hsubst aEqualsB @(Symm a) (Symm refl))’
      In the expression: unsymm (hsubst aEqualsB @(Symm a) (Symm refl))
{% endhighlight %}

To understand why this doesn't typecheck, we need to take a careful look at the
kind of `Symm a`. The full kind of `Symm` is `forall j k. j -> k -> Type`. When
`Symm` is applied to `a`, GHC must instantiate `Symm` with invisible kind
arguments to match up with the `forall j k. ...` part of `Symm`'s kind.

GHC ends up picking `Symm @j @k0 a`. The first invisible argument `@j` comes
from the fact that `a :: j`. There isn't an obvious choice
for the second invisible argument, however. As a result, GHC's kind inference engine
picks a fresh kind variable `k0` and hopes that inference will figure out what
`k0` should be later. (This explains why the error message mentions `k0`,
despite it not appearing anywhere in the original program.)

The kind of `Symm @j @k0 a` is `k0 -> Type`. It should be clarified that this
is _not_ the same thing as `forall k0. k0 -> Type` in this context, and this is
the root of the issue. The `c` in the type of `hsubst` is required to have a
higher-rank kind—that is, one where the kind variable is quantified by a `forall`.
Since `k0 -> Type` is not a higher-rank kind, GHC rejects this program as
being ill-kinded.

The fact that `k0 -> Type` cannot be generalized to `forall k0. k0 -> Type` is
somewhat surprising, since this a property that is unique to kinds. For a more
in-depth explanation on why this is the case, refer to my other blog post
[_The surprising rigidness of higher-rank kinds_](../../../../2019/07/10/the-surprising-rigidness-of-higher-rank-kinds).
The tl;dr version is that `Symm` has the wrong kind for our needs. We need a
different version of `Symm` such that `Symm a` really does have kind `forall k. k -> Type`.

There are two different ways to get what we seek. One is to define an entirely
new data type with an appropriate kind:

{% highlight haskell %}
type Symm' :: forall j. j -> forall k. k -> Type
newtype Symm' a b = Symm' { unsymm' :: b :== a }
{% endhighlight %}

The only tangible difference between `Symm` and `Symm'`, aside from their names,
are the kinds involved. `Symm'` has a kind with a nested `forall`. This makes an
important difference when applied to invisible type arguments. Note that `Symm a b`
is shorthand for `Symm @j @k a b`, while `Symm' a b` is shorthand for `Symm' @j a @k b`.

The order of `forall`s is of utmost importance when `Symm` and `Symm'` are
partially applied. As discussed above, `Symm a` is shorthand for `Symm @j @k0 a`,
and since `@j` and `@k0` instantiate both of the `forall`'d type variables in `Symm`'s kind,
`Symm @j @k0` has kind `k0 -> Type`.

On the other hand, `Symm' a` is shorthand for
`Symm' @j a`. This is because the kind of `Symm'` starts with `forall j. j -> ...`,
so only one invisible type argument needs to be supplied. As a result,
`Symm' @j a` has kind `forall k. k -> Type`, which is exactly what we are looking for.
This means that `Symm' a` is a suitable type context to instantiate `c` with, as
evidenced by the fact that this typechecks:

{% highlight haskell %}
symm :: forall a b. (a :== b) -> (b :== a)
symm aEqualsB = unsymm' (hsubst aEqualsB @(Symm' a) (Symm' refl))
{% endhighlight %}

While this works, it's slightly unsatisfying that we had to duplicate the entire
definition of `Symm` just to give it a slightly different kind. An alternative
to this approach is to still use `Symm`, but to define a general-purpose way to
rearrange the order of `forall`s in a kind. Like with many problems we've
encountered up to this point, the solution is to define another newtype:

{% highlight haskell %}
type Push :: (forall j k. j -> k -> Type)
          -> forall j. j -> forall k. k -> Type
newtype Push p a b = Push { unpush :: p a b }
{% endhighlight %}

`Push` is a peculiar newtype that swizzles around the order of kind variables,
converting something that quantifies all of its kind variables upfront to
something that quantifies one of its kind variables in a nested fashion. Or,
to put it another way: `Push p @j a @k b` is a thin wrapper around
`p @j @k a b`.

The `p` can be instantiated with many different type constructors, and in our
case, we will instantiate it with `Symm`. Note that `Push Symm` has kind
`forall j. j -> forall k. k -> Type`, making `Push Symm` essentially identical
to `Symm'`. Here is how to implement `symm` using `Push Symm`:

{% highlight haskell %}
symm :: forall a b. (a :== b) -> (b :== a)
symm aEqualsB =
  unsymm (unpush (hsubst aEqualsB @(Push Symm a) (Push (Symm refl))))
{% endhighlight %}

Personally, I prefer this way of implementing `symm`, even if it is a bit more
verbose. Defining `Push` will ultimately save us some typing in the long run,
as it will prevent us from having to re-define several existing data types to
give them different kinds. This won't be the last time we use `Push`!

### Transitivity

To round out the proof that `(:==)` is an equivalence relation, let's prove
transitivity. Once again, I'll naïvely port over the implementation of `trans`
for homogeneous Leibniz equality:

{% highlight haskell %}
trans :: forall a b c. (a :== b) -> (b :== c) -> (a :== c)
trans aEqualsB bEqualsC = hsubst bEqualsC @((:==) a) aEqualsB
{% endhighlight %}

We run into similar troubles here as we did with `symm`, however, since
the context `((:==) a)` also doesn't have the right kind:

{% highlight plaintext %}
error:
    • Expected kind ‘forall i. i -> Type’,
        but ‘(:==) a’ has kind ‘k0 -> Type’
    • In the type ‘((:==) a)’
      In the expression: hsubst bEqualsC @((:==) a) aEqualsB
      In an equation for ‘trans’:
          trans aEqualsB bEqualsC = hsubst bEqualsC @((:==) a) aEqualsB
{% endhighlight %}

Like `Symm`, `(:==)` has kind `forall j k. j -> k -> Type`, so
`((:==) a` is shorthand for `((:==) @j @k0 a)`. As a result, it also
has kind `k0 -> Type`.

In order to repair this, we are again presented with two options. The first option
is to redefine `(:==)` with the kind `forall j. j -> forall k. k -> Type` and
use that instead. I, for one, am feeling lazy, so I'm going to pick the second
option: use `Push (:==)` instead. The very same `Push` data type we defined
earlier works just as well in this situation:

{% highlight haskell %}
trans :: forall a b c. (a :== b) -> (b :== c) -> (a :== c)
trans aEqualsB bEqualsC =
  unpush (hsubst bEqualsC @(Push (:==) a) (Push aEqualsB))
{% endhighlight %}

## Converting from and to homogeneous equality

In part 1 of this blog series, I went from demonstrating `(:=)` is an
equivalence relation directly to showing how to implement `castWith`, the
type-safe cast function. In this part, however, I'm going to do things in
a slightly different order. Before getting to `castWith`, I'll show to
convert from `a := b` to `a :== b` and vice versa. This will reveal some
important insights that will be needed to implement `castWith`.

Both `(:=)` and `(:==)` are forms of equality, and as you might expect, you can
convert from one representation to the other. Converting from homogeneous
to heterogeneous equality proves very straightforward:

{% highlight haskell %}
fromHomogeneous :: forall a b. (a := b) -> (a :== b)
fromHomogeneous aEqualsB = subst aEqualsB @((:==) a) refl
{% endhighlight %}

What about the opposite direction? If we try to convert from heterogeneous
to homogeneous equality, we might first try this:

{% highlight haskell %}
toHomogeneous :: forall a b. (a :== b) -> (a := b)
toHomogeneous aEqualsB = hsubst aEqualsB @((:=) a) reflHomogeneous
{% endhighlight %}

Here, `reflHomogeneous` is the same function as
[`refl` from part 1](../../../../2021/08/22/leibniz-equality-in-haskell-part-1#reflexivity), but I'm giving it a different name here to avoid
confusion with the
[heterogeneous version of `refl`](#reflexivity).

When implementing `toHomogeneous`, we pick `((:=) a` as the context to substitute into. This doesn't
quite work, however:

{% highlight plaintext %}
error:
    • Expected kind ‘forall i. i -> Type’,
        but ‘(:=) a’ has kind ‘k -> Type’
    • In the type ‘((:=) a)’
      In the expression: hsubst aEqualsB @((:=) a) reflHomogeneous
{% endhighlight %}

Our plans have once again been foiled by the need for a higher-rank kind.
Recall that `(:=) :: forall k. k -> k -> Type`, and since `((:=) a)` is
shorthand for `((:=) @k a)`, the whole thing has kind `k -> Type`, _not_
`forall k. k -> Type`. What's more, our usual tricks for massaging the order
of `forall`s in kinds won't work here. The defining characteristic of
homogeneous equality is that the kinds of the arguments must be the same.
If we tried using something of kind `forall j. j -> forall k. k -> Type`
instead, it wouldn't be homogeneous anymore.

There's a tension here. The `c` in the type of `hsubst` must
have a higher-rank kind. As a consequence, when picking a type to instantiate `c` with,
the last type argument must have a distinct kind from
other type arguments. This clashes with the need to use `((:=) a)` as a type
context, since the last type argument to `(:=)` must have the same kind as `a`.
How can we resolve this conflict?

As it turns out, we've already seen an example of this sort of conflict in part
1, although it looked somewhat different there. When
[implementing the `injLeibniz` function](../../../../2021/08/22/leibniz-equality-in-haskell-part-1#generativity-and-injectivity)
in part 1, we needed to use an argument of type `f a := f b` to substitute into
something of type `a := b`. At first glance, this seemed impossible, but it
turns out that type families grant just enough extra power to make this
possible. We can define a newtype where the last type argument is `f a`, but
after unwrapping the newtype, the `f a` turns into an `a` by way of type family
reduction.

What does this have to do with implementing `toHomogeneous`? Both `injLeibniz`
and `toHomogeneous` are examples of "shape mismatches". In `injLeibniz`, there
was a mismatch between `f a` and `a`. In `toHomogeneous`, the
shapes being mismatched are at the kind level. `hsubst` expects a kind with a
higher-rank shape, but the shape of `(:=)`'s kind doesn't match.

Just as with `injLeibniz`, the solution to the shape mismatch in `toHomogeneous`
is to use an auxiliary type family. Let's figure out what the outline of this
type family will be by sketching out the newtype it will be used in. Here is a
first attempt:

{% highlight haskell %}
type AuxNewtype :: forall j. j -> forall k. k -> Type
newtype AuxNewtype (a :: j) (b :: k) =
  AuxNewtype { unAuxNewtype :: a := b }
{% endhighlight %}

`AuxNewtype` has the right kind to be used in `hsubst`, but the field of type
`a := b` doesn't kind-check:

{% highlight plaintext %}
error:
    • Expected kind ‘j’, but ‘b’ has kind ‘k’
    • In the second argument of ‘(:=)’, namely ‘b’
      In the type ‘a := b’
      In the definition of data constructor ‘AuxNewtype’
{% endhighlight %}

It is true that in general, `a` and `b` have different kinds, so we can't simply
write `a := b`. However, this is a bit of a special circumstance. In the type of
`toHomogeneous`, the kinds of `a` and `b` must be the same. In other words, if
you wrote out the type of `toHomogeneous` with explicit kinds, you'd get:

{% highlight haskell %}
toHomogeneous :: forall j (a :: j) (b :: j). (a :== b) -> (a := b)
{% endhighlight %}

Since we are planning to only use `AuxNewtype` in the implementation of `toHomogeneous`,
the kinds of `a` and `b` will be the same in `AuxNewtype` in practice.
We can take advantage of this information and define a type family
`AtKind`, which is intended to be used in `AuxNewtype` like so:

{% highlight haskell %}
type AuxNewtype :: forall j. j -> forall k. k -> Type
newtype AuxNewtype (a :: j) (b :: k) =
  AuxNewtype { unAuxNewtype :: a := AtKind j b }
{% endhighlight %}

The idea is that `AtKind j b` returns kind `j`, and `AtKind j b` will reduce to `b` only if
`b` actually has kind `j`. The use of `AtKind` avoids the kind
error we saw above, since both arguments to `(:=)` are now of kind `j`. Moreover,
since we are planning to use `AuxNewtype` in a context where `a` and `b` are both
of kind `j`, we can be sure that `AtKind j b` will reduce to `b`.
Therefore, `AuxNewtype a b` will turn into `a := b` once the newtype constructor
is removed.

Now that I've hyped up `AtKind`, let's actually define it:

{% highlight haskell %}
type AtKind :: forall j -> forall k. k -> j
type family AtKind j (b :: k) where
  AtKind j (b :: j) = b
{% endhighlight %}

This is a pretty remarkable type family, so let's look at the definition of `AtKind` closely.
First, let's look at the kind `forall j -> forall k. k -> j`. The `forall j -> ... -> j`
part indicates that whatever we use as the first argument will also be the return kind [[^2]].
To list some examples, `AtKind Type b` will return something of kind `Type`, `AtKind (Type -> Type) b` will
return something of kind `Type -> Type`, and so on.

The other remarkable thing about `AtKind` is the equation `AtKind j (b :: j) = b`.
Because the `forall k. k` in `AtKind`'s kind signature, the kind of `b` is
allowed to be different from `j`. However, `AtKind j b` will only reduce if
`b` actually does have kind `j`, since the equation requires it. As a result,
`AtKind Type Bool` will reduce to `Bool`, but `AtKind Type Maybe` will not
reduce, since `Maybe` does not have kind `Type`. The ability to equate kinds
in type family equations is another thing that was introduced in GHC 8.0.

That was a lot of build-up just for one type family! But it needed build-up for
a reason, since `AtKind` is the linchpin that holds everything together.
Once we have `AtKind` and define `AuxNewtype` on
top of it, the definition of `toHomogeneous` falls out with relative ease:

{% highlight haskell %}
toHomogeneous :: forall j (a :: j) (b :: j). (a :== b) -> (a := b)
toHomogeneous aEqualsB =
  unAuxNewtype (hsubst aEqualsB @(AuxNewtype a)
    (AuxNewtype reflHomogeneous))
{% endhighlight %}

To recap, this works because we use the argument of type `a :== b` to
convert something of type `AuxNewtype a a` to `AuxNewtype a b`, where
`AuxNewtype a :: forall k. k -> Type`. Removing one layer of newtypes,
we see that we are really going from type `a := AtKind j a` to `a := AtKind j b`. Because
`a` and `b` both have kind `j`, the applications of `AtKind j` will reduce in both cases.
That is to say, really have newtypes around `a := a` and `a := b`, and latter
is just the thing we need.

### Aside: the K axiom

The fact that we successfully defined `toHomogeneous` is a minor miracle. In
some other programming languages, it is impossible to impossible to define
`toHomogeneous` at all. In Coq, for instance,
[their version of `toHomogeneous`](https://coq.inria.fr/library/Coq.Logic.JMeq.html#JMeq_eq)
must be assumed as an axiom. This axiom is
[equivalent to several other similar axioms](https://gist.github.com/amintimany/798a096e2f2ff0582b36)
as well, including the famous [K axiom](https://stackoverflow.com/questions/39239363/what-is-axiom-k).
For the sake of brevity, I'll collectively refer to all such axioms under the
umbrella term "K".

One might wonder why GHC programmers can define `toHomogeneous`, which implies
K, but Coq programmers must instead assume K as an axiom. The primary reason is that
Coq is meant to be consistent as a logic—that is to say, one shouldn't be able to
write a Coq program that proves something false. GHC, however, is
[not meant to be consistent as a logic](https://stackoverflow.com/questions/44034591/is-haskells-type-system-isomorphic-to-an-inconsistent-logic-system-if-so-what),
so the fact that one can use the K axiom in GHC isn't breaking new ground [[^3]].

While the K axiom doesn't imply
falsity on its own, it does become inconsistent when combined with the
univalence axiom, a different axiom which is used in
[homotopy type theory](https://en.wikipedia.org/wiki/Homotopy_type_theory).
The full details are beyond the
scope of this post (and probably beyond my ability to explain), so if
you are interested, I invite you to read the paper
[_Pattern Matching Without K_](https://jesper.sikanda.be/files/pattern-matching-without-K.pdf),
which discusses the subtleties at length.

Why am I bringing up the K axiom in this post? There are some interesting parallels to draw between the
functions we defined earlier which require type families and the functions
which require the K axiom in other programming languages. Note that none of `refl`,
`symm`, or `trans` require type families to define, and indeed, one could define
these functions in Coq without ever needing the K axiom. On the other hand,
`toHomogeneous` _does_ require a type family, and coincidentally enough,
one cannot define `toHomogeneous` in Coq without reaching for the K axiom.

I am not a type theorist (far from it, actually), so I'll let more knowledgeable
people make the final call on whether there really is a connection to the K axiom here or not.
I do feel confident in pointing out that there is a pattern, however. Functions
which require using `hsubst` on an equality type where both of the arguments have
the same kind tend to require the use of type families. `toHomogeneous` meets this
requirement, whereas `refl`, `symm`, and `trans` do not. `symm` and `trans`
use `hsubst` on `(:==)` with differently kinded arguments, and `refl` doesn't
use `hsubst` at all.

## Type-safe cast

It's time to return to an old friend, the `castWith`
function. The heterogeneous version of `castWith` proves harder to
tame than the homogeneous version, however. As you've probably guessed by this point,
naïvely porting over the homogeneous version of `castWith` isn't going to work:

{% highlight haskell %}
type Identity :: Type -> Type
newtype Identity a = Identity { runIdentity :: a }

castWith :: (a :== b) -> a -> b
castWith aEqualsB x =
  runIdentity (hsubst aEqualsB @Identity (Identity x))
{% endhighlight %}

The kind of `Identity` is not polymorphic enough for `hsubst`'s needs:

{% highlight plaintext %}
error:
    • Expected kind ‘forall i. i -> Type’,
        but ‘Identity’ has kind ‘Type -> Type’
    • In the type ‘Identity’
      In the first argument of ‘runIdentity’, namely
        ‘(hsubst aEqualsB @Identity (Identity x))’
      In the expression:
        runIdentity (hsubst aEqualsB @Identity (Identity x))
{% endhighlight %}

Simply changing the kind of `Identity` to `forall i. i -> Type` won't work
either, since its field really must be of kind `Type`. If this is starting
to sound familiar, it's because this is basically the same conundrum we
encountered when implementing `toHomogeneous`. Once again, the kinds of both
arguments to `(:==)` must be the same. In particular, they must both be of
kind `Type`:

{% highlight haskell %}
castWith :: forall (a :: Type) (b :: Type). (a :== b) -> a -> b
{% endhighlight %}

Fortunately, our previous solution of using `AtKind` works just as well here.
Using `AtKind`, we can invent a different form of `Identity` with the
appropriate kind:

{% highlight haskell %}
type PolyIdentity :: forall i. i -> Type
newtype PolyIdentity a =
  PolyIdentity { runPolyIdentity :: AtKind Type a }
{% endhighlight %}

Replacing `Identity` with `PolyIdentity` in the implementation of
`castWith` makes everything work:

{% highlight haskell %}
castWith :: forall (a :: Type) (b :: Type). (a :== b) -> a -> b
castWith aEqualsB x =
  runPolyIdentity (hsubst aEqualsB @PolyIdentity (PolyIdentity x))
{% endhighlight %}

Alternatively, if you're lazy like me and don't want to define another
newtype, you can also implement `castWith` in terms of `toHomogeneous`.

{% highlight haskell %}
castWith :: forall (a :: Type) (b :: Type). (a :== b) -> a -> b
castWith aEqualsB x =
  castWithHomogeneous (toHomogeneous aEqualsB) x
{% endhighlight %}

Here, `castWithHomogeneous` is the same function as
[the homogeneous `castWith` from part 1](../../../../2021/08/22/leibniz-equality-in-haskell-part-1#type-safe-cast),
but with a different name to avoid confusion.

# Parting thoughts

I've demonstrated all of the tips and tricks you'll need to effectively define
and use heterogeneous Leibniz equality. As exercises, see if you can define
heterogeneous versions of the following:

* Functions to convert from propositional to Leibniz equality, as well as the
  opposite direction. Hint: You'll likely want to use `Push` for the opposite
  direction.
* Functions which show that Leibniz equality is generative and injective.
  For inspiration, see
  [`injLeibniz` from part 1](../../../../2021/08/22/leibniz-equality-in-haskell-part-1#generativity-and-injectivity).
* An instance of the
  [`TestEquality`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Type-Equality.html#t:TestEquality)
  class for `(:==)`.

A pre-packaged form of `(:==)` and friends is available in the
[`eq`](https://hackage.haskell.org/package/eq)
package on Hackage, under the
[`Data.Eq.Type.Hetero`](https://hackage.haskell.org/package/eq-4.2.1/docs/Data-Eq-Type-Hetero.html)
module. Note that this module contains spoilers for the exercises above!

-----

[^1]: Another common term for heterogeneous equality in other programming
      languages is "John Major equality", such as in Coq's
      [`JMeq`](https://coq.inria.fr/library/Coq.Logic.JMeq.html) type.
      The term "John Major equality" is a joke about UK politics, which is
      explained in section 5.1.3 of
      [Conor McBride's thesis](http://strictlypositive.org/thesis.pdf).

[^2]: For more on what the `forall (l :: [a]) -> ...` quantifier means, refer
      to my
      [blog post on visible dependent quantification](../../../../2019/03/15/visible-dependent-quantification-in-haskell).

[^3]: After all, there are plenty of ways to define things of type `Void` in
      Haskell without needing to reach for fancy things like the K axiom.
