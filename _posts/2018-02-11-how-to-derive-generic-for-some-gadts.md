---
layout: post
title: How to derive Generic for (some) GADTs using QuantifiedConstraints
---

The `Generic` and `Generic1` classes are extremely useful tools in a GHC
programmer's toolbox, but their utility is currently limited in the sense that
one can only derive `Generic` instances for simple data types. By "simple", I
mean that GHC will simply error if you try to derive `Generic` a more
sophisticated data type—a GADT—like this one:

{% highlight haskell %}
data MyGADT a where
  MyGADT :: Int -> T Int
{% endhighlight %}

In this post, we will ask the question: is this restriction necessary? That is,
could we tweak `Generic` somehow such that we could permit a derived `Generic`
instance for `MyGADT`?

It turns out that while we won't be able to derive `Generic` for _all_ GADTs,
we can in fact derive them for a _subset_ of them. The trick that makes this
possible is `QuantifiedConstraints`, an experimental GHC language feature that
hasn't been merged yet. (But hopefully will soon!)

Throughout this post, I'm going to be assuming basic familiarity with the
`GHC.Generics` API. If you want to follow along with the code in this post
at home, you can build a branch of GHC that implements `QuantifiedConstraints`
located [here](http://git.haskell.org/ghc.git/shortlog/refs/heads/wip/T2893).

## A preamble

Before we proceed any further, here's a list of all language extensions and
imports we'll need at some point:

{% highlight haskell %}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Data.Kind
import GHC.Generics
{% endhighlight %}

## Why deriving `Generic` for GADTs is hard

Let's revisit our `MyGADT` example:

{% highlight haskell %}
data MyGADT a where
  MyGADT :: Int -> T Int
{% endhighlight %}

Can we come up with a suitable `Generic` instance for this? A naïve first
attempt would be something like this:

{% highlight haskell %}
instance Generic (MyGADT a) where
  type Rep (MyGADT a) = Rec0 Int

  from (MyGADT x) = K1 x
  to (K1 x) = MyGADT x
{% endhighlight %}

(Note that for the sake of brevity, I've intentionally left out representation
types like `D1`, `C1`, and `S1` from the `Rep` instance, since they contribute
nothing except metadata.)

Alas, this will not typecheck. GHC will complain thusly:

     • Couldn't match type ‘a’ with ‘Int’
       Expected type: MyGADT a
         Actual type: MyGADT Int
     • In the expression: MyGADT x
       In an equation for ‘to’: to (K1 x) = MyGADT x
       In the instance declaration for ‘Generic (MyGADT a)’
    |
    |   to (K1 x) = MyGADT x
    |               ^^^^^^^^

The problem is that the `Rep` type we've written is actually for a
slightly different datatype:

{% highlight haskell %}
data MyGADT a where
  MyGADT :: Int -> T a
{% endhighlight %}

Where the return type `T a` does not constrain `a` to be equal to `Int`.
Therefore, we haven't encoded enough information into the `Rep` type in order
for `from` and `to` to be able to roundtrip through it.

## What's missing?

The error message makes it pretty clear what we need to add to `Rep` in order
for this to go through: some sort of proof that `a` is equal to `Int`. To use
the language of GHC, we need an `(a ~ Int)` constraint. Moreover, we'd like to
do so in a way that is reusable, so that we can encode _other_ constraints,
such as `Show a`, if we wish.

To this end, I propose creating a new generic representation type, which I
will call `ECC` (short for Existential Constructor Context):

{% highlight haskell %}
data ECC :: Constraint -> (Type -> Type) -> Type -> Type where
  ECC :: c => { unECC :: f x } -> ECC c f x
{% endhighlight %}

Let's take a closer look at what this data type represents. The type of the
field itself, `f x`, is uninteresting—we can instantiate that with whatever
representation type we want, such as `Rec0 Int x`. The novelty of `ECC` is the
`c` parameter—it allows to abstract over whatever context we want, using the
power of the `ConstraintKinds` language extension. (For instance, `c` could be
`(a ~ Int)`, `Show a`, or whatever other constraint we might have.)

With `ECC`, we can define a `Generic` instance for `MyGADT` that actually
typechecks:

{% highlight haskell %}
instance Generic (MyGADT a) where
  type Rep (MyGADT a) = ECC (a ~ Int) (Rec0 Int)

  from (MyGADT x) = ECC (K1 x)
  to (ECC (K1 x)) = MyGADT x
{% endhighlight %}

## `ECC` in action

Defining a `Generic` instance is only half of the battle—now we have to
figure out how to actually _use_ it. As a motivating example, let's consider
the familiar `Eq` class. GHC is smart enough to figure out how to derive `Eq`
for GADTs:

{% highlight haskell %}
deriving instance Eq (MyGADT a)
{% endhighlight %}

So can we accomplish the same thing with `Generic`? Let's find out.

First, we need a generic implementation of the `(==)` method:

{% highlight haskell %}
genericEq :: forall a. (Generic a, Eq (Rep a ())) => a -> a -> Bool
genericEq x y = from' x == from' y
  where
    from' :: a -> Rep a ()
    from' = from
{% endhighlight %}

Now let's try to use it:

{% highlight haskell %}
instance Eq (MyGADT a) where
  (==) = genericEq
{% endhighlight %}

It turns out this won't work quite yet:

     • No instance for (Eq (ECC (a ~ Int) (Rec0 Int) ()))
         arising from a use of ‘genericEq’
     • In the expression: genericEq
       In an equation for ‘==’: (==) = genericEq
       In the instance declaration for ‘Eq (MyGADT a)’
    |
    |   (==) = genericEq
    |          ^^^^^^^^^

Ah, right. We haven't given an `Eq` instance for `ECC` yet! Let's try to do so
now:

{% highlight haskell %}
instance _ => Eq (ECC c f x) where
  ECC x == ECC y = (x == y)
{% endhighlight %}

Here's an intriguing question: what context should go in place of the `_`?
There are several possible candidates, but for now, let's go with the most
obvious one:

{% highlight haskell %}
instance Eq (f x) => Eq (ECC c f x) where
  ECC x == ECC y = (x == y)
{% endhighlight %}

That is sufficient to make the `Eq` instances for `ECC` and `MyGADT` typecheck!
Now we can congratulate ourselves on a productive day of work.

## ...or not

It turns out that this approach doesn't scale very well. Let's imagine that we
tweak `MyGADT` slightly to include another constructor:

{% highlight haskell %}
data MyGADT a where
  MyGADT1 ::         Int -> MyGADT Int
  MyGADT2 :: Eq a => a   -> MyGADT a
{% endhighlight %}

This time, we have a constructor `MyGADT2` that has an existential `Eq a`
context. We need to update the `Generic` instance accordingly:

{% highlight haskell %}
instance Generic (MyGADT a) where
  type Rep (MyGADT a)
    =     ECC (a ~ Int) (Rec0 Int)
      :+: ECC (Eq a)    (Rec0 a)

  from (MyGADT1 x) = L1 (ECC (K1 x))
  from (MyGADT2 x) = R1 (ECC (K1 x))
  to (L1 (ECC (K1 x))) = MyGADT1 x
  to (R1 (ECC (K1 x))) = MyGADT2 x
{% endhighlight %}

Having done this, we now discover that our `Eq` instance for `MyGADT` no
longer typechecks!

     • No instance for (Eq a) arising from a use of ‘genericEq’
     • In the expression: genericEq
       In an equation for ‘==’: (==) = genericEq
       In the instance declaration for ‘Eq (MyGADT a)’
    |
    |   (==) = genericEq
    |          ^^^^^^^^^

It turns out the culprit was our `Eq` instance for `ECC`:

{% highlight haskell %}
instance Eq (f x) => Eq (ECC c f x) where
  ECC x == ECC y = (x == y)
{% endhighlight %}

We got lucky earlier when the only field we had was `Rec0 Int x`, since that is
an instance of `Eq` on its own. But when the field is `Rec0 a x`, we're in
trouble, since that is only an instance of `Eq` when there is an `Eq a`
constraint available.

As you might have guessed, the issue is that we're not using the `c` part of
`ECC c f x`. Happily, it doesn't seem like this would be too hard to fix:

{% highlight haskell %}
instance (c, Eq (f x)) => Eq (ECC c f x) where
  ECC x == ECC y = (x == y)
{% endhighlight %}

...or not. Again, our `Eq` instance for `MyGADT` fails to typecheck:

     • Couldn't match type ‘a’ with ‘Int’
         arising from a use of ‘genericEq’
     • In the expression: genericEq
       In an equation for ‘==’: (==) = genericEq
       In the instance declaration for ‘Eq (MyGADT a)’
    |
    |   (==) = genericEq
    |          ^^^^^^^^^

Argh, what is going on _now_? For some reason, GHC thinks that the `a` in
`MyGADT a` must always be equal to `Int`, even though that should only be
required in the `MyGADT1` constructor! Again, it turns out that our hasty
`Eq` instance for `ECC` is to blame. If we stare closer at the instance
context we gave it:

{% highlight haskell %}
instance (c, Eq (f x)) => Eq (ECC c f x) where ...
{% endhighlight %}

This requires `c` to hold _everywhere_. Since we use `ECC` twice in
`Rep (MyGADT a)`, this means that a generic `Eq` instance for `MyGADT` will
end up with both `a ~ Int` _and_ `Eq a` as constraints. This is clearly not
what we want—we only ever want these constraints to be used on a
constructor-by-constructor basis.

## Enter `QuantifiedConstraints`

Wouldn't it be great if there were a way to tell GHC "I want `Eq (f x)` to
hold, but only under the assumption that `c` already holds"? In other words,
if we can assume `c` _locally_ (without propagating it to the top like our
previous attempt), then that should imply `Eq (f x)`. Fortunately, this is
exactly what the `QuantifiedConstraints` language extension will give us.
With it, we can write the correct `Eq` instance for `ECC`:

{% highlight haskell %}
instance (c => Eq (f x)) => Eq (ECC c f x) where
  ECC x == ECC y = (x == y)
{% endhighlight %}

Notice the use of the quantified constraint `c => Eq (f x)`. The fact that
the `=>` syntax is reused here is intentional, as one can think of
`c => Eq (f x)` as a local instance declaration that need not hold everywhere
in the program. Indeed, `c => Eq (f x)` makes no sense on its own, but in the
context of typechecking a generic `Eq` instance for `MyGADT`, we will at
different points instantiate `c` and `f x` to obtain:

* `(a ~ Int) => Eq (Rec0 Int x)`
* `(Eq a)    => Eq (Rec0 a   x)`

Both which are in fact valid instantiations of the `Eq` instance for `Rec0`.

With the magic of `QuantifiedConstraints`, we now discover that our `Eq`
instance for `MyGADT`:

{% highlight haskell %}
instance Eq (MyGADT a) where
  (==) = genericEq
{% endhighlight %}

Now typechecks without issue! We no longer need to write out any constraints
in this instance, since
`QuantifiedConstraints` ensures that the `c`s in `ECC c f x` will only ever
be used locally.

## The limitations of this approach

### Those _other_ GADTs

Earlier in this post, I mentioned that we could only derive `Generic` for a
subset of GADTs. There is another thing that GADTs can do that is still beyond
reach: existential _quantification_. In all of our previous examples of GADTs,
our only use of the `ExistentialQuantification` extension was for existential
_contexts_ that only ever mentioned type variables that were bound by the
data type itself (i.e., they were _universally_ quantified). But GADTs can also
use existentially quantified type variables that are not bound by the data type
itself, as in the following example:

{% highlight haskell %}
data Ex where
  MkEx :: forall a. a -> Ex
{% endhighlight %}

Here, `a` is only scoped over the `MkEx` constructor, which means that if you
tried to create a `Rep Ex` instance:

{% highlight haskell %}
instance Generic Ex where
  type Rep Ex = ...
{% endhighlight %}

There is no way to refer to `a`! This is a serious problem, as it means that we
effectively cannot come up with a generic representation for `MkEx` due to our
inability to refer to the type of its field.

(This problem has been tackled before in a more limited context in the paper
_[Generic Programming for Indexed Datatypes](http://dreixel.net/research/pdf/gpid.pdf)_,
where the authors come up with a workaround for certain classes of GADTs that
existentially quantify type variables. However, this approach is far from
perfect—for instance, it does not work for the `Ex` example above.)

I won't offer a solution to this particular problem in this post. But even if
we exclude GADTs with existentially quantified type variables, this approach
allows us to generically program in far more situations than we could before.
For instance, it would allow us to eliminate an enormous amount of boilerplate
for [`Binary` instances](http://git.haskell.org/ghc.git/blob/f489c12c9fe4e24dce55269e6998323fd1d9b2a4:/libraries/ghci/GHCi/Message.hs#l416)
in the `ghci` library.

### The `Generic1` wrinkle

How does `ECC` interact with `Generic1`, where we are forced to be parametric
in the last type variable of a data type? For instance, GHC can derive
`Foldable` for `MyGADT`:

{% highlight haskell %}
deriving instance Foldable MyGADT
{% endhighlight %}

Can we accomplish the same thing with `Generic1`? Sadly, I haven't found a way
to make this work. My inclination was to create a version of `ECC` where the
context takes the last type variable as an argument:

{% highlight haskell %}
data ECC1 :: (Type -> Constraint) -> (Type -> Type) -> Type -> Type where
  ECC1 :: c a => { unECC :: f a } -> ECC1 c f a
{% endhighlight %}

This is simple enough, but difficulties arise if you attempt to give a
`Foldable` instance for `ECC1`:

{% highlight haskell %}
instance (_ => Foldable f) => Foldable (ECC1 c f) where
  foldMap f (ECC1 x) = foldMap f x
{% endhighlight %}

What should go in place of `_`? We can't use `c a`, because we don't have a
type variable `a` in scope. Neither does `QuantifiedConstraints` save us,
since if you try to do something like this:

{% highlight haskell %}
instance (forall a. c a => Foldable f) => Foldable (ECC1 c f) where
  foldMap f (ECC1 x) = foldMap f x
{% endhighlight %}

Then GHC will complain that `a` is ambiguous. So you can use `ECC` in your
`Generic1` instances, but under the restriction that `c` cannot mention the
last type variable at all.

## Other applications of this trick

Existential constructor contexts aren't the only place we can use
`QuantifiedConstraints` to increase the surface area of `GHC.Generics`.
There's another restriction that `DeriveGeneric` imposes where no field
types can mention rank-n types, such as in this example:

{% highlight haskell %}
newtype RankNExample f a = RankNExample (Functor f => f a)
{% endhighlight %}

But I laugh in the face of your restrictions, GHC. Akin to the `ECC`
(Existential Constructor Context) type above, we can create something
analogous for rank-n contexts called `RFC` (Rank-n Constructor Context):

{% highlight haskell %}
newtype RFC :: Constraint -> (Type -> Type) -> Type -> Type where
  RFC :: { unRFC :: c => f a } -> RFC c f a
{% endhighlight %}

Notice that this time, the `c` has been moved inside the type of the field,
which allows `RFC` to be a newtype (as opposed to `ECC`, which must be a
proper data type due to its existential context). Now we can give a
`Generic1` instance for `RankNExample` like so:

{% highlight haskell %}
instance Generic1 (RankNExample f) where
  type Rep1 (RankNExample f) = RFC (Functor f) (Rec1 f)

  from1 (RankNExample x) = RFC (Rec1 x)
  to1 (RFC x) = RankNExample (unRec1 x)
{% endhighlight %}

And with this, we can generically derive a `Functor RankNExample` instance:

{% highlight haskell %}
instance (c => Functor f) => Functor (RFC c f) where
  fmap f (RFC x) = RFC (fmap f x)

genericFmap :: (Generic1 f, Functor (Rep1 f)) => (a -> b) -> f a -> f b
genericFmap f = to1 . fmap f . from1

instance Functor (RankNExample f) where
  fmap = genericFmap
{% endhighlight %}

Not bad!

## Final thoughts

In this post, we've seen a potential extension of `GHC.Generics` that would
allow us to derive `Generic(1)` for certain classes of GADTs. The key to this
trick is `QuantifiedConstraints`, without which none of this would be possible.

`QuantifiedConstraints` is still going through the
[GHC proposal process](https://github.com/ghc-proposals/ghc-proposals/pull/109),
but once it lands, the tricks in this blog post will become available to the
masses. In fact, if this idea is popular enough, `ECC`/`RFC` might even deserve
to be a GHC proposal of their own at some point... but this time, `RFC` could
stand for Request for Comment. _ba-dum-tsh_

-----

If you'd like a standalone file to glance at the code used in this blog post,
I've prepared a gist
[here](https://gist.github.com/RyanGlScott/71d9f933e823b4a03f99de54d4b94d51).
