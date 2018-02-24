---
layout: post
title: How QuantifiedConstraints can let us put join back in Monad
---

_This is the second part in my series of practical applications of the
`QuantifiedConstraints` extension. See
[here](2018/02/11/how-to-derive-generic-for-some-gadts)
for part 1._

# `join`

The humble
[`join`](https://hackage.haskell.org/package/base-4.10.1.0/docs/Control-Monad.html#v:join)
function:

{% highlight haskell %}
join :: Monad m => m (m a) -> m a
join x = x >>= id
{% endhighlight %}

`join` has been a part of the `base` library for a long time, and for good
reason. Not only is it useful in many monadic contexts, it's also quite general.
For instance, one can re-implement `(>>=)` itself using `join`:

{% highlight haskell %}
(>>=) :: Monad m => m a -> (a -> m b) -> m b
x >>= f = join (fmap f x)
{% endhighlight %}

In fact, `(>>=)` and `join` are equivalent in expressive power. Anything one
can do with `(>>=)`, one could just as well do with `join`, and vice versa.
Many folks took note of this and proposed that `join` be added as a class
method to `Monad` itself so that `(>>=)` and `join` could have default
implementations in terms of the other. In fact, the original
[`Applicative`–`Monad` Proposal](https://wiki.haskell.org/index.php?title=Functor-Applicative-Monad_Proposal&oldid=60290)
(or AMP for short) proposed doing just that, in addition to making
`Applicative` a superclass of `Monad`.

However, the `join` portion of the AMP was eventually dropped, not because of
community outcry, but because of unfortunate technical restrictions. In this blog
post, we will explore what these technical restrictions are in further detail, and
how the proposed
[`QuantifiedConstraints`](https://github.com/ghc-proposals/ghc-proposals/pull/109)
extension can lift these restrictions.

> Note that while there is a
> [prototype implementation](http://git.haskell.org/ghc.git/shortlog/refs/heads/wip/T2893)
> of `QuantifiedConstraints` available, much of the code in this blog post will not
> work with it. That's because the code relies on an unimplemented (as of the
> time of writing) featurette, described in
> [this section](https://github.com/Gertjan423/ghc-proposals/blob/e16828dbcd59d0ca58573c81fc6cea671875e6e2/proposals/0000-quantified-constraints.rst#id8)
> of the `QuantifiedConstraints` proposal.

# The day the `join` died

To put the following section into the right historical context: the year is 2014.
GHC 7.8 has just been released, boasted a variety of improvements such as typed
holes, closed type families, pattern synonyms, and an overhauled version of
`GeneralizedNewtypeDeriving` that fixed a latent type safety bug (more on this
point later). Team GHC is hard at work preparing a patch which will implement
the AMP for the upcoming 7.10 release.

One day, and unexpected
[hurdle](https://mail.haskell.org/pipermail/ghc-devs/2014-May/004964.html)
arises. After making `join` a class method of `Monad`, GHC starts complaining
when compiling the `haskeline` library. The issue can be boiled down to the
following code:

{% highlight haskell %}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Let's use a stripped-down Monad with just join
class Monad' m where
  join :: m (m a) -> m a

newtype T m a = MkT (m a)
  deriving Monad'
{% endhighlight %}

Here is the perplexing error that GHC gives (I'll use a contemporary version,
GHC 8.2.2, for a slightly more comprehensible error message):

      • Couldn't match representation of type ‘m (T m a)’
                                 with that of ‘m (m a)’
          arising from the coercion of the method ‘join’
            from type ‘forall a. m (m a) -> m a’
              to type ‘forall a. T m (T m a) -> T m a’
        NB: We cannot know what roles the parameters to ‘m’ have;
          we must assume that the role is nominal
      • When deriving the instance for (Monad' (T m))
    |
    |   deriving Monad'
    |            ^^^^^^

As mentioned earlier, GHC 7.8 introduced a new version of
`GeneralizedNewtypeDeriving`, and since the code snippet uses it prominently,
it became evident that `GeneralizedNewtypeDeriving` was the culprit. However,
this was no common bug. This was a restriction that arose due to the design of
the new `GeneralizedNewtypeDeriving` itself, and no one could come up with a
satisfactory workaround.

Disheartened and defeated, Team GHC eventually decided to abandon the `join`
portion of the AMP, as using `GeneralizedNewtypeDeriving` was simply too useful
to give up. Needless to say, though, this left a sour taste in the mouths of many
people—Haskellers don't take kindly to being told that something can't be
done in Haskell!

In order to better understand why adding `join` to `Monad` interacted so poorly
with `GeneralizedNewtypeDeriving`, we must first take a detour into _roles_, the
mechanism which underlies GHC's notion of type-safe coercions. Understanding
roles is a key step into seeing why `Monad`-plus-`join` is so tricky to derive.

The coming sections present an overview of how `GeneralizedNewtypeDeriving`,
`Coercible`, and roles work. If you are already comfortable with these, feel
free to skip to [this section](#when-roles-go-rogue).

## `GeneralizedNewtypeDeriving` gone wrong

What exactly was wrong with `GeneralizedNewtypeDeriving` before GHC 7.8?
First, let's consider a typical example:

{% highlight haskell %}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Age = MkAge Int
  deriving Num
{% endhighlight %}

Conceptually, what `GeneralizedNewtypeDeriving` does here is to generate an
instance of `Num` for `Age` that does the appropriate wrapping and unwrapping
of the `MkAge` constructor:

{% highlight haskell %}
instance Num Age where
  MkAge x + MkAge y = MkAge (x + y)
  fromInteger i = MkAge (fromInteger i)
  -- etc.
{% endhighlight %}

Figuring out where all of these constructor wrapping should go can be tricky,
however, so to make things easier, GHC takes a shortcut. Before 7.8, this
would be the code that would actually get generated:

{% highlight haskell %}
instance Num Age where
  (+)         = unsafeCoerce ((+) :: Int -> Int -> Int)
  fromInteger = unsafeCoerce (fromInteger :: Integer -> Int)
  -- etc.
{% endhighlight %}

This use of `unsafeCoerce` may seem fishy, but it's actually OK here. That's
because at runtime, an `Age` and an `Int` have the exact same representation,
so it's perfectly safe to coerce from an `Int` to an `Age`, or vice versa.
Moreover, any function that works over `Int`s can also be coerced to a function
over `Age`s, which explains why it's safe to coerce from, say,
`fromInteger :: Integer -> Int` to `fromInteger :: Integer -> Age`.

It turns out, however, that always using `unsafeCoerce` in
`GeneralizedNewtypeDeriving` compromised type safety. (You might be thinking,
"Well duh!", but this was not obvious at the time!) To see what can go wrong
with this approach, consider this extension to the example above, which
leverages type families:

{% highlight haskell %}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

type family Inspect x
type instance Inspect Age = Int
type instance Inspect Int = Bool

class BadIdea a where
  bad :: a -> Inspect a

instance BadIdea Int where
  bad x = x > 0

newtype Age = MkAge Int
  deriving BadIdea
{% endhighlight %}

Now consider what happens when you invoke `bad (MkAge 42)`. Since
`Inspect Age = Int`, we'd expect this to return an `Int`. But since we
newtype-derived the `BadIdea` instance for `Age`, we are going to coerce from
the `bad` implemention for `Int`. But note that in the `bad` implementation for
`Int`, we return a `Bool`! Thus, `bad (MkAge 42)` returns `42 > 0` (a `Bool`)
unsafely coerced to an `Int`, which are two types with _very_ different runtime
representations. Undefined behavior (or segfaults) are just around the corner—
in other words, we have violated type safety.

## Time to role up our sleeves

Clearly, `GeneralizedNewtypeDeriving` was subtly broken, but where precisely did
things go kaput? Coercing from `Int` to `Age` was just fine, but coercing
from `Inspect Age` to `Inspect Int` caused pandemonium. This observation that
coercing between types could be safe in some contexts but unsafe in other
contexts in the key idea behind _roles_.

### An abundance of equality

To explain what a role is, first consider the equality type `(~)`. Before
roles, `a ~ b` would only hold if `a` and `b` were
the same type. But this is somewhat unsatisfying, since types like `Int` and
`Age` are "equal" (in the sense that they have the same representation at
runtime), but you could not conclude that `Int ~ Age`.

GHC 7.8 addressed this by parameterizing its notion of equality with a role.
There are three different roles:

* **Nominal**: Two types are nominally equal when they are the exact same type.
  (This is equivalent to the notion of equality GHC used before the introduction
  of roles.)

  For instance, `Age` is nominally equal to `Age`, and `Int` is nominally equal
  to `Int`. However, `Age` is not nominally equal to `Int`, or `Bool`, or any
  other type.

* **Representational**: Two types are representationally equal when they have
  the same representation at runtime. (This is a strict superset of nominal
  equality.)

  Any newtype is representationally equal to its underlying type, so we have
  that `Age` is representationally equal to `Int`. However, `Age` would not
  be representationally equal to `Bool`.

* **Phantom**: This is the broadest notion of equality, as _any_ two types
  are phantom-equal.

In GHC 7.8, `(~)` became synonymous with nominal equality. GHC 7.8 also added
a type for representational equality called `Coercible`. Moreover, a type-safe
version of `unsafeCoerce` was added, simply called `coerce`:

{% highlight haskell %}
coerce :: Coercible a b => a -> b
{% endhighlight %}

`coerce` will be key in fixing `GeneralizedNewtypeDeriving`. If GHC cannot
conclude `Coericble a b`, then we cannot be sure that it is safe to coerce
from a value of type `a` to type `b`, so it will be rejected with a type error.

### What can be `Coercible`

The earlier definition I gave for representational equality (having the same
runtime representation) is a mite hand-wavey. For instance, we can intuitively
say that `Inspect Int` and `Inspect Age` do not have the same runtime representation,
since they reduce to `Bool` and `Int`, respectively, but how would we be able
to know that it's not safe in general to `coerce` between `Inspect a` and
`Inspect b` for distinct types `a` and `b`?

To give a clearer picture of what can and can't be `coerce`d, I'll give some
examples in terms of `Coercible`. We already know that newtypes and their
underlying types are inter-`Coercible`:

{% highlight haskell %}
instance Coercible Age Int
instance Coercible Int Age
{% endhighlight %}

> Note: I'm writing these `Coercible` examples as instances, since that's how
> Haskellers are used to thinking about constraints. Do note, however, that
> `Coercible` is _not_ actually a type class, and you can't actually define
> instances for it yourself. Rather, these example instances are intended to
> demonstrate facts about `Coercible` that GHC is able to use in its
> constraint solver.

But `Coercible` can do more that this: it can also look through other type
constructors! For instance, it can look through the function type:

{% highlight haskell %}
instance Coercible (Integer -> Age) (Integer -> Int)
instance Coercible (Integer -> Int) (Integer -> Age)
{% endhighlight %}

Critically, however, it cannot look through type family constructors,
so we would _not_ be able to conclude that:

{% highlight haskell %}
instance Coercible (Inspect Age) (Inspect Int) -- WRONG
instance Coercible (Inspect Int) (Inspect Age) -- WRONG
{% endhighlight %}

What separates the honest type constructor `(->)` from the miscreant
type family `Inspect`? It all comes down to how each type uses its arguments.
In GHC, if you have a type `T a_1 ... a_n`, then each parameter `a_i` is given a
role, which indicates the broadest notion of equality that is permitted to be used
when determining if `T a_1 ... a_n` is representationally equal to
`T b_1 ... b_n`.

As a concrete example, both of the parameters to `(->)` have representational
roles. In terms of the `RoleAnnotations` language extension, we can write this
as:

{% highlight haskell %}
type role (->) representational representational
{% endhighlight %}

In other words, `a1 -> a2` is representationally equal to `b1 -> b2` if
`a1` is representationally equal to `a2`, and `b1` is representationally equal
to `b2` (i.e., if `(Coercible a1 a2, Coercible b1 b2)` holds). Having
representationally roled parameters is a trait that many data types also share:

{% highlight haskell %}
type role Maybe representational
type role [] representational
type role Either representational representational
{% endhighlight %}

This makes sense, since all of the parameters to these data types have fields
of those type, so they matter at runtime. If we had a data type that had a
parameter that _didn't_ appear as the type of a field, such as in `Proxy`:

{% highlight haskell %}
data Proxy a = Proxy
{% endhighlight %}

Then that parameter would be phantom-roled:

{% highlight haskell %}
type role Proxy phantom
{% endhighlight %}

In other words, `Proxy a` is representationally equal to `Proxy b` for _any_ types
`a` and `b` (i.e., `Coercible (Proxy a) (Proxy b)` always holds). Again, this makes
intuitive sense, the parameter to `Proxy` has no effect whatsoever on its
runtime representation.

Now you might be wondering: can we ever get a parameter to be nominally roled?
Indeed we can: type families are a prime example of this, as every parameter
to a type family must be assigned a nominal role. For instance:

{% highlight haskell %}
type role Inspect nominal
{% endhighlight %}

In other words, `Inspect a` is representationally equal to `Inspect b` if
`a` is _nominally_ equal to `b` (i.e., if `a ~ b`). This is exactly the
behavior we desire, because it's only safe to coerce `Inspect a` if we
can be sure that the coercion does not change the type `a` whatsoever.

> There are other types in Haskell that demand nominal roles for its parameters,
> such as GADTs and type classes. The curious reader is encouraged to read the paper
> [_Safe Zero-cost Coercions for Haskell_](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/coercible.pdf)
> for further commentary on these design choices.

### Role with the changes

Tying everything we've just learned together, we can now tweak
`GeneralizedNewtypeDeriving` to be type-safe. Instead of generated the following
code for a derived `BadIdea Age` instance:

{% highlight haskell %}
instance BadIdea Age where
  bad = unsafeCoerce (bad :: Int -> Inspect Int)
{% endhighlight %}

All we have to do is make one small change: just replace `unsafeCoerce` with `coerce`!

{% highlight haskell %}
instance BadIdea Age where
  bad = coerce (bad :: Int -> Inspect Int)
{% endhighlight %}

That's it! Now, the typechecker will be unable conclude that
`Coercible (Inspect Int) (Inspect Age)` holds, and thus the typechecker will
reject this code.

# When roles go rogue

Now, let's take a closer look at the code that gets generated when we derive
an instance of `Monad`-plus-`join` for the `T` newtype from
[this section](#the-day-the-join-died).
We will get:

{% highlight haskell %}
class Monad' m where
  join :: m (m a) -> m a

newtype T m a = MkT (m a)
  deriving Monad'

instance Monad' m => Monad (T m) where
  join                :: T m (T m a) -> T m a
  join = coerce (join ::   m   (m a) ->   m a)
{% endhighlight %}

In order for this to typecheck, we need to be able to conclude that the
following constraints hold:

* `Coercible (m (m a)) (T m (T m a))`
* `Coercible (m a) (T m a)`

The latter fact follows from the fact that `T m a` is a newtype around `m a`.
The former fact, however, is trickier. We can know unwrap the outer `T` to
get an obligation of `Coercible (m (m a)) (m (T m a))`, but at this point, we
are in trouble. Recall this part of GHC's error message:

      • Couldn't match representation of type ‘m (T m a)’
                                 with that of ‘m (m a)’
        NB: We cannot know what roles the parameters to ‘m’ have;
          we must assume that the role is nominal

This gets at the heart of the matter: `m` is a higher-kinded type variable
that takes an argument. Moreover, `m` is abstract, so we cannot know in
advance if `m` will be instantiated with something like `Maybe` (whose
parameter is representationally roled) or an abstract type like `Set`
(whose parameter is nominally roled). Thus, to preserve type safety, we
_must_ be conservative and assume that the parameter to `m` is nominal.

But this is a serious problem. `m (m a)` is only ever nominally equal to
`m (T m a)` when `m a` is nominally equal to `T m a`, and that can never
happen! GHC therefore rejects this as ill roled.

What can we do about all this? It might appear that we've designed ourself
into a corner with the way representational equality works. Indeed, there were
several proposed extensions to the role system, such as higher-order
roles, which would address this shortcoming. However, each idea had certain
programs for which it would not work, so none of these suggestions were
ever implemented.

## Enter `QuantifiedConstraints`

In [late 2014](https://ghc.haskell.org/trac/ghc/ticket/9123#comment:29), it was
discovered that `QuantifiedConstraints`—a much older idea that had been
developed for different purposes—could be used to roll your own higher-order roles!
Recall that we got stuck when trying to solve `Coercible (m (m a)) (m (T m a))`,
since didn't know enough about `m`. But what if we were allowed to say that
`m a` is representationally equal to `m b` _under the assumption_ that
`a` is representationally equal to `b`? This is precisely the power that
`QuantifiedConstraints` gives you at the language level.

First, let's bring back our earlier attempt at deriving `Monad`-plus-`join`:

{% highlight haskell %}
instance ( Monad' m
         , ...
         ) => Monad (T m) where
  join                :: T m (T m a) -> T m a
  join = coerce (join ::   m   (m a) ->   m a)
{% endhighlight %}

We want to be able to say that for _any_ types `a` and `b`:

{% highlight haskell %}
forall a b. <...>
{% endhighlight %}

If we can conclude that `a` and `b` are representationally equal...

{% highlight haskell %}
forall a b. Coercible a b => <...>
{% endhighlight %}

...then we can conclude that `m a` is representationally equal to `m b`:

{% highlight haskell %}
forall a b. Coercible a b => Coercible (m a) (m b)
{% endhighlight %}

This is exactly the quantified constraint we need:

{% highlight haskell %}
instance ( Monad' m
         , forall a b. Coercible a b => Coercible (m a) (m b)
         ) => Monad (T m) where
  join                :: T m (T m a) -> T m a
  join = coerce (join ::   m   (m a) ->   m a)
{% endhighlight %}

And now, this typechecks! Best of all, we didn't need to change the underlying
role system at all for this to work—instead, we took advantage of GHC's
constraint solver itself, which is quite flexible.

A consequence of this quantified constraint is that instantiating `m` with
any type `N` such that `type role N nominal` will fail. This is a good thing—
these types `N` are precisely what we want to rule out, _and nothing more!_

# `GeneralizedNewtypeDeriving` gone right?

We now have a blueprint for how to spruce up `GeneralizedNewtypeDeriving` to
address its current shortcomings. In particular, we can change it so that
when inferring constraints for derived instance, it infers quantified
`Coercible` constraints whenever the class has a method with a type signature
in which a higher-kinded type variable bound by the instance is applied to
some type.

This would do the job, but it would be a bit of a departure from the
constraints that GHC typically infers. Would users be surprised to see
error messages involving quantified constraints? That remains to be seen,
but we now at least have the _potential_ to fix the problem, which is a huge
leap forward from the earlier status quo.
