---
layout: post
title: Four ways to partially apply constraint tuples
---

Tuples, aside from their weird syntax, are just like any other data type.
You can even partially apply a tuple type constructor by writing it in
"prefix" style. For instance, `(Int, Bool)` and `(,) Int Bool` are two ways
of writing the same type. This flexibility allows tuples to be manipulated
in all the ways we're used to in Haskell.

Unfortunately, _constraint_ tuples do not enjoy the same privileges as their
relatives, the tuple _data types_. In particular, it is not possible to
partially apply a constraint tuple type constructor in the conventional sense,
which can make them somewhat awkward to use at times. In this post, I will
discuss four different techniques for working around this
limitation of constraint tuples.

# A trove of tuples

Tuples in Haskell can have several different meanings depending on the context.
For instance, the tuple in `(a, b)` can be a data type, a data constructor, or
a constraint depending on where it is used. Here is a piece of code that
demonstrates each of these three meanings:

{% highlight haskell %}
foo :: (Int, Bool)                    -- Data type
foo = (42, True)                      -- Data constructor

type ReadAndShow a = (Read a, Show a) -- Constraint
{% endhighlight %}

As far as syntactic puns goes, tuples are one of GHC's longest-running gags.
One drawback of punning tuple syntax is that there can be ambiguous
situations where it is unclear precisely which form of tuple is being used.
As a concrete example, imagine this code:

{% highlight haskell %}
type MyTuple a b = (a, b)
{% endhighlight %}

Is `MyTuple` a type synonym for a tuple data type or a constraint tuple? The answer
depends on whether the kinds of `a` and `b` are `Type` or `Constraint`.
In the absence of additional kind information, GHC will conservatively assume that
`MyTuple` refers to a data type, not a constraint. If this was not the
programmer's intention, they can clarify their intentions by adding extra
kind annotations, like so:

{% highlight haskell %}
type MyTuple (a :: Constraint) (b :: Constraint) = (a, b)

-- Alternatively, one could also write
-- type MyTuple a b = ((a, b) :: Constraint)
{% endhighlight %}

In this sense, GHC has a very limited form of type-directed name resolution.
This trick is usually sufficient to distinguish constraint tuples from
tuple data types in most situations.

## Tuples and partial application

Just as tuples have special syntax for fully saturated applications, tuples
also have special syntax for partial applications. For instance, here is some
code from earlier in this post, but rewritten to use this partial application syntax:

{% highlight haskell %}
foo :: (,) Int Bool
foo = (,) 42 True
{% endhighlight %}

To complete the analogy, we should also define `ReadAndShow` using a partially
applied constraint tuple. Intuitively, you might expect that this would get the
job done:

{% highlight haskell %}
type ReadAndShow a = (,) (Read a) (Show a)
{% endhighlight %}

Surprisingly, this won't work. If you feed this code into GHC, it will regurgitate:

{% highlight plaintext %}
error:
    • Expected a type, but ‘Read a’ has kind ‘Constraint’
    • In the first argument of ‘(,)’, namely ‘(Read a)’
      In the type ‘(,) (Read a) (Show a)’
      In the type declaration for ‘ReadAndShow’

error:
    • Expected a type, but ‘Show a’ has kind ‘Constraint’
    • In the second argument of ‘(,)’, namely ‘(Show a)’
      In the type ‘(,) (Read a) (Show a)’
      In the type declaration for ‘ReadAndShow’
{% endhighlight %}

This error message would suggest that GHC thinks the `(,)` in `(,) (Read a) (Show a)`
refers to the type constructor for a data type, not a constraint. What's more,
even if you add extra kind information:

{% highlight haskell %}
type ReadAndShow (a :: Constraint) = (,) (Read a) (Show a)
type ReadAndShow a = ((,) (Read a) (Show a) :: Constraint)
type ReadAndShow a = ((,) :: Constraint -> Constraint -> Constraint) (Read a) (Show a)
{% endhighlight %}

GHC _still_ refuses to recognize `(,)` as being the name of a constraint tuple.
The unfortunate reality is that GHC's type-directed name resolution trick for
tuples only applies to fully applied tuples. Any use of a partially applied tuple
will be interpreted to mean a tuple data type. In other words, partially applying
a constraint tuple is essentially prohibited. Bummer.

# Regaining partial application

GHC may prohibit us from partially applying constraint tuples in a straightforward
fashion, but that's clearly not the end of the story. Otherwise, I wouldn't be
writing this blog post! We needn't get discouraged by the inability to use `(,)`
in constraints because there are other ways to accomplish the same thing. All we
need is some creativity and a couple of language extensions. (OK, perhaps slightly
more than a couple.)

## Solution 1: Roll your own constraint tuples

If we can't use GHC's built-in constraint tuples, a feasible alternative is to
just bake our own from scratch. But what exactly _are_ constraint tuples,
anyway? Let's try to reverse engineer them.

To pick one particular example, the constraint tuple used in
`(Read a, Show a)` takes two `Constraint`s as inputs (`Read a` and `Show a`)
and produces another `Constraint` as an output. We only have one tool in our
toolbelt for crafting new types that live `Constraint`: type classes.
As luck would have it, type classes provide exactly what we need to assemble
a makeshift constraint tuple. Here is a decent first attempt:

{% highlight haskell %}
class (a, b) => CTuple2 (a :: Constraint) (b :: Constraint)
{% endhighlight %}

`CTuple2` has two `Constraint`-kinded arguments that also function as the
superclasses. This is possible thanks to the power of GHC's
`ConstraintKinds` extension, as well as supporting roles from
`KindSignatures`, `MultiParamTypeClasses`,
and `UndecidableSuperClasses` (which we need in order to define a class with a
superclass that is a bare type variable). Now, we should be able to take
`(Read a, Show a)` and replace it with `CTuple2 (Read a) (Show a)`.
For example, we can do this:

{% highlight haskell %}
roundtrip :: CTuple2 (Read a) (Show a) => a -> a
roundtrip = read . show
{% endhighlight %}

And it compiles! (Well, after enabling `FlexibleContexts`, that is.)
However, our `CTuple2` is still only half-baked. If you try to actually
_use_ `roundtrip` somewhere, you're likely to run into trouble.
Here is one such troublemaker:

{% highlight haskell %}
roundtripInt :: Int
roundtripInt = roundtrip 27
{% endhighlight %}

If you compile this, GHC will whine thusly:

{% highlight plaintext %}
error:
    • No instance for (CTuple2 (Read Int) (Show Int))
        arising from a use of ‘roundtrip’
    • In the expression: roundtrip 27
      In an equation for ‘roundtripInt’: roundtripInt = roundtrip 27
{% endhighlight %}

GHC is pouting because it wants there to be an instance of `CTuple2` in
scope in order to satisfy the `CTuple2` constraint in `roundtrip`.
(Whereas we assumed the existence of such an instance in the definition
of `roundtrip` itself, so we did not need it there.) We could try
to shut GHC up by just defining a one-off instance like so:

{% highlight haskell %}
instance (Read a, Show a) => CTuple2 (Read a) (Show a)
{% endhighlight %}

This works, but it's not a terribly robust solution. If you need to use any
other pair of constraints, such as `CTuple2 (Eq a) (Ord a)`, then you'll also
need to define a corresponding `CTuple2 (Eq a) (Ord a)` instance. Since there
are infinitely many such combinations of constraints, we would have to write
a lot of code to achieve parity with built-in constraint tuples this way.

Thankfully, we can get away with only writing one instance instead of
infinitely many. With the help of `FlexibleInstances`, we can implement the
one instance to rule them all:

{% highlight haskell %}
instance (a, b) => CTuple2 (a :: Constraint) (b :: Constraint)
{% endhighlight %}

This will make `roundtripInt` compile, as well as any other conceivable use
of `CTuple2`. Normally, defining "catch-all" instances like the one above
is considered bad practice, since any other instance that a user might want
to define will overlap with it [[^1]]. In the particular case of `CTuple2`,
however, having a catch-all instance is just fine, as this is really the only
instance of `CTuple2` you'll ever need. This is a rare scenario where we will
choose to embrace catch-all instances rather than shun them.

To recap, we were able to roll our own constraint tuple like so:

{% highlight haskell %}
class    (a, b) => CTuple2 (a :: Constraint) (b :: Constraint)
instance (a, b) => CTuple2 (a :: Constraint) (b :: Constraint)
{% endhighlight %}

The crucial bit here is that since `CTuple2` is a class, it can be partially
applied. On the other hand, if we were to define `CTuple2` as a type synonym:

{% highlight haskell %}
type CTuple2 (a :: Constraint) (b :: Constraint) = (a, b)
{% endhighlight %}

The same property would not hold true, since type synonyms cannot be partially
applied like classes can [[^2]]. For this reason, the class-and-instance
version of `CTuple2` can be used in strictly more places than the type
synonym version can.

This trick—defining a class accompanied by a single catch-all instance—is
called the "constraint synonym" encoding [[^3]]. It is worth noting that I am far
from the first person to make use of this encoding. This
[blog post by Icelandjack](https://gist.github.com/Icelandjack/5afdaa32f41adf3204ef9025d9da2a70#constraint-synonym-encoding-or-class-synonym)
has a much more thorough exposition on constraint synonyms and all of the
interesting things one can do with them.

## Solution 2: Roll your own constraint newtypes

Another common name for constraint synonyms is "constraint newtypes". The
rationale behind this analogy is that newtypes provide a cheap way to define
something that is like another type, but with a new name. For instance,
in the following constraint synonym:

{% highlight haskell %}
class    C => MyC
instance C => MyC
{% endhighlight %}

`MyC` is basically the exact same type as `C`, but with a distinct name.
It's almost as if you wrote `newtype MyC = MyC C`!

In GHC, the analogy goes even deeper than that. When the class declaration for
`MyC` is compiled to Core, it becomes a dictionary data type with a single
field of type `C` to represent its superclass. As an optimization, GHC takes
all dictionary data types with exactly one field and turns them into newtypes.
In other words, `MyC` is quite literally a newtype at the Core level [[^4]].

However, not all constraint synonyms become newtypes in Core. One
counterexample is the `CTuple2` class. Recall its definition:

{% highlight haskell %}
class    (a, b) => CTuple2 a b
instance (a, b) => CTuple2 a b
{% endhighlight %}

`CTuple2` may look like a class newtype on top of a built-in constraint tuple
of size 2, but it's not. The reason is that the `(a, b)` to the left of
the `=>` is not, strictly speaking, a constraint tuple. It's simply syntax
denoting the combination of two superclasses, `a` and `b`.
(Yet another way tuples are punned in Haskell!)
To put in another way, this is the dictionary version of `CTuple2` in Core:

{% highlight haskell %}
data CTuple2 a b = CTuple2 a b
{% endhighlight %}

Rather than a newtype on top of `(a, b)`, `CTuple2` is a data type with two
separate fields of types `a` and `b`.
In practice, this difference probably won't matter, since
`CTuple2 a b` looks and behaves like `(a, b)` would anyway. But it does raise
the question: can we define `CTuple2` another way so that it _is_ a newtype
on top of `(a, b)` in Core?

As it turns out, we can. The idea is that instead of giving `CTuple2`
two superclasses, we only give it one. To accomplish this, we make use of
good-old-fashioned type synonyms. First, we define a type synonym for
`(a, b)`:

{% highlight haskell %}
type MyTuple (a :: Constraint) (b :: Constraint) = (a, b)
{% endhighlight %}

When used on the right-hand side of a type synonym like this, `(a, b)` really
does refer to a built-in constraint tuple type. With `MyTuple` in
hand, defining a "newtype" version of `CTuple2` is as easy as this:

{% highlight haskell %}
class    MyTuple a b => CTuple2 a b
instance MyTuple a b => CTuple2 a b
{% endhighlight %}

Now `CTuple2` will compile to (roughly)
`newtype CTuple2 a b = CTuple2 (MyTuple a b)` in Core.

Alternatively, if you want to minimize the number of types you have to define, you
can "inline" `MyTuple` into the definition of `CTuple2` like so:

{% highlight haskell %}
class    ((a, b) :: Constraint) => CTuple2 a b
instance ((a, b) :: Constraint) => CTuple2 a b
{% endhighlight %}

Yes, you read that right: when used to the left of `=>`, `(a, b)` and
`((a, b) :: Constraint)` compile to different things in Core. Is this confusing?
Probably. Don't blame me, I didn't design the compiler.
(At least, not this part.)

## Solution 3: Decompose constraint tuple applications with type families

The previous two solutions get the job done, but in some sense, they're
cheating a bit. That is because they give you ways to partially apply things
like _look_ like GHC's built-in constraint tuples, but they're still technically
different types altogether. What if we want to use actual, honest-to-goodness
constraint tuples types instead? At this point, we have to get even
more creative.

While we can't partially apply a constraint tuple type constructor directly
in source Haskell (as in `(,) (Read a) (Show a)`), these type constructors
are still very much real in the eyes of GHC. We will crucially rely on this
fact in order to get our hands on them. In particular, GHC lets you _decompose_
applications of type constructors by way of type families. For instance,
if you are given the type `f x`, you can decompose it into `f` like so:

{% highlight haskell %}
type family DecomposeType (a :: Type) :: Type -> Type where
  DecomposeType (f x) = f
{% endhighlight %}

We can test out `DecomposeType` in GHCi by using the `:kind!` command, which
both computes a type's kind and reduces type families that appear in the
type itself as much as possible:

{% highlight plaintext %}
λ> :kind! DecomposeType (Maybe Int)
DecomposeType (Maybe Int) :: Type -> Type
= Maybe
{% endhighlight %}

This is, admittedly, a silly use case for this feature, as we could have
just wrote `Maybe` directly instead of the more convoluted
`DecomposeType (Maybe Int)`. On the other hand, we can define a variant
of `DecomposeType` that is much more useful:

{% highlight haskell %}
type family DecomposeConstraint (a :: Constraint) :: Constraint -> Constraint -> Constraint where
  DecomposeConstraint (f x y) = f
{% endhighlight %}

This time, we're only dealing with `Constraint`-kinded type constructors that
take two arguments. This is because `DecomposeConstraint` is tailor-made to
decompose the constraint tuple in a type like `(Read a, Show a)`. As proof
that this works, behold:

{% highlight plaintext %}
λ> :kind! DecomposeConstraint (Read Int, Show Int)
DecomposeConstraint (Read Int, Show Int) :: Constraint
                                            -> Constraint -> Constraint
= GHC.Classes.(%,%)
{% endhighlight %}

Ta-da! This constraint tuple type constructor looks a little funny since it's
usually not exposed to the programmer like that, but that's exactly what we
were looking for. On the other hand, typing out `DecomposeConstraint` every
time we want to use this type constructor is a little cumbersome, so we
can make our lives a little easier by defining a shorthand for it:

{% highlight haskell %}
type CTuple2 = DecomposeConstraint (Read Int, Show Int)
{% endhighlight %}

This version of `CTuple2` behaves just like its cousins in the previous two
sections, but it evaluates directly to GHC's built-in constraint tuple
constructor instead of making use of an auxiliary type class.

## Solution 4: Decompose constraint tuple applications with type synonyms

There is one drawback to the approach in the
previous section: you cannot define type class instances that mention `CTuple2`
in the instance head. For instance, if you tried this compiling this
example:

{% highlight haskell %}
data Dict (c :: Constraint) = ...

class MyClass a where ...
instance MyClass (Dict (CTuple2 (Read Int) (Show Int))) where ...
{% endhighlight %}

GHC will pump the brakes immediately:

{% highlight plaintext %}
error:
    • Illegal type synonym family application ‘DecomposeConstraint
                                                 (Read Int, Show Int)’ in instance:
        MyClass (Dict (CTuple2 (Read Int) (Show Int)))
    • In the instance declaration for
        ‘MyClass (Dict (CTuple2 (Read Int) (Show Int)))’
{% endhighlight %}

Blast. If we want to work around this limitation of GHC's, we'll need to reach
even deeper into our magic hat. Luckily, we still have one more trick in our
repertoire. The idea is the same: decompose an application of a type constructor
of the right kind. This time, however, we're going to avoid using type families
altogether and rely on type synonyms to do the actual decomposition part.

It might sound a bit strange to use a type synonym to decompose a type constructor
application. Trying to decompose things the "simple" way just won't cut it.
This code:

{% highlight haskell %}
type DecomposeConstraint (f x y) = f
{% endhighlight %}

Won't even get past the parser. Type synonym declarations require all of the
arguments to be bare type variables, so this idea seems doomed to fail. But
we shouldn't give up yet. Even though the arguments of a type synonym
must be bare variables, there are no such restrictions on what their _kinds_
can be. That's right: it's time to get fancy.

Enlisting the help of the `DataKinds` and `PolyKinds` extensions, we can
define a type synonym that decomposes an application of a type constructor
at the kind level. First, we need a data type to store this kind [[^5]]:

{% highlight haskell %}
data P (c :: Constraint) = MkP
{% endhighlight %}

Now we need a way to decompose something of kind `P`:

{% highlight haskell %}
type DecomposeConstraint (x :: P (f a b)) = f
{% endhighlight %}

This is the fancy part. We take the argument (which must have kind `P (f a b)`),
decompose the `f` part and return it as the right-hand side type. The use of `f` as
both a kind variable _and_ a type variable is only possible on GHC 8.0 or later,
where types and kinds are one and the same.

Finally, defining `CTuple2` is just a matter of plumbing the right types through:

{% highlight haskell %}
type CTuple2 = DecomposeConstraint (MkP :: P (Read Int, Show Int))
{% endhighlight %}

This version of `CTuple2` can do all the things that the version in the previous
section can do with the added benefit of being usable in instance heads.

# Parting thoughts

I have demonstrated four different techniques for defining partially applicable
constraint tuples types in GHC. Which technique you prefer the most will likely
depend on your personal tastes, but they all accomplish mostly the same things.
I have used `CTuple2` as the running example
throughout this post, but the same ideas also allow you define `CTuple3`,
`CTuple4`, or whatever-sized constraint tuple you may desire. As a proof-of-concept,
I have created a
[`constraint-tuples`](http://hackage.haskell.org/package/constraint-tuples)
library which defines constraint tuples of varying sizes using each of the
four techniques documented here.

-----

[^1]: See [here](https://stackoverflow.com/a/51812559) for a lengthier
      explanation of why catch-all instances are usually discouraged.

[^2]: See [this blog post]((../../../../2019/05/26/on-the-arity-of-type-families))
      for an extended discussion about why type synonyms (and type families)
      cannot be partially applied.

[^3]: The term "constraint synonym" should not be confused with a type synonym
      that uses a constraint on the right-hand side, such as
      `type MyShow a = Show a`.

[^4]: To be pedantic, newtypes don't actually exist in Core, since all uses of
      newtypes are compiled to coercions. For the sake of this post, however,
      this distinction isn't important.

[^5]: `P` is just a kind-restricted version of
      [`Proxy`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Proxy.html)
      from the `base` library with a different name.
