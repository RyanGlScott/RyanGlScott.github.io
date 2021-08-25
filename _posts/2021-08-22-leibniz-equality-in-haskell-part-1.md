---
layout: post
title: Leibniz equality in Haskell, part 1
---

When using types, one often asks the question: when are two types
the same? There are many different ways to encode the sameness of two types.
In GHC, one of the most common ways is through
[this data type](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Type-Equality.html#t::-126-:),
defined in `Data.Type.Equality`:

{% highlight haskell %}
type (:~:) :: k -> k -> Type
data a :~: b where
  Refl :: a :~: a
{% endhighlight %}

I don't know how to pronounce "`(:~:)`", so I'm going to call this the
_propositional equality_ data type. Propositional equality is pretty neat. It's
used as a building block in several libraries, including a type-safe
cast function in `Data.Type.Equality` named
[`castWith`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Type-Equality.html#v:castWith).

Now let's suppose we live in the bad timeline where GHC never gained the
ability to define GADTs such as `(:~:)`. Bummer. (Yes, I know this is a totally
contrived scenario. Just go along with it for now.)

In this timeline, would we have to give up the
nice things we had in the other timeline, such as `castWith`? Perhaps surprisingly,
we wouldn't! There is another way to define an equality data type, called
_Leibniz equality_, that is equivalent in power to propositional equality.
In this post, we will explore what Leibniz equality is and how to use it.

# What exactly is Leibniz equality?

Leibniz equality is the principle that if two things are indistinguishable in
any context, then the things must be the same. This is better known among
philosophers as the [identity of indiscernibles](https://en.wikipedia.org/wiki/Identity_of_indiscernibles).
Because "identity of indiscernibles" has way too many syllables, someone
decided to abbreviate this to "Leibniz's law", named after
[Gottfried Wilhelm Leibniz](https://en.wikipedia.org/wiki/Gottfried_Wilhelm_Leibniz)
[[^1]].

What does Leibniz's law look like as code? Here is the basic idea:

{% highlight haskell %}
type Leibniz :: k -> k -> Type
type Leibniz a b = forall c. c a -> c b
{% endhighlight %}

The type `Leibniz a b` states that for all contexts `c`, `c a` must be
convertible to `c b`. From a types-as-propositions perspective, this means that
every proposition that is true of `a` must also be true of `b`. In other words,
`a` and `b` are equal. This is perhaps not the first thing you would try when
defining your own equality type, but don't worry—it will become more intuitive
as we proceed through the post and provide more examples.

Later on, it will be convenient to have a data type counterpart to the `Leibniz`
type synonym. Let's define such a data type now:

{% highlight haskell %}
type (:=) :: k -> k -> Type
newtype a := b = Refl { subst :: forall c. c a -> c b }
{% endhighlight %}

From here on out, when I refer to "Leibniz equality", I'm referring to the
`(:=)` data type.

There are two things worth highlighting about `(:=)`. First, note that
we've named the record selector `subst`. This is a deliberate choice: using
`subst` on a value of type `a := b` is akin to substituting `a` for `b` in
some context `c`. We will see several examples of this kind of substitution
later in the post.

Second, the name `(:=)` is quite similar to `(:~:)` from `Data.Type.Equality`.
Again, this is intentional, as `(:=)` is intended to be a viable alternative
to `(:~:)`. Later, we will show how one can go from `(:=)` to
`(:~:)` and vice versa.

## Credit where credit is due

At this point, I should be clear that I am not the first person
to turn Leibniz's law into code. A version of the `(:=)` data type appears in
the paper ["Typing Dynamic Typing"](https://dl.acm.org/doi/abs/10.1145/581478.581494)
by Baars and Swierstra. Oleg Kiselyov later
[refined this idea](http://okmij.org/ftp/Computation/extra-polymorphism.html#injectivity),
and Edward Kmett packaged this up as
[the `eq` library](https://hackage.haskell.org/package/eq).
For the most part, this post follows the naming conventions established in the
`eq` library.

# Leibniz equality basics

## Reflexivity

So now that we know the definition of Leibniz equality, what can we actually do
with it? Let's start with a simple one: can we prove that a type is equal to itself?
That is, can we define a function with this type?

{% highlight haskell %}
refl :: a := a
{% endhighlight %}

We can be pretty sure that we'll need to use the `Refl` newtype constructor
to construct a value of this type, so let's start with that:

{% highlight haskell %}
refl :: a := a
refl = Refl _
{% endhighlight %}

The hole `_` has type `forall c. c a -> c a`, so we're going to need to fill it
with a function of some sort. Moreover, it has to work for any possible choice
of `c`. There's basically only one valid choice here: the `id` function.

{% highlight haskell %}
refl :: a := a
refl = Refl id
{% endhighlight %}

That's it! Although this definition is quite small, it plays a key role in
making everything work. At its core, Leibniz equality is really just the
`id` function packaged up in a clever way.

## Type-safe cast

While `refl` provides a way to construct Leibniz equality values, we also need
a way to use them. A classic use case for equality data types is
a type-safe cast function. The `Data.Type.Equality` module
provides such a function named
[`castWith`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Type-Equality.html#v:castWith).
Let's try to define the Leibniz counterpart to the `castWith` [[^2]]:

{% highlight haskell %}
castWith :: (a := b) -> a -> b
{% endhighlight %}

As a first take, let's introduce the arguments:

{% highlight haskell %}
castWith :: (a := b) -> a -> b
castWith (Refl aEqualsB) x = _
{% endhighlight %}

Here, `aEqualsB :: forall c. c a -> c b` and `x :: a`, while our ultimate goal is to
fill in the hole `_` with something of type `b`. Squinting at the type of `f` reveals one
possible path forward, as `f` converts something of type `c a` to type `c b`
(for any context `c`). If we can turn `x :: a` into something of `c a`, convert it to
type `c b`, and then turn that into type `b`, then we'll be done.

Here's the tricky part: what do we use for `c`? The situation now is different from when
we were implementing `refl`. There, we had to _produce_ a value of type
`forall c. c a -> c a`, which meant that we couldn't know in advance what `c`
was. With `castWith`, however, we instead have an _argument_ of type
`forall c. c a -> c b`, which means that we can apply the argument to whatever
we want. In other words, we can instantiate `c` to a more specialized type that
is better tailored to our particular use case.

In the case of `castWith`, we need a choice of `c` that allows us to easily go
from type `a` to `c a`, as well as from type `c b` to `b`. It just so happens
that the `Identity` type is a perfect fit for this situation:

{% highlight haskell %}
type Identity :: Type -> Type
newtype Identity a = Identity { runIdentity :: a }
{% endhighlight %}

If we instantiate `c` with `Identity`, then our approach becomes clear. We
can go from type `a` to `Identity a` via the `Identity` data constructor,
and we can go from type `Identity a` to `a` via the `runIdentity` record
selector. What's more, because `Identity` is a newtype, there is no
additional runtime cost to using it.

Let's put all of the pieces together and implement `castWith`:

{% highlight haskell %}
castWith :: (a := b) -> a -> b
castWith (Refl aEqualsB) x =
  runIdentity (aEqualsB @Identity (Identity x))
{% endhighlight %}

Or, if you prefer using record selectors, you can use `subst`:

{% highlight haskell %}
castWith :: (a := b) -> a -> b
castWith aEqualsB x =
  runIdentity (subst aEqualsB @Identity (Identity x))
{% endhighlight %}

Now we're done! Note that the use of visible type application in `@Identity`
is not strictly necessary here, as GHC is capable of inferring this type.
I'm including it primarily as a reminder of which context `c` we are picking.
This will be helpful as we go and encounter other examples with more interesting
choices of `c`.

There are two other things about the implementation of `castWith` that are
worth highlighting. First, note that if you ignore all of the newtype wrapping
and unwrapping, `castWith` simply applies `aEqualsB` to `x`. In other words,
a type-safe cast is a very fancy way of applying the `id` function.

Second, note that the approach we used—figuring out a suitable newtype to instantiate
`c` with—is the same approach that we will use for the remainder of the post.
In some ways, using `(:=)` is like a jigsaw puzzle, but all of the jigsaw
pieces are shaped like type contexts.

## Symmetry

Equality is an equivalence relation, and Leibniz equality is no exception.
We've already shown that `(:=)` is reflexive by implementing `refl`, so
let's complete the trilogy by showing that it is symmetric and transitive.

Symmetry states that if `a` equals `b`, then `b` also equals `a`. In terms
of code, that looks like this:

{% highlight haskell %}
symm :: forall a b. (a := b) -> (b := a)
{% endhighlight %}

Given an argument of type `a := b`, how can we use that
to produce a value of type `b := a`? We can procure a value of type `a := a`
using `refl`, so if we can somehow turn the first `a` in that type into a `b`,
then we'll be finished.

The interesting part is figuring out how to convert _only_ the first `a` in the
type `a := a` to `b`. It may be tempting to implement `symm` like this:

{% highlight haskell %}
symm :: forall a b. (a := b) -> (b := a)
symm aEqualsB = subst aEqualsB (refl :: a := a)
{% endhighlight %}

Unfortunately, this doesn't quite work. This is perhaps easier to see if we
use visible type application:

{% highlight haskell %}
symm :: forall a b. (a := b) -> (b := a)
symm aEqualsB = subst aEqualsB @((:=) a) (refl :: a := a)
{% endhighlight %}

We can see here that the choice of context `c` is `((:=) a)`. In other words,
we are converting something of type `a := a` to type `a := b`, which is the
opposite order that we want.

Luckily, we can invent a more suitable context without too much trouble. The issue
with using `((:=) a` as the context was that it gave the wrong order, so what
if we used a context that flipped the order around? We'll call this context
`Symm`:

{% highlight haskell %}
type Symm :: k -> k -> Type
newtype Symm a b = Symm { unsymm :: b := a }
{% endhighlight %}

`Symm` is a thin wrapper around `(:=)` that swaps the order in which
`a` and `b` appear. The order makes all the difference
when `Symm a` is chosen as the context. With Leibniz equality, we can
go from `Symm a a` to `Symm a b`. If we peel off one layer of newtypes,
that is tantamount to going from `a := a` to `b := a`.

It's time to put this idea into practice. We'll start off with a value
of type `a := a`:

{% highlight haskell %}
symm :: forall a b. (a := b) -> (b := a)
symm aEqualsB = _ refl
{% endhighlight %}

Next, we'll turn that into something of type `Symm a a`:

{% highlight haskell %}
symm :: forall a b. (a := b) -> (b := a)
symm aEqualsB = _ (Symm refl)
{% endhighlight %}

Then, we'll use the argument of type `a := b` to substitute into the
`Symm a a` value to turn it into something of type `Symm a b`:

{% highlight haskell %}
symm :: forall a b. (a := b) -> (b := a)
symm aEqualsB = _ (subst aEqualsB @(Symm a) (Symm refl))
{% endhighlight %}

Finally, we'll unwrap the `Symm a b` to obtain something of type
`b := a`:

{% highlight haskell %}
symm :: forall a b. (a := b) -> (b := a)
symm aEqualsB = unsymm (subst aEqualsB @(Symm a) (Symm refl))
{% endhighlight %}

## Transitivity

To finish our earlier claim that Leibniz equality is an equivalence relation,
we need to prove that it is transitive. That is, we need to implement this
function:

{% highlight haskell %}
trans :: forall a b c. (a := b) -> (b := c) -> (a := c)
{% endhighlight %}

Although `trans` has a somewhat hefty type signature, implementing `trans` is
arguably more straightforward than implementing `symm`. All we need to do is
take the argument of type `a := b` and turn it into type `a := c`. As luck
would have it, the argument of `b := c` is just what we need to turn the `b`
into a `c`. Moreover, we can choose `((:=) a)` as the context for `subst`,
so we don't even need an extra newtype!

{% highlight haskell %}
trans :: forall a b c. (a := b) -> (b := c) -> (a := c)
trans aEqualsB bEqualsC = subst bEqualsC @((:=) a) aEqualsB
{% endhighlight %}

Alternatively, one can go the other way around and use the `b := c` argument to
substitute into `a := b`. This is slightly less direct, as it requires using
`symm` to flip the order of equalities around in various places.

# Equivalence of Leibniz and propositional equality

Leibniz equality and propositional equality are equivalent, as one can convert
from the former to the latter and vice versa. To demonstrate this, let's start
by showing how to convert a propositional equality value to a Leibniz equality one:

{% highlight haskell %}
toLeibniz :: forall a b. (a :~: b) -> (a := b)
{% endhighlight %}

This direction is fairly simple to implement, as one can just pattern match on
the argument of type `a :~: b` to convince the typechecker that `a` equals `b`.
At that point, all we need is to produce a value of type `a := a`, which `refl`
accomplishes [[^3]]:

{% highlight haskell %}
import Data.Type.Equality as Eq

toLeibniz :: forall a b. (a :~: b) -> (a := b)
toLeibniz Eq.Refl = refl @a
{% endhighlight %}

For the other direction, we need to convert from Leibniz to propositional
equality:

{% highlight haskell %}
fromLeibniz :: forall a b. (a := b) -> (a :~: b)
{% endhighlight %}

The `Eq.Refl` constructor gives us a value of type `a :~: a`. From there,
we can substitute the second `a` for `b` in that type by way of `subst`,
picking `((:~:) a)` as the context:

{% highlight haskell %}
fromLeibniz :: forall a b. (a := b) -> (a :~: b)
fromLeibniz aEqualsB = subst aEqualsB @((:~:) a) Eq.Refl
{% endhighlight %}

# Generativity and injectivity

Before we conclude our tour of Leibniz equality, let's look at a bonus
challenge that takes some extra thought to figure out. One unique property
of propositional equality in GHC is that it can be _decomposed_. One can
decompose an equality in two different ways. The first way is by decomposing
the argument types in an equality between two applications:

{% highlight haskell %}
inj :: (f a :~: f b) -> (a :~: b)
inj Eq.Refl = Eq.Refl
{% endhighlight %}

This works because type constructors are _injective_ in Haskell, so if `f a`
equals `f b`, then the typechecker can conclude that `a` equals `b` [[^4]].
In addition to the argument types, one can also decompose the _function_ types
in an equality between two applications. This property is referred to as
_generativity_:

{% highlight haskell %}
gen :: (f a :~: g a) -> (f :~: g)
gen Eq.Refl = Eq.Refl
{% endhighlight %}

GHC's constraint solver makes it straightforward to implement `inj` and `gen`
over propositional equality: just pattern match on `Eq.Refl`. Implementing the
same functionality for Leibniz equality, on the other hand, proves to be
slightly trickier. To show you what I mean, let's try implementing a Leibniz
version of `inj`:

{% highlight haskell %}
injLeibniz :: forall f a b. (f a := f b) -> (a := b)
{% endhighlight %}

Reaching into our bag of tricks that we've accumulated throughout this post,
we can start by using `refl :: a := a`. At that point, we need to turn the
second `a` in `a := a` into a `b`. Unfortunately, the argument we have is
not of type `a := b`, but rather `f a := f b`. Trying to use that to
substitute into `a := b` simply won't work.

Another tempting approach is to use `refl :: f a := f a` instead. That won't
get us any further, however, since all we could do is use the argument to turn
it into something of type `f a := f b`. This is the same type as the argument
itself, so that doesn't get us anywhere.

The problem is that we need to concoct a way to turn `f x` into `x` for any
`x`. This was an issue that irked Haskellers trying to use Leibniz equality for
many years. An especially high-profile example of this was documented in the
2004 paper
[_Implementing Cut Elimination: A Case Study of
Simulating Dependent Types in Haskell_](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.59.2300&rep=rep1&type=pdf).

Luckily, Oleg Kiselyov discovered a solution to this problem: type families.
Type families have the power to match on applications of type constructors, and
this is exactly the power we need to implement `injLeibniz`. Here is an example of
such a type family:

{% highlight haskell %}
type ArgOfApp :: j -> k
type family ArgOfApp fa where
  ArgOfApp (f a) = a
{% endhighlight %}

`ArgOfApp` will check if its argument consists of a type constructor applied to
an argument, and if so, it will return the argument type. For example,
`ArgOfApp (Maybe Int)` and `ArgOfApp [Int]` will reduce to `Int`. On the other
hand, `ArgOfApp Int` will not reduce at all, since `Int` by itself doesn't have
the right shape.

With `ArgOfApp`, we can build a newtype that will act as a suitable context for
`subst`:

{% highlight haskell %}
type InjNewtype :: j -> k -> Type
newtype InjNewtype a fa = InjNewtype { unInjNewtype :: a := ArgOfApp fa }
{% endhighlight %}

It is worth spending some time thinking about the definition of this newtype, as it
pulls a lot of weight. Because of the use of `ArgOfApp fa`,
if `fa` is instantiated to be a type constructor application, then the
underlying field will be reduced. For instance, let's suppose we had a value
of type `InjNewtype a (f a)`. If we strip off the `InjNewtype` constructor,
we would have something of type `a := ArgOfApp (f a)`, which reduces to
`a := a`. In fewer words: `InjNewtype refl` has the type
`InjNewtype a (f a)`.

`InjNewtype` is exactly the substitution context we need for `injLeibniz`.
Recall that in `injLeibniz`, we have an argument of type `f a := f b`, which
limits us to only substituting occurrences of `f a`. If we have something
of type `InjNewtype a (f a)`, then we can convert it to type
`InjNewtype a (f b)`. Finally, removing the `InjNewtype` constructor would
give us something of type `a := ArgOfApp (f b)`. This reduces to `a := b`,
the return type of `injLeibniz`.

Now that we have a better understanding of what role `InjNewtype` serves,
let's finally use it to implement `injLeibniz`:

{% highlight haskell %}
injLeibniz :: forall f a b. (f a := f b) -> (a := b)
injLeibniz faEqualsFb =
  unInjNewtype (subst faEqualsFb @(InjNewtype a)
    (InjNewtype refl :: InjNewtype a (f a)))
{% endhighlight %}

And we're done! It took a surprising amount of preparation to get to this
point, but once we applied the key insight involving type families,
the rest fell into place pretty naturally. Now that I've shown how to
implement `injLeibniz`, see if you can implement a Leibniz version of
the `gen` function from earlier as an exercise.

It's somewhat interesting that of all the facts about equality that we
discussed in this post, only injectivity and generativity require the use of
type families. There is probably a deeper, type-theoretic explanation as to why
this is the case. That being said, I am definitely not a type theorist, so I'll
defer to more knowledgeable people on this point.

# Parting thoughts

In this post, I introduced Leibniz equality, an alternative formulation of
equality that does not require the use of GADTs to define or use. Before GHC
gained the ability to use GADTs, Leibniz equality was a very popular technique
for encoding type equality in Haskell. In modern GHCs, Leibniz equality has
fallen out of fashion somewhat, since the propositional equality type `(:~:)` is arguably
more convenient to use. Still, there are some places where Leibniz equality
enjoys continued use. For example, the
[`Equality`](https://hackage.haskell.org/package/lens-5.0.1/docs/Control-Lens-Combinators.html#t:Equality)
type from the `lens` library can be thought of as a particular encoding of
Leibniz equality with more type parameters.

For the most part, the ideas that I've presented in this post are a faithful
retelling of the existing Haskell literature about Leibniz equality. (See
[this section of the post](#credit-where-credit-is-due)
for links to said literature.) In the next installment of this blog series,
I'll explore _heterogeneous_ Leibniz equality, where the kinds of the types
being equated are allowed to differ. This form of Leibniz equality is less
discussed in the Haskell literature, due in no small part to the fact that it
requires modern GHC tricks to even define it.

-----

[^1]: Yeah, the same Leibniz that discovered calculus. He was also fond of
      [monads](https://en.wikipedia.org/wiki/Monad_(philosophy)), so I'd like to
      think that if Leibniz were alive today, he'd be a functional programmer.

[^2]: This is called
      [`coerce`](https://hackage.haskell.org/package/eq-4.2.1/docs/Data-Eq-Type.html#v:coerce)
      in the `eq` library, but I've
      opted to call it `castWith` in this post to avoid confusion with the
      [`coerce`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Coerce.html#v:coerce)
      function from `Data.Coerce`, which is
      an entirely separate thing.

[^3]: I will use `Eq.Refl :: a :~: a` from here on out to disambiguate it from
      the `Refl` constructor for `(:=)`.

[^4]: GHC has a somewhat unique take on injectivity compared to languages such
      as Agda and Idris, which do not assume injectivity of unknown
      type constructors. For more details, see the paper
      [_Higher-order Type-level Programming Haskell_](https://www.microsoft.com/en-us/research/uploads/prod/2019/03/ho-haskell-5c8bb4918a4de.pdf).
