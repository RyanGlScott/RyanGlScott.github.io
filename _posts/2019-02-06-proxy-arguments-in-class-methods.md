---
layout: post
title: Proxy arguments in class methods&#58; a comparative analysis
---

The `Foreign.Storable` module provides a `Storable` class with two rather unusual methods:

{% highlight haskell %}
class Storable a where
  sizeOf    :: a -> Int
  alignment :: a -> Int
  ...
{% endhighlight %}

What is unusual about `sizeOf` and `alignment`? To demonstrate, consider what happens if we look up the sizes of various `Int8`s:

{% highlight plaintext %}
λ> sizeOf (0 :: Int8)
1
λ> sizeOf (1 :: Int8)
1
λ> sizeOf (2 :: Int8)
1
λ> all (\c -> sizeOf c == 1) [minBound..maxBound :: Int8]
True
{% endhighlight %}

No matter what `Int8` value we provide as an argument, `sizeOf` will always give the same answer: 1 byte. In fact, `sizeOf` completely ignores its argument! You could just as well pass an infinite loop to `sizeOf`, and it will return the same answer:

{% highlight plaintext %}
λ> sizeOf (let x = x in x :: Int8)
1
{% endhighlight %}

It's not just the `Storable Int8` instance that exhibits this quirk, either. Every single `Storable` instance in the `base` library implements `sizeOf` and `alignment` so that they ignore their arguments. From a certain perspective, `sizeOf` and `alignment` are "metadata" methods of `Storable`. They make use of the type of the argument to look up the appropriate instance, but nothing more.

Many Haskellers (understandably) find it quite weird that these methods never make use of their argument, and various competing designs have emerged which attempt to address this weirdness. If these competing designs had won out when the `base` library was first being developed, then we might have a `Storable` class that looks like this instead:

{% highlight haskell %}
class Storable a where
  sizeOf    :: Proxy a -> Int
  alignment :: Proxy a -> Int
{% endhighlight %}

Or perhaps this:

{% highlight haskell %}
class Storable a where
  sizeOf    :: proxy a -> Int
  alignment :: proxy a -> Int
{% endhighlight %}

Or even this:

{% highlight haskell %}
class Storable a where
  sizeOf    :: Int
  alignment :: Int
{% endhighlight %}

I've often wondered if there is one design to rule them all, so I wrote up this blog post in an attempt to better understand the pros and cons of each approach. Will we discover which design is unambiguously the best one by the end? Let's find out.

# Dummy arguments

> Requirements: Any version of GHC

I would be remiss not to include the original design that sparked this whole conversation in the first place. Any time you have a class of the form:

{% highlight haskell %}
class C a where
  metaData :: a -> Blah
{% endhighlight %}

If `metaData` doesn't ever inspect its argument, then it has a dummy argument.

## Pros

_Simple types_. There's not much hassle involved in defining `metaData`, since its type `a -> Blah` is about as simple as it gets. (Hey, I had to think of _something_ to put in the Pros column.)

## Cons

_Awkward APIs_. The type `a -> Blah` alone doesn't communicate the fact that its argument is never used, which means you have to document this fact (or worse, have users discover this themselves through trial and error).

_Some creativity may be required to use it_. It's not always clear what to pass to `metaData` as an argument. For instance, what if you have an instance of `C Void`? At that point, your only option is to pass something like `undefined :: Void` or `let x = x in x :: Void` as an argument, so you'd better hope that `metaData` never forces this argument!

# `Proxy` arguments

> Requirements: Any version of GHC

One of the major drawbacks of the dummy arguments approach is that you often have to invent some value to pass as an argument. What if we could take away this guesswork? This is what `Proxy` gives you:

{% highlight haskell %}
data Proxy a = Proxy
{% endhighlight %}

`Proxy` is a data type with a single constructor. Moreover, its type parameter `a` is a phantom type, so one never needs to actually come up with an inhabitant of `a` in order to create a `Proxy`. In other words, any time you need a `Proxy a`, it's as simple as typing "`Proxy`".

Using `Proxy`, we can redesign our class like so:

{% highlight haskell %}
class C a where
  metaData :: Proxy a -> Blah
{% endhighlight %}

## Pros

_No creativity required_. Any time you need to use `metaData`, all you have to do is pass it a `Proxy` at the right type.

_More descriptive types_. Without any other knowledge of what `metaData` does, its type `Proxy a -> Blah` is a pretty clear indication that `a` is only used for its type.

## Cons

_Awkward instantiation_. When you type `metaData Proxy`, more often than not GHC will complain that the type of `Proxy` is ambiguous. If you're using just Haskell 98, then resolving this ambiguity might require an explicit type signature of the form `(Proxy :: Proxy a)`, which is rather verbose. If you're allowed to use GHC extensions, then you can make this much more tolerable with `TypeApplications`, since you can shorten this to just `(Proxy @a)`.

_To match or not to match_. When implementing `metaData`, do you write this?

{% highlight haskell %}
instance C Foo where
  metaData Proxy = ...
{% endhighlight %}

Or this?

{% highlight haskell %}
instance C Foo where
  metaData _ = ...
{% endhighlight %}

This is a subtle distinction, but one that can be important, since it's
possible that someone out there is invoking `metaData (undefined :: Proxy a)`,
and explicitly matching on the `Proxy` constructor would cause an exception at
runtime. This suggests that the most permissive way to implement `metaData` is
the latter form (without the match on `Proxy`). This is something that you
must remember to do, however, as GHC won't warn you if you don't.

_You're locked into using `Proxy`_. A very minor drawback, but hard-coding the
use of `Proxy` in the type of `metaData` means that you can't use any other
types whose last type parameter is `a`. (More on this later.)

# `Proxy#` arguments

> Requirements: GHC 7.8 or later

A minor variation on the previous design is to use the primitive `Proxy#` data
type from `GHC.Exts`:

{% highlight haskell %}
data Proxy# a -- Abstract
proxy# :: Proxy# a
{% endhighlight %}

`Proxy#` is very similar to `Proxy`, but with two important distinctions:

1. Unlike `Proxy`, `Proxy#` is unlifted, so it cannot be inhabited by a thunk.
2. `Proxy#` is a zero-width data type. That is, it provides a guarantee that it
   will not occupy any memory at runtime.

## Pros
`Proxy#` inherits all of the [pros](#pros-1)
of `Proxy`. Some advantages that `Proxy#` has
over `Proxy` are:

_No matching ambiguity_. Unlike with `Proxy`, where you had a choice of
whether to match on the `Proxy` constructor or not, there is no such issue
with `Proxy#`, as the constructor for `Proxy#` is not exposed in the first
place. There are very few things you can do with an argument of `Proxy# a`
(besides pass it to other functions), and this is a good thing, since it
doesn't give you any way to shoot yourself in the foot.

_Slightly more efficient_. Since `Proxy#` has a compile-time guarantee about
its memory usage, one could argue that using `Proxy#` is more space-efficient
than `Proxy`. Then again, GHC has a very powerful simplifier that will likely
optimize away the overhead of `Proxy` arguments, so it's unlikely that `Proxy`
will be a performance bottleneck in practice.

## Cons
_It's magic_. Using `Proxy#` requires users to remember to turn on the
`MagicHash` extension in order to use, and failing to remember this will result
in somewhat obscure parser errors. (This is a problem that arises less often
after you develop the habit of instinctively enabling `MagicHash` before diving
into GHC's primitives, but it can be confusing for newcomers.)

`Proxy#` also inherits some of the [cons](#cons-1)
of `Proxy`, such as:

_Awkward instantiation_. This time, you might need to remember to explicitly
write out `(proxy# :: Proxy# a)`, or `proxy# @_ @a` with type applications.
(Yes, [that `@_` is required](https://ghc.haskell.org/trac/ghc/ticket/16293)!)

_You're locked into using `Proxy#`_. I keep mentioning this as a drawback, so
I should follow up on my promise to explain this con in more detail...

# `proxy` arguments

> Requirements: Any version of GHC

Having `Proxy` (or `Proxy#`) arguments in class methods is all well and good,
but it is a bit limiting in that it forces you to, well, instantiate the methods
with `Proxy`. Sometimes, however, you might already have another value lying
around with a type that mentions `a`. For example, you might have an
[`M1 i c f a`](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html#t:M1)
(if you're working with [`GHC.Generics`](https://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-Generics.html)) or a
[`Sing a`](https://hackage.haskell.org/package/singletons-2.5.1/docs/Data-Singletons.html#t:Sing)
(if you're working with the [`singletons`](https://hackage.haskell.org/package/singletons-2.5.1) library)
or a
[`TypeRep a`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Type-Reflection.html#t:TypeRep)
(if you're working with [`Type.Reflection`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Type-Reflection.html)).
In these situations, it might be convenient to specify what `a` is by simply
passing one of these values that you already have instead of conjuring up a
separate `Proxy` for this purpose.

Luckily, there is a very simple trick for making your class methods this permissive:
just generalize `Proxy` to `proxy`!

{% highlight haskell %}
class C a where
  metaData :: proxy a -> Blah
{% endhighlight %}

In other words, turn `Proxy` into a type variable. This way, `proxy` can be
instantiated with `Proxy`, `M1 i c f`, `Sing`, `TypeRep`, or whatever your heart
may desire.

## Pros

_Minimal creativity required, but maximal creativity permitted_. The name "`proxy`"
very strongly hints at how it is intended to be instantiated 90% of the time, so
most people will know how to use it just from the type alone. At the same time,
adventurous users who with to instantiate `proxy` with things besides `Proxy`
have the freedom to do so.

_No matching ambiguity_. One nice side effect of making `metaData` parametric
over `proxy` is that you can't match it against a specific constructor anymore.
Like values of type `Proxy# a`, the only thing you can really do with a value of
type `proxy a` is pass it around.

## Cons

If I had written this blog post even one year ago, I would have struggled to
think of any real downsides to this approach over using `Proxy`. But recently,
Oleg Grenrus clued me in to a very serious downside of using `proxy`:

_Incompatible with `GeneralizedNewtypeDeriving`_. It turns out that, unlike any of
the other approaches I discuss in this blog post [[^1]], this one does not work when
combined with the
[`GeneralizedNewtypeDeriving`](https://downloads.haskell.org/~ghc/8.6.3/docs/html/users_guide/glasgow_exts.html?highlight=generalizednewtypederiving)
language extension. I was stunned when I realized
this, and it took me a while to realize why this is the case. To set the scene,
imagine that we have this code:

{% highlight haskell %}
class C1 a where
  metaData1 :: Proxy a -> Blah
instance C1 Int where
  metaData1 _ = ...

class C2 a where
  metaData2 :: proxy a -> Blah
instance C2 Int where
  metaData2 _ = ...

newtype Age = MkAge Int
  deriving (C1, C2)
{% endhighlight %}

This derives instances for `Age` by reusing the
instances of the underlying type, `Int`.
As it turns out, GHC can successfully derive a `C1 Age` instance, but not a
`C2 Age` instance, as it will fail with this error:

{% highlight plaintext %}
    • Couldn't match representation of type ‘proxy Int’
                               with that of ‘proxy Age’
        arising from the coercion of the method ‘metaData2’
          from type ‘forall (proxy :: * -> *). proxy Int -> Blah’
            to type ‘forall (proxy :: * -> *). proxy Age -> Blah’
      NB: We cannot know what roles the parameters to ‘proxy’ have;
        we must assume that the role is nominal
    • When deriving the instance for (C2 Age)
{% endhighlight %}

Uh-oh. Roles strike again. If you want to know more about roles and what this error message
means in more detail, I encourage you to read my
[previous](../../../../2018/03/04/how-quantifiedconstraints-can-let-us-put-join-back-in-monad/)
blog
[posts](../../../../2018/06/22/quantifiedconstraints-and-the-trouble-with-traversable/)
on the subject. For the purposes of _this_ blog post, it suffices to say that GHC
is unable to take something of type `proxy Int` and coerce it to (i.e., reuse as)
something of type `proxy Age` because the type variable `proxy` is abstract, and GHC
doesn't know how to look underneath it.

This is a serious bummer, since `GeneralizedNewtypeDeriving` is used ubiquitously
to generate instances for newtypes for cheap. Moreover, since
[`DerivingVia`](https://downloads.haskell.org/~ghc/8.6.3/docs/html/users_guide/glasgow_exts.html?highlight=derivingvia)
also generates instances in a similar fashion, this also renders this technique
incompatible with `DerivingVia`. Surely there must be a way around this limitation?

# "Representational" `proxy` arguments

> Requirements: GHC 8.6 or later

Yes, there _is_ a way around this limitaiton! We'll need some fancy types to
accomplish it, however. The trick is to reuse this type synonym that I introduced in
[this blog post](../../../../2018/06/22/quantifiedconstraints-and-the-trouble-with-traversable/)
about `QuantifiedConstraints`:

{% highlight haskell %}
type Representational1 m =
  forall a b. Coercible a b => Coercible (m a) (m b)
{% endhighlight %}

Essentially, this says that we can coerce between `m a` and `m b` under the
assumption that we can coerce between `a` and `b`. (In the language of roles,
this would be as if `m` was representationally roled in its argument.) With
`Representational1` in hand, we can tweak `metaData` slightly:

{% highlight haskell %}
class C a where
  metaData :: Representational1 proxy
           => proxy a -> Blah
{% endhighlight %}

Giving `proxy` a `Representational1` constraint gives us a way to tell GHC that
it's quite alright to look underneath `proxy` when determining if `proxy Int` can
be coerced to `proxy Age`. Sure enough, if we try it out:

{% highlight haskell %}
instance C Int where
  metaData _ = ...

newtype Age = MkAge Int
deriving instance C Age
{% endhighlight %}

Then GHC accepts it! Note that I'm deliberately using `StandaloneDeriving` in the
code above instead of simply writing `newtype Age = MkAge Int deriving C`, since the
latter won't typecheck due to
[unfortunate technical reasons](https://ghc.haskell.org/trac/ghc/ticket/15290).

Perhaps you want your `proxy` type variable to act even _more_ like `Proxy`. One
thing to note about `Proxy` is that its type argument is phantom, so `Proxy a` can
be coerced to `Proxy b` for any types `a` and `b`. If you wish to encode this as
a quantified constraint, you can whip up the following type:

{% highlight haskell %}
type Phantom1 m = forall a b. Coercible (m a) (m b)
{% endhighlight %}

If you change the context of `metaData` to be `Phantom1 proxy`, this will require
that `proxy` be instantiated only with type constructors that have phantom types.

## Pros

This inherits most of the [pros](#pros-3) of the `proxy` approach, minus the downside
of being incompatible with `GeneralizedNewtypeDeriving`.

## Cons

_Does anyone actually use this?_ As far as I know, I invented this technique for the
first time when writing this blog post, so I would be shocked if anyone was using
this trick in the wild. That isn't necessarily a sign that this technique is flawed,
only that it's a new development.

_Slightly less creativity permitted_. If `proxy` has no constraints, then users can
instantiate it with whatever type constructor they wish. If `proxy` has a
`Representational1` constraint, however, then users are slightly more restricted
in what they can do. They wouldn't be able to instantiate `proxy` with `Sing` or
`TypeRep`, for instance, since both of those type constructors are nominally roled
in their argument. (That is, in order to coerce from `Sing a` to `Sing b`, then
`a` and `b` must be _exactly_ the same type.) Users will be even more limited if
`proxy` has a `Phantom1` constraint.

# No explicit arguments at all

> Requirements: GHC 8.0 or later

To round off my analysis, I want to mention one more approach that started to
become viable in GHC 8.0. Unlike the other designs in this blog post, this
design eschews the use of explicit arguments altogether:

{% highlight haskell %}
class C a where
  metaData :: Blah
{% endhighlight %}

One thing to note is that GHC will reject the code above by default, since `a` is
ambiguous in the type of `metaData` (`forall a. C a => Blah`). You can relax this
restriction, however, with the `AllowAmbiguousTypes` extension. In order to actually
_use_ `metaData`, you'll need to make use of another language extension,
`TypeApplications`:

{% highlight haskell %}
metaData @a
{% endhighlight %}

Besides this funkiness, this approach works just like the other ones in this
post [[^2]].

## Pros

_Brevity_. This approach requires by far the least amount of typing. Instead of
having to plumb around proxies to inform `metaData` what `a` is, `metaData` relies
on liberal use of `TypeApplications` to do the heavy lifting.

## Cons

_Potential for confusing error messages_. If you accidentally forget to provide
`metaData` with its type argument `@a`, you can get extremely disorienting error
messages like this one:

{% highlight plaintext %}
    • No instance for (C a0) arising from a use of ‘metaData’
    • In the expression: metaData
{% endhighlight %}

If you're unaccustomed to the quirks of GHC's error messages, it might require
some head-scratching before you realize what the source of the problem is.

This isn't a problem that's exclusively limited to this approach, since GHC might
complain similarly if you type `metaData Proxy` (without the `a`).
But you if start to type "`Proxy`", it's somewhat
easier to remember that you need to finish the thought by typing out "`Proxy @a`" in full.
On the other hand, invisible type arguments can be easily forgotten altogether.

-----

# Final thoughts

That concludes my analysis of all the different ways one can define "metadata"
methods in type classes. To recap, we have looked at the following designs:

1. Dummy arguments
2. `Proxy` arguments
3. `Proxy#` arguments
4. `proxy`
5. "Representational" `proxy` arguments
6. No explicit arguments at all

Out of these six choices, is there a clear winner? As much as I want to pick one,
each of them have at least one downside that can prove to be annoying in practice. Perhaps this
isn't surprising, as all six methods accomplish the goal of specifying a type `a`
in a rather indirect fashion. In an ideal world, I would be able to just say this:

{% highlight haskell %}
metaData a
{% endhighlight %}

That is, I would like to be able to just pass the type `a` as a plain old argument
to `metaData` without any additional fuss. While GHC certainly can't do this today,
this might be possible in a future version of GHC where terms and types can be
intermixed like this. If we had the syntax for Dependent Haskell quantifiers from
[this GHC proposal](https://github.com/ghc-proposals/ghc-proposals/pull/102),
we could imagine `metaData` having this type:

{% highlight haskell %}
metaData :: forall a -> C a => Blah
{% endhighlight %}

This is almost the same type as `forall a. C a => Blah`, except that `a` is an
explicit argument to `metaData`. This type signature would eliminate many of the
drawbacks of other approaches:

* No proxies (or other strange types) need to be passed, just `a`.
* There's no way to pattern-match on `a`, since `forall` is an _irrelevant_
  quantifier (see
  [the proposal](https://github.com/goldfirere/ghc-proposals/blob/26b2391b85252aa7e9c65e399aff65b37febe5e2/proposals/0000-pi.rst#id4)
  for a more detailed explanation of what this means).
* If you accidentally forget to supply `a` as an argument, you'll get a decent
  error message, since `a` is an explicit argument.
* It should be compatible with `GeneralizedNewtypeDeriving`.

This sounds great in theory, although imagining how to give `metaData` this
type might prove to be challenging in practice. Even supposing that we had
Dependent Haskell today, the syntax for type classes is quite rigid. If you
have this:

{% highlight haskell %}
class C a where
  metaData :: ...
{% endhighlight %}

Then regardless of what the rest of the type of `metaData` is, it will always begin
with the following:

{% highlight haskell %}
metaData :: forall a. C a => ...
{% endhighlight %}

Notice that `forall a` has a dot after it, not an arrow, which is what we really
want. Unfortunately, GHC doesn't offer any mechanism to change this. There has been
[one GHC proposal](https://github.com/ghc-proposals/ghc-proposals/pull/148)
put forward which suggests a way to tweak this slightly, but it
remains to be seen if the proposal would permit the sort of type signature I envision for
`metaData` (or if the proposal will even be accepted at all).

OK, that's enough speculation about Dependent Haskell. For now, I hope
this blog post serves as a useful field guide to the various ways one can write
methods like `metaData` and can help inform you if you ever need to craft one of
your own.

-----

[^1]: Actually, it turns out that the `Proxy#` approach doesn't work well with
      `GeneralizedNewtypeDeriving` either, but that's due to a
      [GHC bug](https://ghc.haskell.org/trac/ghc/ticket/16293).

[^2]: In case you're curious, this approach _is_ compatible with `GeneralizedNewtypeDeriving`,
      but only with GHC 8.8 or later, since that features an
      [important bugfix](https://ghc.haskell.org/trac/ghc/ticket/15637).
