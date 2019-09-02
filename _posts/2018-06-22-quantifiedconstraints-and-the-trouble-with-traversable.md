---
layout: post
title: QuantifiedConstraints and the trouble with Traversable
---

_This is the third part in my series of practical applications of the
`QuantifiedConstraints` extension. See also
[part 1](../../../../2018/02/11/how-to-derive-generic-for-some-gadts)
and
[part 2](../../../../2018/03/04/how-quantifiedconstraints-can-let-us-put-join-back-in-monad)._

_In particular, I will be referencing many concepts that were introduced in
[part 2](../../../../2018/03/04/how-quantifiedconstraints-can-let-us-put-join-back-in-monad),
so if you haven't read that post yet, go read it before proceeding further!_

In my
[last blog post](2018/03/04/how-quantifiedconstraints-can-let-us-put-join-back-in-monad),
I explored how the upcoming `QuantifiedConstraints` language extension let us derive
a hypothetical version of `Monad` which includes the `join` function using
`GeneralizedNewtypeDeriving` (GND). The key trick in that post is the use of quantified
constraints of the form:

{% highlight haskell %}
type Representational1 m = forall a b. Coercible a b => Coercible (m a) (m b)
{% endhighlight %}

Which states that `m a` and `m b` are representationally equal (i.e., they can be
converted from one to the other using the `coerce` function) for any types `a` and
`b` such that `a` and `b` are representationally equal. In other words, a
`Representational1 m` constraint represents the idea that `m`'s argument
can be used at a representational role (a fact which is not expressible
through the `RoleAnnotations` syntax).

In this post, I will explore more applications of this `Representational1`
trick. In particular, I will show how `Representational1` lets us derive
two more classes in the `base` library using GND, which would be impossible
without the use of `QuantifiedConstraints`. One of these classes, `Traversable`,
will prove to be challenging to derive without making some sort of
backwards-incompatible change, but I will demonstrate a way to work
around that issue.

# GND-incompatible classes in `base`

`Monad`-plus-`join` wasn't the only class that gave the role system fits.
To my knowledge, there were two other packages from `base` that could no longer
be derived with GND once it switched from using
`unsafeCoerce` to `coerce`:

* `ArrowApply`
* `Traversable`

Let's look at each of these in more detail.

## `ArrowApply`

The less interesting case is `ArrowApply`:

{% highlight haskell %}
class Arrow a => ArrowApply a where
  app :: a (a b c, b) c
{% endhighlight %}

Notice the similarity between the type of `app` and the return type of `join`,
which is `m (m a)`. Both types feature a distinctive,
nested-type-variable application structure. In the case of `join`, it's the variable
`m` that's applied in a nested fashion, and for `app`, it's the type variable `a`.
Just as `join` caused problems for `WrappedMonad`, `app` caused problems for a
wrapper newtype defined in the
[`acme-schoenfinkel`](http://hackage.haskell.org/package/acme-schoenfinkel) package [[^1]]:

{% highlight haskell %}
newtype WrappedSchoenfinkel cat a b
  = WrappedSchoenfinkel { unwrapSchoenfinkel :: cat a b }
  deriving (Category, Arrow, ArrowApply)
{% endhighlight %}

The derived `ArrowApply` instance no longer typechecked once GND switched to `coerce`,
as the typechecker could not conclude that the following two types
were representationally equal:

* `cat (WrappedSchoenfinkel cat a b, a) b`
* `cat (                    cat a b, a) b`

Luckily, the fix for this is relatively straightforward.
We simply need to use `QuantifiedConstraints` to require that a type constructor that
takes two arguments should be representationally roled in its first argument:

{% highlight haskell %}
type RepresentationalInFirstArg c =
  (forall a1 a2 b. Coercible a1 a2
              => Coercible (c a1 b) (c a2 b) :: Constraint)
{% endhighlight %}

Equipped with this constraint, the derived `ArrowApply` instance for
`WrappedSchoenfinkel` becomes:

{% highlight haskell %}
instance (ArrowApply cat, RepresentationalInFirstArg cat)
    => ArrowApply (WrappedSchoenfinkel cat) where
  app :: forall a b. WrappedSchoenfinkel cat (WrappedSchoenfinkel cat a b, a) b
  app = coerce (app :: cat (cat a b, a) b)
{% endhighlight %}

Ta-da! This goes to show that even Acme libraries can provide interesting blog post material.

## `Traversable`

A more intriguing example of breakage comes in the form of the well known `Traversable` class:

{% highlight haskell %}
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
{% endhighlight %}

Like `join` and `app`, the return type of `traverse`, `f (t b)`, has an interesting nesting
structure. If we were to attempt to derive an instance of `Traversable` for
a higher-kinded newtype of a certain shape, such as in the following example:

{% highlight haskell %}
newtype WrappedTraversable t a
  = WrapTraversable { unwrapTraversable :: t a }
  deriving newtype Traversable
{% endhighlight %}

Then GHC's typechecker would be profoundly unhappy. This time, it would complain that
it is unable to conclude that the following types are representationally equal:

* `f (t b)`
* `f (WrappedTraversable t b)`

Unlike the `WrappedSchoenfinkel` breakage, which went unnoticed for some time due to
its relative obscurity, this `WrappedTraversable` breakage was observed while the patch
to make GND use `coerce` was being written. The author of said patch worked around the
issue by forcing `deriving Traversable` for newtypes to use the
`DeriveTraversable` algorithm instead of GND. That workaround is still in place today
in GHC. In fact, the only way I was able to convince GHC to use GND to derive `Traversable`
in the example above was by using an explicit `newtype`
[deriving strategy](https://downloads.haskell.org/~ghc/8.4.3/docs/html/users_guide/glasgow_exts.html?highlight=derivingstrategies#extension-DerivingStrategies)
keyword!

Well, now that we know GND'ing `Traversable` is broken, can we fix it using the
technique we employed for `Monad`-plus-`join` and `ArrowApply`? It's tempting to
think that we can simply write:

{% highlight haskell %}
instance (Traversable t, Representational1 f)
    => Traversable (WrappedTraversable t) where
  traverse :: Applicative f => (a -> f b) -> WrappedTraversable t a
                            -> f (WrappedTraversable t b)
  traverse = coerce (traverse :: (a -> f b) -> t a -> f (t b))
{% endhighlight %}

But GHC will reject this. Why? Because the `Representational f` constraint in the
instance context is a lie. The `f` that appears in the type signature of `traverse`
is actually bound by the method itself, and not in the instance head. If we used
explicit `forall` syntax, the above instance would be:

{% highlight haskell %}
instance forall t f.
      (Traversable t, Representational1 f)
    => Traversable (WrappedTraversable t) where
  traverse :: forall f a b.
              Applicative f => (a -> f b) -> WrappedTraversable t a
                            -> f (WrappedTraversable t b)
  traverse = coerce (traverse :: (a -> f b) -> t a -> f (t b))
{% endhighlight %}

Now it is more evident that the `f` bound in the type signature of `traverse` is
shadowing the `f` bound by the instance head, which means that the
`Representational f` constraint
in the instance context is referring to the wrong `f`.
In fact, there's no way we can possibly refer to the _right_ `f` within
the instance context. Bummer.

This poses an interesting question: what other ways can we tweak things such that
we can impose a `Representational1` constraint on the `f` in `traverse`?
Here are three possible suggestions, in decreasing order of
boldness:

### (a) Make `Representational1` a superclass of `Functor`

In the type of `traverse`, `f` is constrained to be `Applicative`, which
has `Functor` as a superclass. An interesting question one can ask is: should
every `Functor` also be `Representational1`? In other words, if `f` is a `Functor`,
and you know that `Coercible a b` holds for some types `a` and `b`, can you
conclude that `Coercible (f a) (f b)` holds?

I will claim that the answer to this question is "yes", provided that you have
a law-abiding `Functor`. As a very hand-wavey proof, we can write both halves of the
isomorphism that `Coercible (f a) (f b)` induces:

{% highlight haskell %}
to :: (Coercible a b, Functor f) => f a -> f b
to = fmap coerce

from :: (Coercible a b, Functor f) => f b -> f a
from = fmap coerce
{% endhighlight %}

For assurance that `to` and `from` are no-ops at runtime, we can appeal to a `Functor`
law: `fmap id = id`. If we use some fast-and-loose reasoning and claim that `coerce`
is morally equivalent to `id`, then we have that `fmap coerce = coerce`, which
means that `to` and `from` can also be implemented as:

{% highlight haskell %}
to   = coerce -- No-ops,
from = coerce -- as desired
{% endhighlight %}

OK, that's enough hand-waving.

In any case, one might
find it appealing to make `Representational1` a superclass of `Functor`. The
immediate benefit of doing this is that we can now `coerce` underneath any
`Functor`. In particular, this means that if we used GND to produce a `Traversable`
instance for `WrappedTraversable` like so:

{% highlight haskell %}
instance Traversable t => Traversable (WrappedTraversable t) where
  traverse :: Applicative f => (a -> f b) -> WrappedTraversable t a
                            -> f (WrappedTraversable t b)
  traverse = coerce (traverse :: (a -> f b) -> t a -> f (t b))
{% endhighlight %}

Then it typechecks without any additional fuss! We don't even have
to change anything about the `Traversable` class or GND, as GHC simply
discovers a `Representational1 f` constraint by expanding
`Applicative f`'s superclass constraints.

What are the downsides of this approach? The obvious one is that it requires
changing `Functor`—a Haskell98 mainstay whose definition has stood unchanged
for many years—to impose a superclass constraint which makes use of an extremely recent GHC
extension. Changing such an important class in `base` is likely to met with
a healthy amount of skepticism, which is why I haven't seriously entertained
the idea of proposing that this be done.

Another, more surprising consequence of this change is that there would be
`Functor` instances in the wild that would no longer typecheck. For instance,
consider
[this example](https://github.com/ekmett/bifunctors/blob/ffc6b5c0a7fe8f3e1c3603e54a4025f06d7bcfba/src/Data/Biapplicative.hs#L212-L222)
from the `bifunctors` library:

{% highlight haskell %}
data Mag a b t where
  Map :: (x -> t) -> Mag a b x -> Mag a b t
  One :: a -> Mag a b b

instance Functor (Mag a b) where
  fmap = Map
{% endhighlight %}

If `Representational1` were a superclass of `Functor`, then this code would
implode. That's because `Mag` is a GADT that constrains its last type parameter
to be nominally roled (due to the equality occurring in the `One` constructor's
return type). In other words, one can never `coerce` from `Mag a b t1` to
`Mag a b t2`, so a `Representational1 (Mag a b)` constraint is never satisfiable.

There is a silver lining to this breakage, however. If you look closely, you'll
notice that the `Functor (Mag a b)` instance is not law-abiding, since
`fmap id /= id`! To my knowledge, the only other examples of `Functor` instances
that a `Representational1` superclass prevents would also break the `Functor` laws
in a similar fashion. It doesn't rule out all such lawbreakers, but it does catch
a good number of them in a clever way.

### (b) Replace `traverse` with something else

Hm, perhaps changing `Functor` is too rash of an idea. Surely folks would
be alright with only making breaking changes to `Traversable`?
...OK, probably not. But
let's consider what we could do if that _were_ an option.

One thing we could do is throw out the `traverse` method entirely [[^2]]
and replace it with a counterpart that's more amenable to GND. If we
want something that closely resembles `traverse`, we can use the following:

{% highlight haskell %}
  traverseRep :: (Applicative f, Representational1 f)
              => (a -> f b) -> t a -> f (t b)
{% endhighlight %}

`traverseRep` is exactly `traverse` with an additional `Representational1 f`
constraint. This ensures that one can `coerce` into `f`'s argument without
needing to change anything about the `Functor` (or `Applicative`) class
itself.

Alternatively, if you want a variant that doesn't involve `Representational1`
at all, you can use
[this invention](https://ghc.haskell.org/trac/ghc/ticket/13153)
of David Feuer's:

{% highlight haskell %}
  mapTraverse :: Applicative f
              => (t b -> r) -> (a -> f b) -> t a -> f r
  -- mapTraverse p f xs = p <$> traverse f xs
{% endhighlight %}

`mapTraverse` is quite GND-friendly, since no type mentioning `t` ever appears
underneath an application of `f`.

### (c) Add an additional method to `Traversable`

As a middle ground between not changing anything and making breaking changes,
what if we could make `Traversable` benefit from GND without needing to resort
to backwards-incompatible shenanigans? I believe it is possible to have our
cake and eat it, too.

Instead of changing the `traverse` method, I propose to simply add another
method alongside it:

{% highlight haskell %}
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

  traverseRep :: (Applicative f, Representational1 f)
              => (a -> f b) -> t a -> f (t b)
  traverseRep = traverse
{% endhighlight %}

I've chosen `traverseRep` due to its similarity to `traverse`, although we
could just as well have picked `mapTraverse` or some other variant thereof.

Since `Traversable` still has `traverse`, we can't directly use GND to derive
an instance of it for `WrappedTraversable`. But as an alternative, what if we
trained `DeriveTraversable` to be smarter with respect to newtypes? That is,
have `DeriveTraversable` emit the code that it normally does for non-newtypes,
but if we're using `DeriveTraversable` on a newtype, then in addition to generating
the usual code for `traverse`, _also_ generate an
implementation of `traverseRep` that uses `coerce`.

To demonstrate this proposed technique, here is the code that I would imagine
`DeriveTraversable` producing for an instance for `WrappedTraversable`:

{% highlight haskell %}
instance Traversable t => Traversable (WrappedTraversable t) where
  traverse :: forall f a b. Applicative f
           => (a -> f b) -> WrappedTraversable t a -> f (WrappedTraversable t b)
  traverse f (WrapTraversable ta) = WrapTraversable <$> traverse f ta

  traverseRep :: forall f a b. (Applicative f, Representational1 f)
              => (a -> f b) -> WrappedTraversable t a -> f (WrappedTraversable t b)
  traverseRep = coerce (traverseRep :: (a -> f b) -> t a -> f (t b))
{% endhighlight %}

The implementation for `traverse` is entirely standard `DeriveTraversable` fare,
but the `traverseRep` bit is new. The implementation for `traverseRep` is what
GND would have produced, except that we've taken special care to only use
`coerce` in `traverseRep`, not `traverse` (since it wouldn't typecheck in the
latter). In this way, we're bestowing some of GND's powers onto `DeriveTraversable`.

This approach combines the best of both worlds, since now folks can reach for
`traverseRep` if they want the most efficient version, and other folks who want
to stick to Haskel98 can still use `traverse`.

# Takeaways

I've shown how `QuantifiedConstraints` opens the door to `coerce`-ing more things
than ever before and taking GND to new heights. What's more, I've shown how we can
do so in a way that minimizes breakage—or even avoids it entirely.

Have we reached the end of all the cool tricks one can accomplish with
`QuantifiedConstraints`? Of course not! Stay tuned for future blog posts in this
series for more.

-----

[^1]: The `acme-schoenfinkel` package is one of the lesser known libraries authored by
      the late Ertugrul Söylemez. This blog post is dedicated in his memory.

[^2]: To be precise, we'd not only have to throw out the `traverse` method, but also
      the `mapM`, `sequence`, and `sequenceA` methods, which all have a similar
      flavor. But hey, if we're discussing crazy ideas that will probably never happen,
      what is one more crazy idea going to hurt?
