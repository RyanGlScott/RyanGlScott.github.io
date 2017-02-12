---
layout: post
title: Improvements to deriving in GHC 8.2
---

We're drawing closer to a release of GHC 8.2, which will feature a variety of enhancements to GHC's `deriving`-related extensions. None of the improvements are particularly revolutionary, and for most code, you won't notice a difference. But there are quite a few quality-of-life fixes that should make doing certain things with `deriving` a little less of a hassle.

## Deriving strategies

The largest change to `deriving` that debuts in GHC 8.2 is a new GHC extension: `DerivingStrategies`. Before discussing what `DerivingStrategies` does, let me motivate the problem. Imagine you have this datatype:

{% highlight haskell %}
newtype Foo a = MkFoo a
  deriving Show
{% endhighlight %}

Now suppose you want to derive another instance for `Foo`:

{% highlight haskell %}
class Show a => Bar a where
  bar :: a -> String
  bar = show

deriving instance Bar a => Bar (Foo a)
{% endhighlight %}

How should this derived instance be implemented? Well, if you had `GeneralizedNewtypeDeriving` enabled when compiling it, the derived instance will closely resemble this:

{% highlight haskell %}
instance Bar a => Bar (Foo a) where
  bar (MkFoo a) = bar a
{% endhighlight %}

Alternatively, if you had `DeriveAnyClass` enabled, the derived instance would instead be:

{% highlight haskell %}
instance Bar a => Bar (Foo a)
{% endhighlight %}

causing the default implementation of `bar = show` to kick in.

But what happens if `GeneralizedNewtypeDeriving` and `DeriveAnyClass` are _both_ enabled? Then a problem emerges: `deriving Bar` becomes ambiguous! One could reasonably pick either `GeneralizedNewtypeDeriving` or `DeriveAnyClass` to derive `Bar`, as shown above. And the choice matters, since the result of evaluating:

{% highlight haskell %}
bar (MkFoo 'a')
{% endhighlight %}

will be `'a'` if `GeneralizedNewtypeDeriving` is used, and `MkFoo 'a'` if `DeriveAnyClass` is used.

As it turns out, GHC handles such a scenario by making an arbitrary choice:

{% highlight haskell %}
    * Both DeriveAnyClass and GeneralizedNewtypeDeriving are enabled
      Defaulting to the DeriveAnyClass strategy for instantiating Bar
    * In the stand-alone deriving instance for `Bar a => Bar (Foo a)'
{% endhighlight %}

This is a bit unfortunate, however, because this effectively prevents you from using `GeneralizedNewtypeDeriving` in any module where `DeriveAnyClass` is also enabled. Bummer.

The `DerivingStrategies` extension was created to solve precisely this type of ambiguity. Once the extension is enabled, it extends the syntax of `deriving` clauses and standalone `deriving` declarations slightly, allowing you to augment them with one of three keywords:

* `stock`
* `newtype`
* `anyclass`

These are the "strategies" referred to in `DerivingStrategies`. There are only three for now, although more could conceivably be introduced. Here is an example of each strategy:

### `stock`

`stock` is named because it refers to the "stock" type classes that GHC simply knows how to derive on its own (credit goes to Joachim Breitner for suggesting what to name this). These include the derivable type classes mentioned in the Haskell Report:

* `Bounded`
* `Enum`
* `Ix`
* `Eq`
* `Ord`
* `Read`
* `Show`

They also include the classes that are only derivable in GHC through bespoke language extensions:

* `Functor` (via `DeriveFunctor`)
* `Foldable` (via `DeriveFoldable`)
* `Traversable` (via `DeriveTraversable`)
* `Data` and `Typeable` (via `DeriveDataTypeable`)
* `Generic` and `Generic1` (via `DeriveGeneric`)
* `Lift` (via `DeriveLift`)

So if you write:

{% highlight haskell %}
{-# LANGUAGE DerivingStrategies #-}
newtype Blurggle = MkBlurggle Int
  deriving stock Eq
{% endhighlight %}

This provides an additional guarantee that the derived instance will really be:

{% highlight haskell %}
instance Eq Blurggle where
  (MkBlurggle x) == (MkBlurggle y) = (x == y)
{% endhighlight %}

which can be useful for programmer sanity.

### `newtype`

This strategy indicates that you absolutely want to use `GeneralizedNewtypeDeriving`. To reuse the earlier example, if you wrote:

{% highlight haskell %}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype Foo a = MkFoo a
  deriving Show
  deriving newtype Bar
{% endhighlight %}

Then you'll know that `Bar` will be derived via `GeneralizedNewtypeDeriving`.

This code also demonstrates another feature of deriving strategies: multiple strategies can be used after a data declaration! This part:

{% highlight haskell %}
  deriving Show
  deriving newtype Bar
{% endhighlight %}

tells GHC to derive `Show` with whatever strategy it sees fit (in this case, it defaults to `stock`), and to derive `Bar` specifically with `GeneralizedNewtypeDeriving`. You can also put more than one class after each strategy:

{% highlight haskell %}
newtype Foo a = MkFoo a
  deriving (Read, Show)
  deriving stock (Eq, Ord)
  deriving newtype Bar
{% endhighlight %}

### `anyclass`

Finally, the `anyclass` strategy corresponds to a use of `DeriveAnyClass`. So if you had wrote:

{% highlight haskell %}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
newtype Foo a = MkFoo a
  deriving Show
  deriving anyclass Bar
{% endhighlight %}

Then, you guessed it, it'll derive `Bar` via `DeriveAnyClass`.

---

For more details on the innards of deriving strategies, see the corresponding [GHC Commentary page](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DerivingStrategies).

As an additional fun fact, I was able to use deriving strategies to clean up some of the `base` library. In the [`Foreign.C.Types`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Foreign-C-Types.html) and [`System.Posix.Types`](http://hackage.haskell.org/package/base-4.9.1.0/docs/System-Posix-Types.html) modules, there are a lot of newtypes with slightly unusual `Read` and `Show` instances. They're unusual in the sense that they ignore the newtypes' constructors, which means that `deriving (Read, Show)` couldn't be used to implement these instances. Instead, this ugly hack was used (using the `Show CIntPtr` instance as an example):

{% highlight haskell %}
instance Show CIntPtr where
  show = unsafeCoerce# (show :: HTYPE_INTPTR_T -> String)
{% endhighlight %}

Yuck. Happily, this can be made much cleaner with deriving strategies!

{% highlight haskell %}
deriving newtype instance Show CIntPtr
{% endhighlight %}

## `DeriveAnyClass` overhaul

Before GHC 8.2, `DeriveAnyClass` only worked on for type classes whose argument is of kind `*` or `* -> *`. The reason for this seemingly arbitrary restriction is because GHC made a crude simplifying assumption. If you wrote something like:

{% highlight haskell %}
{-# LANGUAGE DeriveAnyClass #-}
data T f a = MkT (f a)
  deriving C
{% endhighlight %}

Then GHC assumes one of the following cases:

* `C`'s argument is of kind `*`. Then GHC will derive `C` like it was stock-deriving `Eq`. That is, it will generate this instance:

{% highlight haskell %}
instance C (f a) => C (T f a)
{% endhighlight %}

* `C`'s argument is of kind `* -> *`. Then GHC will derive `C` like it was stock-deriving `Functor`. That is, it will generate this instance:

{% highlight haskell %}
instance C f => C (T f)
{% endhighlight %}

If neither case is true, then GHC errors.

This assumption made implementing `DeriveAnyClass` simpler, but it made it quite less general than it could be. What's worse, even though `DeriveAnyClass` was co-opting the code for deriving `Eq` and `Functor` to get the instance contexts right (`C (f a)` and `C f`, respectively), it wasn't even doing that part correctly! For example, consider this code:

{% highlight haskell %}

{% endhighlight %}

## `GeneralizedNewtypeDeriving` and associated type families

TODO

## Poly-kinded `GHC.Generics`

* Poly-kinded `Generic1`

## `DeriveFunctor` now implements `(<$)`

TODO

{% highlight haskell %}
{% endhighlight %}
