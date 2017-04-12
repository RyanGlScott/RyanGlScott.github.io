---
layout: post
title: Improvements to deriving in GHC 8.2
---

We're drawing closer to a release of GHC 8.2, which will feature a variety of enhancements to GHC's `deriving`-related extensions. None of the improvements are particularly revolutionary, and for most code, you won't notice a difference. But there are quite a few quality-of-life fixes that should make doing certain things with `deriving` a little less of a hassle.

## Deriving strategies

The largest change to `deriving` that debuts in GHC 8.2 is a new extension: `DerivingStrategies`. Before discussing what `DerivingStrategies` does, let me motivate the problem. Imagine you have this datatype:

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
import GHC.Generics

class TypeName a where
  typeName         :: proxy a -> String
  default typeName :: (Generic a, Rep a ~ D1 d f, Datatype d)
                   => proxy a -> String
  typeName _ = datatypeName $ from (undefined :: a)
{% endhighlight %}

This uses `GHC.Generics` to automatically figure out what the name of a data type is. For instance, here is an example of how you could use it:

{% highlight haskell %}
data T a = MkT a deriving Generic
instance TypeName (T a)

tName :: String
tName = typeName (Proxy :: Proxy (T ())) -- tName == "T"
{% endhighlight %}

So far, so good. But what if we attempted to derive the `TypeName` instance for `T a` using `DeriveAnyClass`?

{% highlight haskell %}
data T a = MkT a
  deriving (Generic, TypeName)
{% endhighlight %}

You might think that GHC would come up with the same instance as the one we wrote manually above:

{% highlight haskell %}
instance TypeName (T a)
{% endhighlight %}

But prior to GHC 8.2, that wasn't true! If you compiled this code with the `-ddump-deriv` flag to see the generated code that GHC derives, you'd discover that the actual instance was this:

{% highlight haskell %}
instance TypeName a => TypeName (T a)
{% endhighlight %}

Huh? This instance has a completely redundant `TypeName a` context! Even worse, `tName` no longer typechecks, since there's no `TypeName` instance for `()`!

This behavior, while totally bonkers, was by design. Recall that `DeriveAnyClass` was using the same algorithm that GHC uses to stock-derive `Eq` instances. That is, because this code:

{% highlight haskell %}
data T a = MkT a deriving Eq
{% endhighlight %}

would generate this instance:

{% highlight haskell %}
instance Eq a => Eq (T a)
{% endhighlight %}

Then as a consequence, `DeriveAnyClass` follows the same pattern in deriving a `TypeName` instance for `T a`. Unfortunately, the approach for deriving `Eq` just doesn't work for a type class like `TypeName`.

It was clear that `DeriveAnyClass` needed a new coat of paint, so GHC 8.2 will debut a new inference algorithm for `DeriveAnyClass`. Unlike, say, deriving `Eq`, which infers the context for its instances by examining the definition of the data type, `DeriveAnyClass` infers its context by examining the _type signatures of the class's methods_. Continuing the `TypeName` example:

{% highlight haskell %}
class TypeName a where
  typeName         :: proxy a -> String
  default typeName :: (Generic a, Rep a ~ D1 d f, Datatype d)
                   => proxy a -> String
  typeName _ = datatypeName $ from (undefined :: a)

data T a = MkT a
  deriving (Generic, TypeName)
{% endhighlight %}

This will generate a `TypeName` instance like this:

{% highlight haskell %}
instance ??? => TypeName (T a)
{% endhighlight %}

GHC determines what `???` is by gathering constraints from the type signatures of `TypeName`'s methods and simplifying them as much as possible. In this example, GHC gathers the constraints:

{% highlight haskell %}
(Generic (T a), Rep (T a) ~ D1 d f, Datatype d)
{% endhighlight %}

GHC is immediately able to discharge all three of these constraints, so this simplifies down to `()`, so the final instance that GHC generates is:

{% highlight haskell %}
instance TypeName (T a)
{% endhighlight %}

Which is exactly what we wanted. Hooray!

Better yet, this new design completely removes the requirement that the derived class's argument must be of kind `*` or `* -> *`, so now `DeriveAnyClass` can be used in far more places than it could before.

I owe a great deal of gratitude to Simon Peyton Jones for patiently explaining the parts of the typechecker needed to implement this feature... and for fixing several mistakes in my initial implementation :)

## `GeneralizedNewtypeDeriving` and associated type families

Prior to GHC 8.2, it was impossible to use `GeneralizedNewtypeDeriving` to derive an instance of this type class:

{% highlight haskell %}
class Marshal a where
  type RepType a
  marshal :: a -> RepType a
  unMarshal :: RepType a -> a
{% endhighlight %}

Or rather, it was impossible for _any_ class with associated type families. But this was rather unfortunate, as implementing `Marshal` instances for newtypes is predictable and laden with boilerplate:

{% highlight haskell %}
newtype Age a = MkAge a
instance Marshal a => Marshal (Age a) where
  type RepType (Age a) = RepType a
  marshal (Age x)      = marshal x
  unMarshal x          = Age x
{% endhighlight %}

So this definitely smells like something that `GeneralizedNewtypeDeriving` should be able to handle. Thankfully, starting with GHC 8.2, that is the case. You can now just write:

{% highlight haskell %}
newtype Age a = MkAge a deriving Marshal
{% endhighlight %}

And it will generate an instance that is equivalent to the manually written one above.

There are a couple of things to watch out for when using this feature, however. One gotcha is that this only works for associated _type_ families, not _data_ families. It doesn't make sense to combine associated data families with `GeneralizedNewtypeDeriving`, because if you tried deriving this:

{% highlight haskell %}
class C a where
  data D a

newtype Age a = MkAge a
  deriving C
{% endhighlight %}

Then what instance would be produced? GHC would have to generate something like this:

{% highlight haskell %}
instance C (Age a) where
  data D (Age a) = ???
{% endhighlight %}

And it is not clear what GHC would fill in for `???`, as creating a data family instance here would require a fresh data constructor. That is to say, data family instances are _generative_, whereas type family instances are not.

Another minor annoyance to watch out for is if you try to derive an instance like this, where the newtype wraps a concrete type (instead of just a type variable, as in `Age` above):

{% highlight haskell %}
class C a where
  type T a

newtype MyInt = MyInt Int
  deriving T
{% endhighlight %}

This is only allowed if `UndecidableInstances` is enabled. Why? That's because the derived instance would be this:

{% highlight haskell %}
instance C MyInt where
  type T MyInt = T Int
{% endhighlight %}

GHC's typechecker isn't smart enough to conclude that reducing `T MyInt` will ever terminate, so it conservatively requires `UndecidableInstances` to allow this. Of course, this requirement does rule out things that would legitimately send the typechecker into a loop—for instance, consider what would happen if you did this!

{% highlight haskell %}
newtype Loop = MkLoop Loop
  deriving C
{% endhighlight %}

## Poly-kinded `GHC.Generics`

If you use `GHC.Generics`, you're probably familiar with the `Generic1` class:

{% highlight haskell %}
class Generic1 (f :: * -> *) where
  type Rep1 f :: * -> *
  from1 :: f a      -> Rep1 f a
  to1   :: Rep1 f a -> f a
{% endhighlight %}

If you squint, you'll notice that the kind of `Generic1` is actually less polymorphic than it could be. We can generalize the kind of `Generic1` to this:

{% highlight haskell %}
class Generic1 (f :: k -> *) where
  type Rep1 f :: k -> *
{% endhighlight %}

In a similar vein, we can kind-generalize most of the datatypes in `GHC.Generics`:

{% highlight haskell %}
data V1 (p :: k)
data U1 (p :: k) = U1
newtype Par1 p = Par1 p
newtype Rec1 (f :: k -> *) (p :: k) = Rec1 (f p)
newtype K1 i c (p :: k) = K1 c
newtype M1 i c (f :: k -> *) (p :: k) = M1 (f p)
data (:+:) (f :: k -> *) (g :: k -> *) (p :: k) = L1 (f p) | R1 (g p)
data (:*:) (f :: k -> *) (g :: k -> *) (p :: k) = f p :*: g p
newtype (:.:) (f :: k2 -> *) (g :: k1 -> k2) (p :: k1) = Comp1 (f (g p))
data family URec a (p :: k)
{% endhighlight %}

(The exception being `Par1`, of course, since its type parameter is forced to be of kind `*`.)

Now we can derive `Generic1` instances for more data types than we could before. For example, Derek Elkins uses `GHC.Generics` to automatically define `Authenticated` instances for a data type that is parameterized over a type that uses `DataKinds` in [this example](https://github.com/derekelkins/ads/blob/5e6167d2aa8a16121caf80187252416200b648f3/Data/Authenticated/Generic.hs#L9).

## `DeriveFunctor` now implements `(<$)`

(This addition was not authored by me, but rather by David Feuer. Thanks, David!)

GHC's `DeriveFunctor` extension grants you the power to easily implement a lawful `Functor` instance for a given datatype. For instance, `data Foo a = Foo a a deriving Functor` would generate the instance:

{% highlight haskell %}
instance Functor Foo where
  fmap f (Foo x y) = Foo (f x) (f y)
{% endhighlight %}

However, there's more to `Functor` than just `fmap`. Here's the fully fleshed-out definition of the `Functor` type class:

{% highlight haskell %}
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  (<$) = fmap . const
{% endhighlight %}

`Functor` also has the somewhat lesser-known method `(<$)`, which replaces locations inside the input with the same value. Notice that in the derived `Functor` instance above, however, GHC didn't implement `(<$)` manually, but relied on the default implementation (`fmap . const`). As it turns out, this default implementation can be very inefficient for certain data structures. Here's an example from the `containers` library:

{% highlight haskell %}
data Tree a = Bin !(Tree a) a !(Tree a)
            | Tip
  deriving Functor
{% endhighlight %}

This produces the following `Functor` instance:

{% highlight haskell %}
instance Functor Tree where
  fmap f (Bin l v r) = Bin (fmap f l) (f v) (fmap f r)
  fmap _ Tip         = Tip
{% endhighlight %}

Using the default implementation of `(<$)` for `Tree`, we end up with this definition:

{% highlight haskell %}
(<$) :: a -> Tree b -> Tree a
(<$) x = fmap (\_ -> x)
{% endhighlight %}

Alas, GHC is unable to optimize this any further, since fmap is defined recursively. (The curious reader is encouraged to read [this](http://git.haskell.org/ghc.git/blob/c347a121b07d22fb91172337407986b6541e319d:/compiler/typecheck/TcGenFunctor.hs#l222) for the full story of why this `(<$)` definition is difficult to optimize.) And this definition is quite unsatisfactory, since this will produce a `Tree` full of thunks of the form `((\_ -> x) y)`, which allocates far more (and leaks way more space) than it should need to.

Luckily, there's a pretty simple fix: just be smarter about deriving `Functor` instances. In GHC 8.2 and later, `DeriveFunctor` will implement `(<$)` in addition to `fmap` to avoid the aforementioned space leaks. For comparison, here is how 8.2 would derive the `Functor Tree` instance above:

{% highlight haskell %}
instance Functor Tree where
  -- fmap is as before
  z <$ Bin l v r = Bin (z <$ l) z (z <$ r)
  _ <$ Tip       = Tip
{% endhighlight %}

Much better!
