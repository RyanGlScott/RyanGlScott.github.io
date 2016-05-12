---
layout: post
title: What's new with GHC generics in 8.0
---

GHC 8.0 will be released soon, and with it comes many new additions and improvements to the `base` library. In particular, the API in the `GHC.Generics` module (and the underlying machinery provided by the `DeriveGeneric` GHC extension) have undergone quite a few improvements. However, these changes haven't been very well advertised outside of the GHC dev community, so hopefully this blog post will spread awareness of some of the new bells and whistles in GHC generics.

## Type-level metadata

When you type `deriving Generic`, it will spit out a `Generic` instance when compiled. You can observe this for yourself by passing `-ddump-deriv` as an option when compiling. For example, compiling the following code:

{% highlight haskell %}
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics
data Foo = Foo deriving Generic
{% endhighlight %}

with `ghc -ddump-deriv` will spit out something like this in GHC 7.10 and earlier (after some cleanup):

{% highlight haskell %}
instance Generic Foo where
  type Rep Foo = D1 D1Foo (C1 C1_0Foo U1)

  from :: Foo -> Rep Foo x
  from Foo = M1 (M1 U1)

  to :: Rep Foo x -> Foo
  to (M1 (M1 U1)) = Foo
{% endhighlight %}

But that's not the only thing it generates. It also generates some empty datatypes and typeclass instances for those datatypes:

{% highlight haskell %}
data D1Foo
data C1_0Foo

instance Datatype D1Foo where
  datatypeName _ = "Foo"
  moduleName   _ = "Main"

instance Constructor C1_0Foo where
  conName _ = "Foo"
{% endhighlight %}

What's going on here? The reason GHC generics does this is to provide programmers with a datatype's _metadata_. For example, the `Datatype` typeclass gives you the ability to reify the name of a datatype and the module in which it is defined. This can be used when creating generic instances, since the generated `Generic` instance uses `D1Foo` and `C1_0Foo` (the generated datatypes) in `Rep Foo`. For example, [Niklas Hambüchen uses `Datatype`](https://gist.github.com/nh2/1a03b7873dbed348ef64fe536028776d) to come up with a datatype's name for use in an error message:

{% highlight haskell %}
class GTypeName f where
  gtypename :: f a -> String

instance (Datatype c) => GTypeName (M1 i c f) where
  gtypename m = datatypeName m
{% endhighlight %}

But encoding metadata this way has some drawbacks:

* This information can only be accessed at runtime. In particular, there's no way to, say, disallow creating instances of a typeclass for a datatype named `Kerfuffle`, or only allow instances for `newtype`s, at compile-time. The best you could do is check the value of [`datatypeName`](http://hackage.haskell.org/package/base-4.8.2.0/docs/GHC-Generics.html#v:datatypeName) or [`isNewtype`](http://hackage.haskell.org/package/base-4.8.2.0/docs/GHC-Generics.html#v:isNewtype) at runtime, and throw an error if those criteria aren't met.
* There is a measurable compilation performance hit because of this approach. In particular, you have to generate a datatype and typeclass instance for every datatype, constructor, and record selector in your module that has `Generic` derived for it.

Luckily, we can do better. José Pedro Magalhães [devised](https://phabricator.haskell.org/D493) a clever way to encode all of this metadata at the _type_ level using `DataKinds`. The key is to first define new datatypes:

{% highlight haskell %}
data Meta =
    -- Datatypes
    MetaData
      Symbol -- The datatype's name
      Symbol -- The module it's defined in
      Symbol -- The package it's located in
      Bool   -- Is it a newtype?

    -- Constructors
  | MetaCons
      Symbol  -- The constructor's name
      FixityI -- The constructor fixity
      Bool    -- Does it contain record selectors?

    -- Selectors
  | MetaSel
      (Maybe Symbol)     -- The record name (if any)
      SourceUnpackedness -- Whether it was marked {-# UNPACK #-} or {-# NOUNPACK #-}
      SourceStrictness   -- Whether it was given a strict (!) or lazy (~) annotation
      DecidedStrictness  -- What strictness GHC actually decided to use for it

data FixityI = PrefixI
             | InfixI Associativity Nat
{% endhighlight %}

With `Meta`, we can encode all of the properties of a datatype (`MetaData`), constructor (`MetaCons`), or selector (`MetaSel`) that we wish. Now our derived `Generic` instance from earlier will look a little different:

{% highlight haskell %}
instance Generic Foo where
  type Rep Foo = D1 (MetaData "Foo" "Main" "main" False)
                   (C1 (MetaCons "Foo" PrefixI False)
                     U1)

  from :: Foo -> Rep Foo x
  from Foo = M1 (M1 U1)

  to :: Rep Foo x -> Foo
  to (M1 (M1 U1)) = Foo
{% endhighlight %}

The implementations of `from` and `to` are the same, but `D1` and `C1` now use the promoted `Meta` type to represent `Foo`'s metadata. This alleviates the need to generate extra datatypes.

Now comes the amazing part. Before, we had to generate several `Datatype`, `Constructor`, and `Selector` instances for every `deriving Generic` line we used. But now, there are only three such instances we will ever need!

{% highlight haskell %}
instance (KnownSymbol n, KnownSymbol m, KnownSymbol p, SingI nt)
    => Datatype (MetaData n m p nt) where
  datatypeName _ = symbolVal (Proxy :: Proxy n)
  moduleName   _ = symbolVal (Proxy :: Proxy m)
  packageName  _ = symbolVal (Proxy :: Proxy p)
  isNewtype    _ = fromSing  (sing  :: Sing nt)

instance (KnownSymbol n, SingI f, SingI r)
    => Constructor (MetaCons n f r) where
  conName     _ = symbolVal (Proxy :: Proxy n)
  conFixity   _ = fromSing  (sing  :: Sing f)
  conIsRecord _ = fromSing  (sing  :: Sing r)

instance (SingI mn, SingI su, SingI ss, SingI ds)
    => Selector (MetaSel mn su ss ds) where
  selName _ = fromMaybe "" (fromSing (sing :: Sing mn))
  selSourceUnpackedness _ = fromSing (sing :: Sing su)
  selSourceStrictness   _ = fromSing (sing :: Sing ss)
  selDecidedStrictness  _ = fromSing (sing :: Sing ds)
{% endhighlight %}

That's it! We no longer need to generate _any_ auxiliary datatypes or typeclass instances, because the above three instances will work for any possible `Rep` that GHC generates. Don't worry if you don't understand how it's implemented—it uses quite a bit of trickery inspired by the [`singletons`](http://hackage.haskell.org/package/singletons) library to produce values from their type-level equivalents. (The full source for this can be found [here](http://git.haskell.org/ghc.git/blob/6bf0eef74d2b2ce9a48c7acc08ca2a1c0c8a7fbc:/libraries/base/GHC/Generics.hs#l1189).)

Of course, if you wish to you can use this new type-level encoding at compile-time instead of at runtime. For example, Ben Gamari [has defined](https://gist.github.com/bgamari/b67becc433026e80ba2b#file-deepstrict-hs-L22-L35) a type family for determining whether a datatype is strict in all its fields by examining its generic `Rep`. More on this in a bit.

## More metadata

You might have noticed in the definition of `Meta` earlier that there was quite a bit of new information when compared to what `Datatype`, `Constructor`, and `Selector` have in GHC 7.10. That is no coincidence—GHC 8.0 enriches generics with more metadata. Here is a full list of new additions:

### Package names

The `Datatype` class has a new method ([added by Oleg Grenrus](https://ghc.haskell.org/trac/ghc/ticket/10030)):

{% highlight haskell %}
packageName :: t d f a -> String
{% endhighlight %}

As its name suggests, this tells you the name of the package a datatype is defined in. As an example of its utility, you can use `packageName` to [generically define instances](http://hackage.haskell.org/package/lift-generics) for the [`Lift`](http://hackage.haskell.org/package/template-haskell-2.10.0.0/docs/Language-Haskell-TH-Syntax.html#t:Lift) typeclass from `template-haskell`.

### Selector strictness

The `Selector` class has three new methods:

{% highlight haskell %}
selSourceUnpackedness :: t s f a -> SourceUnpackedness
selSourceStrictness   :: t s f a -> SourceStrictness
selDecidedStrictness  :: t s f a -> DecidedStrictness
{% endhighlight %}

where `SourceUnpackedness`, `SourceStrictness`, and `DecidedStrictness` are defined as follows:

{% highlight haskell %}
data SourceUnpackedness = NoSourceUnpackedness
                        | SourceNoUnpack
                        | SourceUnpack

data SourceStrictness = NoSourceStrictness
                      | SourceLazy
                      | SourceStrict

data DecidedStrictness = DecidedLazy
                       | DecidedStrict
                       | DecidedUnpack
{% endhighlight %}

These methods allow you to determine strictness properties of a datatype's fields. `SourceUnpackedness` tells you whether a field is marked with an `{-# UNPACK #-}` pragma, a `{-# NOUNPACK #-}` pragma, or neither. Similarly, `SourceStrictness` tells you whether a field is marked with a strict annotation (a.k.a. a `BangPattern`, or a `!`), a lazy annotation (a `~`, which was introduced due to [Adam Sandberg Ericsson's work on the `-XStrict` extension](https://ghc.haskell.org/trac/ghc/wiki/StrictPragma?version=26#Interactionwithirrefutablepatterns)), or neither.

Whereas `SourceUnpackedness` and `SourceStrictness` reflect what is written in the _source_ code, the actual strictness that GHC _decides_ on for a particular field is slightly more complex, since it takes into account things like [`-funbox-strict-fields`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/options-optimise.html#idp46686511574736) and `-XStrict`. For example, consider the following datatype:

{% highlight haskell %}
data E = ExampleConstructor
           {-# UNPACK #-} !Int
                          !Int
                           Int
{% endhighlight %}

The fields of `ExampleConstructor` will have different `DecidedStrictness` depending on what flags are used to compile GHC:

* If compiled without optimization or other language extensions, then the fields of `ExampleConstructor` will have `DecidedStrict`, `DecidedStrict`, and `DecidedLazy`, respectively.
* If compiled with `-XStrict` enabled, then the fields will have `DecidedStrict`, `DecidedStrict`, and `DecidedStrict`, respectively.
* If compiled with `-O2` enabled, then the fields will have `DecidedUnpack`, `DecidedStrict`, and `DecidedLazy`, respectively.

## Unlifted type representations

Previously, `Generic` couldn't be derived at all for any datatype containing unlifted arguments (e.g., `Int#` or `Double#`). This made GHC generics quite poor in comparison to GHC's other derivable classes (e.g., you can derive `Eq` and `Show` for some unlifted argument types).

To achieve feature parity, GHC generics was enhanced with a new data family for unlifted types. Currently, there are six data instances, corresponding to those unlifted types which at least one other derivable class can handle:

{% highlight haskell %}
data family URec a p

data instance URec (Ptr ()) p = UAddr   { uAddr#   :: Addr#   }
data instance URec Char     p = UChar   { uChar#   :: Char#   }
data instance URec Double   p = UDouble { uDouble# :: Double# }
data instance URec Int      p = UFloat  { uFloat#  :: Float#  }
data instance URec Float    p = UInt    { uInt#    :: Int#    }
data instance URec Word     p = UWord   { uWord#   :: Word#   }

type UAddr   = URec (Ptr ())
type UChar   = URec Char
type UDouble = URec Double
type UFloat  = URec Float
type UInt    = URec Int
type UWord   = URec Word
{% endhighlight %}

Now, the following datatype can have a derived `Generic` instance:

{% highlight haskell %}
{-# LANGUAGE DeriveGeneric, MagicHash #-}
import GHC.Generics
import GHC.Exts
data IntHash = IntHash Int# deriving Generic

===>

instance Generic IntHash where
  type Rep IntHash = D1 (MetaData "IntHash" "Main" "main" False)
                       (C1 (MetaCons "IntHash" PrefixI False)
                         (S1 (MetaSel
                                Nothing
                                NoSourceUnpackedness
                                NoSourceStrictness
                                DecidedLazy)
                           UInt))
  from (IntHash x) = M1 (M1 (M1 (UInt x)))
  to (M1 (M1 (M1 (UInt x)))) = IntHash x
{% endhighlight %}

## Other improvements

Along with major API changes came some other improvements and bugfixes. They include:

* Thanks to Oliver Charles, Ben Gamari, and others, the datatypes in `GHC.Generics` now have [many more typeclass instances](https://ghc.haskell.org/trac/ghc/ticket/9043), including `Enum`, `Bounded`, `Ix`, `Functor`, `Applicative`, `Monad`, `MonadFix`, `MonadPlus`, `MonadZip`, `Foldable`, `Foldable`, `Traversable`, `Generic1`, and `Data`.
* Thanks to Simon Peyton Jones, `DeriveAnyClass` [no longer crashes](https://ghc.haskell.org/trac/ghc/ticket/9968) when used with a multi-parameter typeclass.
* `DeriveAnyClass` [now fills in](https://ghc.haskell.org/trac/ghc/ticket/10361) associated type defaults.

## Things to come

Unfortunately, I wanted to make some more changes to GHC generics before the final 8.0.1 release, but I simply ran out of time. Here are some things to look forward to in future releases:

### Poly-kinded `Generic1`

Previously, the definition of `Generic1` was entirely monomorphic with respect to the kind of its argument:

{% highlight haskell %}
class Generic1 (f :: * -> *) where
  type Rep1 f :: * -> *
  from1  :: f a -> Rep1 f a
  to1    :: Rep1 f a -> f a
{% endhighlight %}

But if you look closely, you'll notice that this is too restrictive! The definition of `Generic1` permits it to range over even more types, which we can achieve with a little bit of `PolyKinds`:

{% highlight haskell %}
class Generic1 (f :: k -> *) where
  type Rep1 f :: k -> *
  from1  :: f a -> Rep1 f a
  to1    :: Rep1 f a -> f a
{% endhighlight %}

Similarly, we can kind-generalize most of the datatypes in the `GHC.Generics` module:

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

With this, we can derive `Generic1` for more datatypes than we could before. For example, Derek Elkins uses GHC generics to automatically define `Authenticated` instances for a datatype that is parameterized over a type that uses `DataKinds` in [this example](https://github.com/derekelkins/ads/blob/5e6167d2aa8a16121caf80187252416200b648f3/Data/Authenticated/Generic.hs#L9).

The above changes are [slated to land](https://ghc.haskell.org/trac/ghc/ticket/10604) in GHC 8.2.

### Generics compilation speed

Unfortunately, recent GHC releases seem to have regressed with respect to how fast it takes to define generic typeclass instances for large datatypes using `DefaultSignatures`. It's suspected that there is some quadratic blowup with respect to code size, and there are currently several GHC Trac tickets ([this](https://ghc.haskell.org/trac/ghc/ticket/5642), [this](https://ghc.haskell.org/trac/ghc/ticket/9630), and [this](https://ghc.haskell.org/trac/ghc/ticket/11415)) about the issue.

There have been several notable Haskell libraries that have been bitten by this issue. Two noteworthy examples are `binary` and `aeson`, both of which use GHC generics and `DefaultSignatures` to allow users to define `Binary` and `ToJSON`/`FromJSON` instances easily. While there is a workaround to alleviate compilation times (see these pull requests for [`binary`](https://github.com/kolmodin/binary/pull/95) and [`aeson`](https://github.com/bos/aeson/pull/335)), it's not a robust solution.

I plan to investigate this compilation speed regression further in the future. If you wish to help, feel free to talk to me (RyanGlScott) on the `#ghc` IRC channel on freenode.

