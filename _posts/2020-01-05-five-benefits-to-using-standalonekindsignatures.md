---
layout: post
title: Five benefits to using StandaloneKindSignatures
---

GHC 8.10.1 is slated to be released
[soon](https://mail.haskell.org/pipermail/ghc-devs/2019-December/018375.html),
and among the improvements that it offers is the
new `StandaloneKindSignatures` language extension. Standalone kind
signatures (or "SAKS" for short) are like type signatures, except that they
describe type-level declarations instead of term-level values. Here is one
example of a standalone kind signature that describes the kind of a type
synonym:

{% highlight haskell %}
{-# LANGUAGE StandaloneKindSignatures #-}
import Data.Kind

type MyMaybe :: Type -> Type
type MyMaybe a = Maybe a
{% endhighlight %}

Besides type synonyms, standalone kind signatures can also accompany data
types, type families, and type classes:

{% highlight haskell %}
type MyEither :: Type -> Type -> Type
data MyEither a b = MyLeft a | MyRight b

type Not :: Bool -> Bool
type family Not x where
  Not False = True
  Not True  = False

type MyShow :: Type -> Constraint
class MyShow a where
  myShow :: a -> String
{% endhighlight %}

`StandaloneKindSignatures`, originally described in
[this GHC proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0054-kind-signatures.rst),
were implemented in GHC thanks to
[Vladislav Zavialov's](https://twitter.com/int_index)
efforts.
To celebrate Vladislav's work, I will highlight five reasons why
I am excited about `StandaloneKindSignatures` and why you should be too.

# \#1: Kind signatures can be determined at a glance

This may go without saying explicitly, but I think it is worth emphasizing:
a standalone kind signature reveals the kind of something with no additional
fuss. While it's certainly possible to figure out the kind of something
_without_ a standalone kind signature, it sometimes takes some squinting to
do so. For example, look at this type family:

{% highlight haskell %}
type family Ex (x :: c -> d) (y :: a -> b -> c) (z :: a) (w :: b) :: d where ...
{% endhighlight %}

The kind of `Ex` is `(c -> d) -> (a -> b -> c) -> a -> b -> d`, but this
information is obscured somewhat by a layer of syntactic noise [[^1]]. On the
other hand, if `Ex` is written with a standalone kind signature:

{% highlight haskell %}
type Ex :: (c -> d) -> (a -> b -> c) -> a -> b -> d
type family Ex x y z w where ...
{% endhighlight %}

Then no sleuthing is required at all. The kind of `Ex` is now front and center,
just as it was always meant to be. Term-level values have long benefited from
having their type signatures displayed prominently like this, and now
type-level declarations can enjoy the same benefits. In this sense,
`StandaloneKindSignatures` makes the language more uniform.

# \#2: SAKS allow precise control over the order of kind variables

The `TypeApplications` language extension, which debuted in GHC 8.0, allows
for more control over how invisible arguments in types are
instantiated, such as the `forall a.` part of `forall a. a -> a`.
More recently (as of GHC 8.8), `TypeApplications` has extended
to kinds as well. For example, here is a demonstration of visible type
application in a term:

{% highlight haskell %}
foo :: Bool
foo = id @Bool True
{% endhighlight %}

And here is an example of visible _kind_ application that mirrors the
previous example:

{% highlight haskell %}
type Id (x :: a) = x
type Foo = Id @Bool True
{% endhighlight %}

Yes, I am aware that I am not practicing what I preach since I am omitting
standalone kind signatures in the code above. But I have good reason to do
so this one time, since the omission of SAKS is important to demonstrate a
problem. Let's suppose we want to use visible kind applications on a slightly more
complicated type synonym:

{% highlight haskell %}
type Const (x :: a) (y :: b) = x

type Bar = Const True LT
{% endhighlight %}

The kind of `Const` is `forall a b. a -> b -> a`, so if we want to write
out `Bar` with visible kind applications, we should do so like this:

{% highlight haskell %}
type Bar = Const @Bool @Ordering True LT
{% endhighlight %}

On the other hand, what if we wanted the opposite order? That is, what if
we wanted the kind of `Const` to be `forall b a. a -> b -> a` so that we
could write `Const @Ordering @Bool` instead of `Const @Bool @Ordering`?
Unfortunately, the syntax for type synonyms does not make this simple to
accomplish, since we must write the binder for `(x :: a)` before the
binder for `(y :: b)`. This, in turn, means that GHC will always quantify
`a` before `b` in the `forall` part of the kind, since
[GHC orders variables mentioned in kind annotations in left-to-right order](https://downloads.haskell.org/~ghc/8.8.1/docs/html/users_guide/glasgow_exts.html?highlight=typeapplications#ordering-of-specified-variables).
Ugh!

There are various schemes one could use to creatively work around this issue,
but thanks to `StandaloneKindSignatures`, we don't need to be creative at all.
Instead, we can just give `Const` a standalone kind signature with the exact
order of type variables we want:

{% highlight haskell %}
type Const :: forall b a. a -> b -> a
type Const x y = x

type Bar :: Bool
type Bar = Const @Ordering @Bool True LT
{% endhighlight %}

That's it! No clever scheming required.

Besides this small example, there are other, more advanced scenarios where one
may desire finer control over the exact order of variables in a kind. Read
[this comment](https://gitlab.haskell.org/ghc/ghc/issues/11620#note_116690)
for a description of a place where this arises in the context of higher-rank
kinds.

# \#3: SAKS make `deriving` more flexible

Up until this point, I have been ignoring the elephant in the room: what about
data types? That is, do data types really _gain_ anything by adding standalone
kind signatures? For instance, I showed off this example of a data type earlier:

{% highlight haskell %}
type MyEither :: Type -> Type -> Type
data MyEither a b = MyLeft a | MyRight b
{% endhighlight %}

You might reasonably think this example isn't very interesting, since you don't
need `StandaloneKindSignatures` in order to write out the full kind of `MyEither`.
Alternatively, you can accomplish the same thing by using GADT syntax:

{% highlight haskell %}
data MyEither :: Type -> Type -> Type where
  MyLeft  :: a -> MyEither a b
  MyRight :: b -> MyEither a b
{% endhighlight %}

In light of this, is there any reason to reach for `StandaloneKindSignatures`
when defining data types (aside from aesthetic preferences)? I claim that the
answer is yes: there are programs you can write with the
`StandaloneKindSignatures` style that you cannot write as easily with the GADT
style.

Before I back up this claim, I need to take a brief detour into
`deriving`. Here is a simple newtype that uses the `GeneralizedNewtypeDeriving`
extension to derive several instances:

{% highlight haskell %}
import Control.Monad.Reader

newtype MyReaderT r m a = MkMyReaderT (ReaderT r m a)
  deriving (Functor, Applicative, Monad, MonadReader r)
{% endhighlight %}

Aside from the usual `Functor`/`Applicative`/`Monad` trio, there is also a
derived instance of `MonadReader r`. This instance is of particular interest
because it mentions the type variable `r` that was bound by the newtype header
(i.e., the `newtype MyReaderT r m a` part of the newtype).
Remember this `r`, since it will play a vital role shortly.

`MyReaderT` was declared using Haskell98 syntax. What happens if we repeat
this experiment using GADT syntax? Here is one attempt at doing so:

{% highlight haskell %}
newtype MyReaderT :: Type -> (Type -> Type) -> Type -> Type where
    MkMyReaderT :: forall r m a. ReaderT r m a -> MyReaderT r m a
  deriving (Functor, Applicative, Monad, MonadReader r)
{% endhighlight %}

Sadly, this won't typecheck. Here is an abridged version of the error message
you will get if you try this:

{% highlight plaintext %}
error:
    • Couldn't match type ‘a’ with ‘r’
        arising from a functional dependency between:
          constraint ‘MonadReader r (ReaderT a b)’
            arising from the 'deriving' clause of a data type declaration
    • When deriving the instance for (MonadReader r (MyReaderT a b))
{% endhighlight %}

Don't be too intimidated by the mention of functional dependencies, as the cause
of this error is actually quite simple: the `r` in `MonadReader r` is no longer
bound by the newtype header. Indeed, the `r m a` type variable binders in
the Haskell98 version of `MyReaderT` have been replaced by the GADT return
kind, which does not bind any type variables at all. As a result, the `r` in
`MonadReader r` is a _fresh_ `r`, which causes a different (and incorrect)
instance to be derived.

(Note that the `r m a` type variables in `MkMyReaderT :: forall r m a. <...>`
are entirely orthogonal to this discussion, as they only scope over the type of
the `MkMyReaderT` constructor and not the `deriving` clause.
I could have just as well made the
type of `MkMyReaderT` be `forall r' m' a'. <...>` and obtained the same error.)

One way to work around this issue is to compromise and bind just the `r` type
variable in the newtype header:

{% highlight haskell %}
newtype MyReaderT r :: (Type -> Type) -> Type -> Type where
    MkMyReaderT :: forall r m a. ReaderT r m a -> MyReaderT r m a
  deriving (Functor, Applicative, Monad, MonadReader r)
{% endhighlight %}

This typechecks, but it somewhat defeats the point of this exercise, since we
no longer write the entirety of `MyReaderT`'s kind in the GADT return kind.
Indeed, there appears to be somewhat of an awkward tension between using GADT
return types and putting classes like `MonadReader` in a `deriving` clause.

Thankfully, this tension is resolved with the advent of
`StandaloneKindSignatures`, which offers a cleaner way to write
`MyReaderT`:

{% highlight haskell %}
type MyReaderT :: Type -> (Type -> Type) -> Type -> Type
newtype MyReaderT r m a = MkMyReaderT (ReaderT r m a)
  deriving (Functor, Applicative, Monad, MonadReader r)
{% endhighlight %}

Alternatively, you can do the same thing with GADT syntax:

{% highlight haskell %}
type MyReaderT :: Type -> (Type -> Type) -> Type -> Type
newtype MyReaderT r m a where
    MkMyReaderT :: forall r m a. ReaderT r m a -> MyReaderT r m a
  deriving (Functor, Applicative, Monad, MonadReader r)
{% endhighlight %}

The `StandaloneKindSignatures` approach combines the best of both worlds. We
can write the full kind of `MyReaderT` (which was previously
something that could only be done with GADT return kinds) while simultaneously
binding the `r` used in the `deriving` clause.

# \#4: SAKS make kind inference more predictable

Can you spot the differences between the following two data types?

{% highlight haskell %}
data D1 :: forall k. k -> Type where
  MkD1 :: D1 Int -> D1 a

data D2 :: k -> Type where
  MkD2 :: D2 Int -> D2 a
{% endhighlight %}

Two obvious differences are the names (`D1`/`D2` and `MkD1`/`MkD2`) and the
fact that `D1`'s kind explicitly binds `k` with a `forall`, whereas `D2`'s
kind does not. But there's actually an even more insidious difference between
the two: `D1` typechecks but `D2` does not! Here is the error message you get
if you attempt to compile `D2`:

{% highlight plaintext %}
error:
    • Expected kind ‘k’, but ‘Int’ has kind ‘Type’
    • In the first argument of ‘D2’, namely ‘Int’
      In the type ‘D2 Int’
      In the definition of data constructor ‘MkD2’
{% endhighlight %}

Eek! How could explicitly quantifying the `k` change whether it typechecks or
not? It turns out that GHC typechecks `D1` and `D2` using two completely
different algorithms, and GHC picks which algorithm to use based on subtle
syntactic clues in the data type declaration. (Yes, this is confusing. I
promise that this story will end in a less confusing place than where it
started.)

I will now briefly describe these two algorithms. I will introduce them by
first using the terminology of term-level values, and later I will return to the
case of type-level declarations like `D1` and `D2` (which are more awkward).
The two algorithms are:

1. If a value has a complete type signature [[^2]], generalize the type and
   check the body of the value against that type.
2. Otherwise, use the body of a value to infer its type.

To use concrete examples, consider the code below:

{% highlight haskell %}
f1 :: [a] -> [a]
f1 = reverse

f2 = reverse
{% endhighlight %}

GHC will use Algorithm 1 to check that `f1`'s body has the type `[a] -> [a]`.
GHC will use Algorithm 2 to infer that `f2`'s type is `[a] -> [a]`.

From a distance, it might seem that Algorithm 2 is more robust than Algorithm 1,
since it can synthesize the type signature using nothing but the body of the
value. There is a catch, however: there are certain values for
which Algorithm 2 cannot infer their types, but Algorithm 1 can successfully
check the types of the values against their bodies. An example of this phenomenon
is a _polymorphically recursive_ function, such as the one illustrated in the
example below:

{% highlight haskell %}
data Nested a
  = Epsilon
  | a :<: Nested [a]

nestedLength :: Nested a -> Int
nestedLength Epsilon    = (0 :: Int)
nestedLength (x :<: xs) = 1 + nestedLength xs
{% endhighlight %}

`nestedLength` is a polymorphically recursive function because in the case for
`(:<:)` it invokes a recursive call at type `Nested [a]` rather than type
`Nested a`. The fact that the type parameter to `Nested` changes in the recursive
invocation is the defining characteristic of polymorphic recursion.

Because we gave a type signature to `nestedLength`, GHC uses Algorithm 1 to typecheck it.
If we omitted the type signature, then GHC would use Algorithm 2, which would
fail with the following error:

{% highlight plaintext %}
error:
    • Occurs check: cannot construct the infinite type: a ~ [a]
      Expected type: Nested [a] -> Int
        Actual type: Nested a -> Int
{% endhighlight %}

We can't really blame GHC for not inferring `nestedLength`'s type here.
In general, type inference in the presence
of polymorphic recursion is
[undecidable](https://doi.org/10.1145%2F169701.169692), so there is no algorithm
that could infer the type of every possible polymorphically recursive
function. Luckily, the workaround is simple: just add a type signature.

Why am I blathering on about polymorphic recursion? Recall the definitions of
`D1` and `D2` from before:

{% highlight haskell %}
data D1 :: forall k. k -> Type where
  MkD1 :: D1 Int -> D1 a

data D2 :: k -> Type where
  MkD2 :: D2 Int -> D2 a
{% endhighlight %}

It turns out that these data types are _also_ polymorphically recursive, since
their data constructors each have a recursive occurrence of `D1`/`D2` where the type
parameter is instantiated to `Int` instead of `a`. Therefore, kind-checking
these data types will likely require the use of Algorithm 1 in order to work.
But what exactly does Algorithm 1 mean in the context of data types?

Let's try to adapt Algorithms 1 and 2 to work over type-level declarations.
Here is a rough draft:

1. If a declaration has a ??? kind signature, generalize the kind
   and check the body of the value against that kind.
2. Otherwise, use the body of a declaration to infer its kind.

There is one problem remaining: how should GHC choose between Algorithm 1 and
2? That is, what should "???" stand for?
For most of GHC's existence there was not an obvious answer
to this question, as some type-level declarations have more kind information than
others. For example, `data T (a :: Bool) b = ...` declares the kind of its first
parameter but not its second. We could try using Algorithm 1 on it, but that
would generalize the kind to `forall k. Bool -> k -> Type`, which might actually
be _too_ general. (Imagine that one of `T`'s constructors uses `Maybe b`, for
instance, which requires that `(b :: Type)`.)

The approach that GHC ended up settling on was the notion of
[complete, user-specific kind signatures](https://downloads.haskell.org/~ghc/8.8.1/docs/html/users_guide/glasgow_exts.html#complete-user-supplied-kind-signatures-and-polymorphic-recursion),
or CUSKs for short. A type-level declaration is considered to have a CUSK if
it has enough syntactic information to warrant using Algorithm 1. For instance,
a GADT with an explicit return kind has a CUSK when all kind variables introduced
after the `::` are explicitly quantified. This explains why `D1` typechecks but
`D2` does not. Since `D1` explicitly quantifies `k`, it has a CUSK and therefore
uses Algorithm 1, whereas `D2` does not have a CUSK and therefore uses Algorithm 2,
which cannot handle the polymorphic recursion in its data constructor's type.

This means that the full version of Algorithms 1 and 2 for type-level declarations
are:

1. If a declaration has a CUSK, generalize the kind and check the body of the
   value against that kind.
2. Otherwise, use the body of a declaration to infer its kind.

CUSKs are good enough for GHC, but they are endlessly confusing for users. There
has been a volley of GHC issues filed about kind inference bugs [[^3]] that end
up being caused by users accidentally forgetting to give a polymorphically
recursive declaration a CUSK. Surely there must be a better alternative to CUSKs?

Well of course there is: it's `StandaloneKindSignatures`! This extension gives
us a much simpler specification for Algorithms 1 and 2:

1. If a declaration has a standalone kind signature, generalize the kind and
   check the body of the value against that kind.
2. Otherwise, use the body of a declaration to infer its kind.

The only difference is that I replaced "CUSK" with "standalone kind signature",
but this is a critical difference. GHC users have already been trained to add
a signature when the type inference engine isn't smart enough to infer a type
for a value, and now the exact same training carries over to type-level
declarations as well. Hooray!

It's worth noting that the CUSK versions of Algorithms 1 and 2 are
still the default in GHC 8.10.1. This behavior is controlled by the `CUSKs`
language extension (also introduced in 8.10.1), and enabling `NoCUSKs` will
cause the standalone kind signature versions of Algorithms 1 and 2 to be used
instead. (The `StandaloneKindSignatures` extension implies `NoCUSKs`.) A
future version of GHC will likely switch the default from `CUSKs` to `NoCUSKs`.

# \#5: SAKS may help fix GHC bugs in the future

If reasons #1-#4 above weren't enough to convince you that
`StandaloneKindSignatures` are the bee's knees, then just wait: there may
be even more good things to come in a future version of GHC. That's
because `StandaloneKindSignatures` are a prerequisite to fix
[GHC#12088](https://gitlab.haskell.org/ghc/ghc/issues/12088), a nasty bug
that prevents certain type-level programs from typechecking. One manifestation
of #12088 is that it is unreasonably difficult to use type families in the
kinds of other type families, as evidenced by the fact that
[this program](https://gitlab.haskell.org/ghc/ghc/issues/15987)
does not typecheck:

{% highlight haskell %}
type FooKind :: Type -> k
type family FooKind a

type FooType :: forall (a :: Type) -> FooKind a
type family FooType a

type A :: Type
data A

type instance FooKind A = Type
type instance FooType A = Int
{% endhighlight %}
{% highlight plaintext %}
error:
    • Expected kind ‘FooKind A’, but ‘Int’ has kind ‘Type’
    • In the type ‘Int’
      In the type instance declaration for ‘FooType’
{% endhighlight %}

This bizarre error—in which `FooKind A` fails to evaluate to `Type`—is a
result of GHC not doing _strongly-connected component_ (SCC) analysis properly
on type-level declarations. In the example above, the `FooType A = Int`
instance depends on the `FooKind A = Type` instance, so a proper SCC analysis
would process the latter instance _before_ the former one. Because of GHC#12088,
however, this does not happen, so these instances get processed out of
dependency order.

Term-level values also undergo SCC analysis, and a crucial part of making that
analysis work correctly is taking type signatures into account during the
analysis. By analogy, having standalone kind signatures available will make
implementing SCC analysis for type-level declarations much easier.

-----

[^1]: Figuring out the kind of something without a standalone kind signature
      can be even gnarlier when
      [visible dependent quantification](../../../../2019/03/15/visible-dependent-quantification-in-haskell/)
      is involved.

[^2]: That is, if a value `f` has a type signature `f :: t`, where `t` contains
      no `PartialTypeSignatures` wildcards.

[^3]: These issues include
      [here](https://gitlab.haskell.org/ghc/ghc/issues/12928),
      [here](https://gitlab.haskell.org/ghc/ghc/issues/10141),
      [here](https://gitlab.haskell.org/ghc/ghc/issues/13109),
      [here](https://gitlab.haskell.org/ghc/ghc/issues/13761),
      [here](https://gitlab.haskell.org/ghc/ghc/issues/13365),
      [here](https://gitlab.haskell.org/ghc/ghc/issues/14207),
      [here](https://gitlab.haskell.org/ghc/ghc/issues/14139), and
      [here](https://gitlab.haskell.org/ghc/ghc/issues/11498).
