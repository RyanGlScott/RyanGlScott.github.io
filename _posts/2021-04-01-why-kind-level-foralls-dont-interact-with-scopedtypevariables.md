---
layout: post
title: Why kind-level foralls don't interact with ScopedTypeVariables
---

If you write enough Haskell programs, it's quite likely that you'll want to
define a function whose body mentions a type variable that is bound by its
type signature. Here is a very simple example:

{% highlight haskell %}
f :: a -> Maybe a
f x = Just (x :: a)
{% endhighlight %}

In Haskell 2010, this would be an error, since the type variable `a` in
`x :: a` is distinct from the `a` in `a -> Maybe a` [[^1]]. This aspect of
Haskell 2010 is somewhat annoying, and while there are ways to work around it in
Haskell 2010 itself, they are somewhat indirect. GHC features a more direct way
to write this sort of program: the `ScopedTypeVariables` language extension.
With `ScopedTypeVariables` enabled, one can instead write this:

{% highlight haskell %}
f :: forall a. a -> Maybe a
f x = Just (x :: a)
{% endhighlight %}

Two things have changed. The first change is syntactic: the type
signature for `f` now uses an explicit `forall` to bind `a`. The second change
is semantic: because the type of `f` begins with a `forall`, it signals to GHC
that `a` should be brought into scope over the body of `f`. As a result, the
`a` in `x :: a` is exactly the same `a` as was bound in the type signature.

Besides type signature declarations, `ScopedTypeVariables` also interacts with
other forms of types as described in
[this section of the GHC User's Guide](https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/exts/scoped_type_variables.html#extension-ScopedTypeVariables).
One part of GHC that `ScopedTypeVariables` does not interact with, however, is
kind-level `forall`s. For example, suppose you wanted to write the type synonym
equivalent of `F` using `StandaloneKindSignatures` [[^2]]:

{% highlight haskell %}
type F :: forall a. a -> Maybe a
type F x = Just (x :: a)
{% endhighlight %}

Somewhat surprisingly, GHC does not accept this program:

{% highlight plaintext %}
error: Not in scope: type variable ‘a’
  |
8 | type F x = Just (x :: a)
  |                       ^
{% endhighlight %}

It's not obvious why this is the case, as this goes against the intuitions that
most GHC programmers develop when using `ScopedTypeVariables`. In this post, I
will explain this discrepancy between kind-level `forall`s and other forms of
`forall`. As it turns out, a key part of the story is that certain type-level
declarations, such as type synonyms and type families, have _arities_. I
will explain the concept of arity as I go, but you may wish to read an
[earlier blog post of mine](../../../../2019/05/26/on-the-arity-of-type-families)
for a more in-depth treatment of arity.

# How `ScopedTypeVariables` works

In order to explain why `ScopedTypeVariables` does or does not interact with
certain `forall`s, I first need to establish how `ScopedTypeVariables` works
in the first place. Let's take another look at this program:

{% highlight haskell %}
f :: forall a. a -> Maybe a
f x = Just (x :: a)
{% endhighlight %}

Earlier, I claimed that the `a` in `x :: a` is exactly the same `a` as was
bound in the type signature, but this statement glossed over some important
details. Note that the type signature for `f` is a completely separate
declaration from the body of `f`. How, then, can `a` be used in the body of
`f` if `a` is bound in a different declaration?

The answer lies in how GHC compiles `f`. When GHC compiles Haskell code, it
turns it into a typed intermediate language called Core. We can see what
`f` looks like in Core by compiling it with a handful of GHC flags:

{% highlight plaintext %}
$ ghc Foo.hs -ddump-simpl -fmax-simplifier-iterations=0 -dsuppress-uniques -dsuppress-module-prefixes -dsuppress-idinfo
[1 of 1] Compiling Foo              ( Foo.hs, Foo.o )

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 11, types: 10, coercions: 0, joins: 0/0}

-- RHS size: {terms: 4, types: 4, coercions: 0, joins: 0/0}
f :: forall a. a -> Maybe a
f = \ (@a) (x :: a) -> Just @a x
{% endhighlight %}

> For a brief explanation of what each of these flags do, see
> [here](../../../../2019/07/10/the-surprising-rigidness-of-higher-rank-kinds#foralls-in-core).

In Core, `f` binds all of its arguments with an explicit lambda. Not only does
`f` bind the term-level `x` argument in a lambda, it also binds the _type_-level
argument `a`. GHC does not currently allow binding type variables with lambda
expressions in its source syntax [[^3]], so for the time being, the `\ (@a)`
syntax is a feature that is unique to Core.

Also note that the right-hand side of this lambda, `Just @a x`, mentions `a`.
Although we originally wrote `Just (x :: a)` in the source syntax, Core does
not have inline type annotation syntax, so it uses an explicit type application
instead. Crucially, this `Just @a x` expression would not be well formed if `a`
wasn't in scope, and the `\ (@a)` lambda binding is what brings `a`
into scope.

Having taken a detour through Core, let's tie this back to `ScopedTypeVariables`.
In the source syntax, when GHC sees a type signature that begins with an explicit
`forall`, it uses that as a clue that when compiling the body to Core. Namely,
GHC ensures that it lambda-binds a type variable with the same name as the type
variable mentioned in the `forall`. If `ScopedTypeVariables` were disabled,
then GHC will still try to use a lambda when compiling to Core, but the type
variable name would be different. It would be as if GHC attempted to generate the
following Core:

{% highlight plaintext %}
f = \ (@a1) (x :: a1) -> Just @a x
{% endhighlight %}

This would not be a well formed Core expression, as the `a` in `Just @a x` would
not be in scope. This key idea is this: **`ScopedTypeVariables` is syntactic
sugar for specifying which type variables should be lambda-bound in the compiled
Core**.

## `ScopedTypeVariables` and type annotations

The previous example explained `ScopedTypeVariables` by way of type signature
declarations, but there are other places where `ScopedTypeVariables` takes effect
as well. One such place is expressions that have inline type annotations, such
as in the following variation of `f`:

{% highlight haskell %}
f' x = Just ((id @a :: forall a. a -> a) x)
{% endhighlight %}

This time, the type variable `a` is brought into scope through the type
annotation `:: forall a. a -> a`, which annotates the expression `id @a`.
This is approximately what the Core for `f'` would look like [[^4]]:

{% highlight plaintext %}
f' = \ (@p) (x :: p) -> Just @p ((\ (@a) -> id @a) @p x)
{% endhighlight %}

Just as before, `a` is bound by a lambda expression, ensuring that the name
of the type variable matches `id @a`, the body of the lambda. Although this
type signature only annotates a subexpression of `f'`, the same principles
apply as for top-level type signature declarations.

# Binding kind variables in type-level declarations

Now that we have a solid understanding of how `ScopedTypeVariables` works in
term-level declarations, let's turn our attention to type-level declarations,
such as type synonyms, type families, classes, and data types. In order to
understand what makes type-level declarations so challenging from a
`ScopedTypeVariables` perspective, we need to take a brief detour to explore
how type-level entities are represented in Core. To that end, let's take a
closer look at the `F` type synonym from before:

{% highlight haskell %}
type F :: forall a. a -> Maybe a
type F x = Just x
{% endhighlight %}

The kind of `F` quantifies over the kind variable `a`. To see how the body
of `F` binds `a`, we can make use of GHCi's `:info` command in combination with
the `-fprint-explicit-kinds` flag, which shows all visible kind applications
explicitly:

{% highlight plaintext %}
λ> :info F
type F :: forall a. a -> Maybe a
type F @a x = 'Just @a x :: Maybe a
{% endhighlight %}

This particular syntax, where `F @a` binds the type variable `a` in the body
of `F`, is not available in GHC's source syntax. However,
this is more-or-less what `F` looks like in Core. Note that the Core for
type-level declarations, such as `F`, is slightly different than the Core
for term-level declarations, such as `f`. Recall the Core for `f`:

{% highlight plaintext %}
f :: forall a. a -> Maybe a
f = \ (@a) (x :: a) -> Just @a x
{% endhighlight %}

`f` binds all of its arguments with a lambda, whereas `F` does not. This is
because unlike in terms, there are no lambdas at the type level. While it would
be theoretically possible to implement type-level lambdas, it would bring
additional complications to the way that GHC's type inference works [[^5]].
As a result, we won't consider type-level lambdas for the remainder of this
post.

Instead of using lambdas, type-level declarations bind a fixed number of
arguments directly. For example, `type F @a x = ...` binds two arguments:
`a` and `x`. In GHC parlance, `a` is an _invisible_ argument and `x` is
a _visible_ argument, which is why `a` is bound with an `@`-sign.

At first glance, this way of binding arguments may not seem that different
from what term-level functions do, but this has significant ramifications for
type synonyms and type families. Unlike with term-level functions, which can
always be partially applied, type synonyms and type families must always be
applied to at least as many arguments that they bind in their definitions.
For example, GHC would reject the following program:

{% highlight plaintext %}
type Proxy :: forall k. k -> Type
data Proxy a = Proxy

g :: Proxy F
g = Proxy
{% endhighlight %}

This is because `F` binds two arguments, so it is only well formed when it is
applied to at least two arguments. `Proxy F` does not meet this criterion, as
`F` is not applied to enough arguments, so GHC reject it. The minimum number of
arguments that a type synonym or type family must be applied to is referred to
as its _arity_.

Because of GHC's arity restrictions, the way that type-level declarations bind
their arguments are of semantic importance. For instance, note that there
are three distinct ways of defining `F`. There is the original, arity-2
definition of `F`:

{% highlight haskell %}
type F x = Just x
-- In Core:
-- type F @a x = Just @a x
{% endhighlight %}

There is also an arity-1 version of `F`:

{% highlight haskell %}
type F = Just
-- In Core:
-- type F @a = Just @a
{% endhighlight %}

Finally, there is an arity-0 version of `F` that requires a kind annotation on
the right-hand side to define:

{% highlight haskell %}
type F = Just :: forall a. a -> Maybe a
-- In Core:
-- type F = Just
{% endhighlight %}

Note that in general, GHC only allows defining arity-0 things by defining
an equation where the right-hand side has a kind annotation with all
`forall`s written explicitly. If the user had instead written
`type F = Just :: a -> Maybe a`, with no `forall`, then it would have arity 1.

Unlike the arity-2 version of `F`, both the arity-1 and arity-0 versions of `F` would
be permitted in `g :: Proxy F`, as it would compile to something resembling
`g :: forall k. Proxy (F @k)` in Core. The arity-0 version can be used in
even more situations than the arity-1 version; for more details, consult my
[earlier blog post](../../../../2019/05/26/on-the-arity-of-type-families) on
the topic.

# Attempting to foist `ScopedTypeVariables` onto kinds

Alright, it's finally time for the main event: what goes wrong if we try to make
`ScopedTypeVariables` work for type-level declarations? When we described how
`ScopedTypeVariables` works in terms, we made use of lambda expressions in
Core. As we saw in the previous section, however, lambdas aren't available at
the type level, so we need a different specification for type-level entities.

In some cases, it seems blindingly obvious how to make `ScopedTypeVariables`
work for kind-level `forall`s. For example, in this version of `F`:

{% highlight haskell %}
type F :: forall a. a -> Maybe a
type F x = Just (x :: a)
{% endhighlight %}

We know that the Core version of `F` will bind the kind variable `@a` on the
left-hand side, so we can argue that `F` should compile to the following Core:

{% highlight plaintext %}
type F :: forall a. a -> Maybe a
type F @a x = Just (x :: a)
{% endhighlight %}

This suggests a possible specification for `ScopedTypeVariables` at the kind
level: a type variable bound by an outermost `forall` in the kind signature
should always correspond to a type variable of the same name being bound in the
left-hand side of the body. Unfortunately, this specification falls apart under
closer scrutiny. Consider this tricky example:

{% highlight haskell %}
type G = Just @a :: forall a. a -> Maybe a
{% endhighlight %}

`G` uses an inline kind annotation, much like the inline type annotations we
saw earlier. Moreover, the kind annotation brings `a` into scope over the
type `Just @a`. Something is very fishy about this program, however. Because of
the right-hand side kind annotation, `G` has arity 0. This means that in
Core, `G` will look something like this:

{% highlight plaintext %}
type G = ...
{% endhighlight %}

Moreover, the right-hand side applies `Just` to `a`:

{% highlight plaintext %}
type G = Just @a
{% endhighlight %}

But now disaster has struck! The right-hand side has an occurrence of the kind
variable `a` without a corresponding binding site on the left-hand side, making
this definition ill formed. It's tempting to try and repair the damage like so:

{% highlight plaintext %}
type G @a = Just @a
{% endhighlight %}

But now `G` has changed from an arity-0 definition to an arity-1 definition. In
other words, its semantics has changed! If compiling a program silently changes
its semantics, then something has gone terribly wrong.

# An alternative to `ScopedTypeVariables`?

As we have just witnessed, `ScopedTypeVariables` doesn't work in the situation
when a type-level declaration binds more kind variables in its kind signature
than its arity permits. This is a rather unsatisfying outcome, however.

For one thing, arity-0 definitions are arguably less common than higher-arity
definitions. One could imagine always bringing kind-level `forall`s into
scope with `ScopedTypeVariables` and reporting a special error message when the
arity is insufficiently small, like in the `G` example above. This has been
proposed in [this GHC issue](https://gitlab.haskell.org/ghc/ghc/-/issues/16635),
but there is currently a lack of consensus on whether this is the right approach.

To make things worse, `ScopedTypeVariables`' non-interaction with kind-level
`forall`s can sometimes lead to confusing behavior. Consider this program:

{% highlight haskell %}
type C :: forall k. k -> Constraint
class C a where
  m :: Proxy k -> Proxy a -> String
{% endhighlight %}

One might be misled into believing that the `k` in `Proxy k` is the same `k`
as in `forall k. k -> Constraint`. But this is not the case! Because kind-level
`forall`s do not interact with `ScopedTypeVariables`, the `k` in `Proxy k` is
completely distinct. It's as if this program had been written:

{% highlight haskell %}
type C :: forall k. k -> Constraint
class C (a :: kindOfA) where
  m :: forall k. Proxy k -> Proxy (a :: kindOfA) -> String
{% endhighlight %}

In order to work around this limitation, one must define `C` with an inline
kind annotation like so:

{% highlight haskell %}
type C :: forall k. k -> Constraint
class C (a :: k) where
  m :: Proxy k -> Proxy a -> String
{% endhighlight %}

As a result, information that was already stated in the standalone kind
signature is now repeated in the body. This isn't so bad in this small example,
but if the kind of `C` were something like
`forall k. REALLY_REALLY_BIG_KIND k -> Constraint`, then it would be annoying
to have to repeat `REALLY_REALLY_BIG_KIND k` again in the body just to bring
`k` into scope.

If `ScopedTypeVariables` isn't the answer to this problem, then what is? Note
that in Core, `C` would look something like this:

{% highlight plaintext %}
type C :: forall k. k -> Constraint
class C @k (a :: k) where
  m :: Proxy k -> Proxy a -> String
{% endhighlight %}

Although GHC's surface syntax does not permit writing `C @k` today,
it might in the future.
[This GHC proposal](https://github.com/ghc-proposals/ghc-proposals/pull/326)
aims to bring Core's `C @k` syntax into the source language. This would offer
the ability to bind type variables in the bodies of type-level declarations
without the tricky interactions involving arity. There are other potential
benefits to this proposal as well [[^6]], but its potential for cleaning up
the awkward corners of GHC described in this post is the primary reason why I
am excited about it.

-----

[^1]: See the [Type Signatures section](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-810004.4.1)
      of the Haskell 2010 Report for more information.

[^2]: If you are not familiar with `StandaloneKindSignatures`, you may find my
      [earlier blog post](../../../../2020/01/05/five-benefits-to-using-standalonekindsignatures)
      on the topic to be informative.

[^3]: At least, not currently. There is an
      [accepted GHC proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst)
      to add the ability to bind type variables in lambdas, however, which would add
      syntax quite similar to the one used in Core.

[^4]: If you compiled `f'` with `-ddump-simpl`, the resulting Core would look
      nearly identical to that of `f`, as GHC would inline the definition of
      `id`. For the sake of this blog post, I'm showing what `f'` would look
      like before optimizations such as inlining.

[^5]: For some examples of how type-level lambdas would complicate type
      inference, see Section 8.1 of the paper
      [_Higher-order Type-level Programming in Haskell_](https://www.microsoft.com/en-us/research/uploads/prod/2019/03/ho-haskell-5c8bb4918a4de.pdf)

[^6]: For example, this GHC proposal is a prerequisite for
      [another proposal](https://github.com/ghc-proposals/ghc-proposals/pull/386)
      that aims to simplify how type family instances are typechecked.
