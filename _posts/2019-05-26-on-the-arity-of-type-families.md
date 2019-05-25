---
layout: post
title: On the arity of type families
---

Partial application is one of the most useful tools in a Haskell programmer's
toolbelt. Since functions are just data, they can be passed around without
being supplied any arguments, just as one would with any other value. Due to
the ubiquity of higher-order functions in Haskell, the ability to partially
apply things is extremely useful.

Alas, not everything can be partially applied. In particular, GHC offers a
_type families_ extension that lets you define functions for use at the type
level, as in the following example:

{% highlight haskell %}
type family Const a b where
  Const a b = a

i'mAnInt :: Const Int Char
i'mAnInt = 42

i'mAlsoAnInt :: Const Int Bool
i'mAlsoAnInt = 27
{% endhighlight %}

If having to repeatedly type out `Const Int` proves bothersome, then functional
programming wisdom would dictate that we factor it out into its own definition:

{% highlight haskell %}
type family ConstInt where
  ConstInt = Const Int

i'mAnInt :: ConstInt Char
i'mAnInt = 42

i'mAlsoAnInt :: ConstInt Bool
i'mAlsoAnInt = 27
{% endhighlight %}

Surprisingly, however, GHC will bluntly reject the definition of `ConstInt`:

{% highlight plaintext %}
• The type family ‘Const’ should have 2 arguments, but has been given 1
• In the equations for closed type family ‘ConstInt’
  In the type family declaration for ‘ConstInt’
{% endhighlight %}

Oh dear. GHC just threw our usual intuition for partial application out the window,
since the typechecker is quite adamant that `Const` be supplied with no fewer
than two arguments. This means that we must define `ConstInt` like this
instead:

{% highlight haskell %}
type family ConstInt b where
  ConstInt b = Const Int b
{% endhighlight %}

In general, a type family must be supplied with a minimum
number of argument types in order to be considered valid, and that number is
referred to as its _arity_. If a type family has been supplied with enough
arguments to match its arity, then it is a _fully saturated_ type family.

The saturation restriction is certainly nothing new—in fact, it's documented
quite clearly in the
[GHC users' guide](https://downloads.haskell.org/~ghc/8.6.5/docs/html/users_guide/glasgow_exts.html#type-family-declarations):

> We call the number of parameters in a type family declaration, the family’s
> arity, and all applications of a type family must be fully saturated with
> respect to that arity.

However, when I recently attempted to explain to someone what exactly "arity"
means, I realized that defining the notion of arity is trickier than I
expected. The phrase "number of parameters" is a good approximation of arity, but
it misses some subtleties that really only become apparent when one looks at
different examples of interesting type families. My goal in this blog post is
to tease out a more precise definition of arity. I won't get it right on my
first attempt, but we'll get there in the end, I promise!

## Aside 1: Why saturation?

At first blush, you might find the saturation requirement for type families
somewhat baffling. Why can't we just partially apply type families as we can
any other function? The answer ultimately stems from the way GHC's type
inference engine works, and since type families can be used at the type level,
they must be able to co-exist with type inference.

This is better explained with an example, which I've adapted from the wonderful
paper
[_Higher-order Type-level Programming in Haskell_](https://www.microsoft.com/en-us/research/uploads/prod/2019/03/ho-haskell-5c8bb4918a4de.pdf).
Consider the following definition:

{% highlight haskell %}
d :: forall (f :: Type -> Type) (a :: Type) (b :: Type).
     (f a ~ f b) => a -> b
d x = x
{% endhighlight %}

Here, `f a ~ f b` is an _equality constraint_, which informs GHC's constraint
solver that `f a` is the same type as `f b`. In order for the body of `d` to
be well typed, GHC's typechecker must be able to conclude that `a` is the same
type as `b`. Fortunately, this is quite possible. GHC can take the `f a ~ f b`
constraint and _decompose_ it to obtain the simpler constraint `a ~ b`, which
suffices to show that `a` and `b` are the same.

Note that `f` is just an ordinary type variable, so `d` should still typecheck
regardless of whether we instantiate `f` to be `Maybe`, `Either Bool`, or any
other type constructor of kind `Type -> Type`. Question: what happens if we
try to instantiate `f` to be a type family? In particular, consider the
following instantiation of `d`:

{% highlight haskell %}
d' :: (ConstInt Char ~ ConstInt Bool) => Char -> Bool
{% endhighlight %}

Should this work? After all, `ConstInt` is of kind `Type -> Type`,
and `ConstInt Char` does in fact equal `ConstInt Bool` (since both things
reduce to `Int`) so it seems like this is a valid instantiation. Yet
something has gone horribly wrong here. If we decompose
`ConstInt Char ~ ConstInt Bool`, we obtain `Char ~ Bool`, which is completely
unsound!

Fortunately, GHC ensures that such blatant unsafety can never occur by
requiring that `f` only ever be instantiated with _well formed_ types. An
ordinary type
constructor like `Maybe` is well formed without any extra stipulations. Type
families, on the other hand, are only well formed if they are fully saturated.
As a consequence, one cannot instantiate `f` to be `ConstInt`, since `ConstInt`
is only well formed when it is supplied with an argument. This restriction,
heavy-handed as it may be, makes decomposing equalities like `f a ~ f b` always
sound.

Observant readers might wonder if they can subvert this saturation
check by instantiating `f` to be something like `/\b. ConstInt b`, where `/\b`
is a hypothetical syntax for a type-level lambda. The answer is no: neither
the surface syntax nor GHC's internals support such a construct.

# The ins and outs of arity

Since saturation is such an important property to be aware of when using
type families, let's try to nail down what exactly the _arity_ of a type family
is. Here is a crude first attempt:

> **Definition 1**: the _arity_ of a type family is the number of arguments it
> accepts.

Can you see anything wrong with this definition? For starters, it's not
exactly clear what "arguments" means in this context. To illustrate this point,
consider the following examples:

{% highlight haskell %}
type family M0 where
  M0 = Maybe
type family M1 a where
  M1 a = Maybe a
{% endhighlight %}

`M0` and `M1` define the same thing... well, almost. They both have kind
`Type -> Type`, which means that they can each be applied to a single
argument. They differ, however, in their respective arities.
`M0` was defined without any parameters, which gives it arity 0, whereas `M1`
was defined with one parameter, which gives it arity 1. In other words, `M0`
can be partially applied, but `M1` cannot.

This leads us to an important realization: _the kind of a type family doesn't
tell you everything_. In particular, there is a distinction between the
arguments that _must_ be supplied in order to be fully saturate a type family,
and the arguments that can _optionally_ be supplied after a type family is saturated.
For example, it is not strictly required to supply `M0` with
an argument in order to be well formed, but for `M1`, it is mandatory.
This information cannot be gleaned
from the kind of a type family alone, as arity is a _syntactic_ property.

It may be informative to see `M0` and `M1` written with explicit return kinds:

{% highlight haskell %}
type family M0 :: Type -> Type where
  M0 = Maybe
type family M1 (a :: Type) :: Type where
  M1 a = Maybe a
{% endhighlight %}

I'm somewhat fond of this syntax since it suggests a fairly decent intuition
for how to compute arity: simply count the number of parameters to the left of
the double-colon (`::`). In `M1`, one parameter appears to the left of the
`::`, whereas in `M0`, no parameters appear to the left of the `::`.

Now that we've seen this, we can make a better attempt at defining arity:

> **Definition 2**: the _arity_ of a type family is the number of parameters
> that appear before the return kind in its definition.

Note that return kinds can either be explicit
(e.g., `type family Blah ... :: Type -> Type where ...`)
or implicit (e.g., `type family Blah ... where ...`) depending on which style
you prefer. I'll use both forms in various places later in the post.

## Are returns kinds a cheat code?

We have just seen an example of how `type family M0 :: Type -> Type` was
strictly more powerful with respect to partial application than
`type family M1 (a :: Type) :: Type`. It is tantalizing to think that the
trick of putting everything in the return kind can make _every_ type family
partially applicable. For instance, one might try defining this:

{% highlight haskell %}
type family ConstInt :: Type -> Type where
  ConstInt b = Const Int b
{% endhighlight %}

If this worked, we could finally have a version `ConstInt` that we could
partially apply. But GHC is privy to such hijinks and will complain if one
tries to define `ConstInt` this way:

{% highlight plaintext %}
• Number of parameters must match family declaration; expected 0
• In the type family declaration for ‘ConstInt’
{% endhighlight %}

Blast. Because we didn't define any parameters to the left of the return kind,
we are only permitted to define equations that use, well, zero parameters. In
other words, we can only write `ConstInt = ...`, not `ConstInt b = ...`,
`ConstInt b1 b2 = ...`, or something of a similar ilk.

Therefore, while stuffing more arguments into the return kind grants more power
in terms of partial applicability, it also removes the power to define certain
things in the equations of the type famiily. Be aware of this tradeoff when
deciding what shape your type family should have.

# Enter `PolyKinds`

Now that we've sampled a taste of what arity is about, it's time to add some
spice to our examples. In particular, all of the examples we've seen up to
this point involve very simple kinds. Things really become interesting, however,
when _polymorphic_ kinds enter the picture.

Unsurprisingly, we're going to be making heavy use of GHC's `PolyKinds`
language extension from here on out. We're also going to be using some other
language extensions we haven't formally introduced yet, but I'll bring them up
when the situation is appropriate.

## A brief introduction to `PolyKinds`

Just as types can be polymorphic, so too can kinds be polymorphic. Here is
the "hello world" of kind polymorphism, the `Proxy` type:

{% highlight haskell %}
data Proxy (a :: k) = Proxy
{% endhighlight %}

Just as `a` ranges over types, `k` ranges over kinds. For example, here are
some particular instantiations of `Proxy`:

{% highlight haskell %}
Proxy Int
Proxy Maybe
Proxy True -- Using the DataKinds language extension
{% endhighlight %}

Each of those examples instantiates `k` in a different way. Note that unlike
the type argument for `a`, which must be explicitly written by the programmer,
the kind argument `k` is omitted, as it is inferred by the typechecker
automatically. While this saves the
programmer some effort, it can occasionally hide interesting details from view.
If you want to specify what `k` gets instantiated to explicitly, then
the `TypeApplications` extension lets you just that:

{% highlight haskell %}
Proxy @Type           Int
Proxy @(Type -> Type) Maybe
Proxy @Bool           True
{% endhighlight %}

Note that the code above will only work on GHC 8.8 or later, as that is the
first version to support `TypeApplications` syntax at the kind level.

## `PolyKinds` and type families

One can also have poly-kinded arguments in type families, as this example
demonstrates:

{% highlight haskell %}
type family PK (a :: k) :: Type where
  PK Int   = Char
  PK Maybe = Double
  PK True  = Float
{% endhighlight %}

Similarly, one can also use `TypeApplications` syntax to spell out what the
kind argument `k` is in each equation (again, this assumes GHC 8.8 or later):

{% highlight haskell %}
type family PK (a :: k) :: Type where
  PK @Type           Int   = Char
  PK @(Type -> Type) Maybe = Double
  PK @Bool           True  = Float
{% endhighlight %}

## The arity of poly-kinded type families

Question: what is the arity of `PK`? This is a surprisingly tricky question to
answer, and it is one the main reasons why I set out to write this blog post.
As we've seen in previous examples, one can write type family equations for
`PK` with either one or two arguments, depending on whether `TypeApplications`
is used or not. As a result, it's not exactly clear whether the arity of `PK`
is 1 or 2.

One thing we can say with certainty is that the arity is _at least_ 1.
That much is evident by the fact that GHC is content with this definition:

{% highlight haskell %}
type family PK1 (a :: k) where
  PK1 a = PK a
{% endhighlight %}

But it is not happy with this one:

{% highlight haskell %}
type family PK0 where
  PK0 = PK
{% endhighlight %}

{% highlight plaintext %}
• The type family ‘PK’ should have 1 argument, but has been given none
• In the equations for closed type family ‘PK0’
  In the type family declaration for ‘PK0’
{% endhighlight %}

We have seen this error message before when attempting to apply a type family
to a number of arguments less than its arity. In those situations, the `X` in
"`should have X argument(s)`" has always happened to be the arity of the type
family in question. For now, let's trust this assumption and operate under
the premise that `PK` has arity 1.

Having seen that, what happens if we push the kind variable `k` into the
return kind? You end up with something like this:

{% highlight haskell %}
type family PKRetKind :: k -> Type
{% endhighlight %}

Since the argument of kind `k` is now part of the return kind, we can no
longer match against it in any type family equations, so something like
`PKRetKind Int = Char` is now out of the realm of possibility. (As a
consequence, I've simply defined `PKRetKind` without any equations.)
On the flip side,
we can partially apply `PKRetKind`, so it would be quite permissible to
define `PK0 = PKRetKind`. It seems like `PKRetKind` has arity 0, so
again, let's operate under this premise (at least, for the time being).

## Just because you can't see it doesn't mean it's not there

Hopefully you saw through my hand-waving in the previous section and realized
that something is amiss here. Recall that we were able to define
equations like `PK @Type Int = Char`. When we spell out `@Type` like this,
it suggests that `PK` is actually accepting another argument besides `Int`.

Indeed, from this perspective, it is not inaccurate to say that
`PK` accepts _two_ arguments: one invisible (`k`) argument and
one visible (`a`) argument.
We don't normally account for the invisible argument, since GHC usually figures
it out for us, but it is still there lurking behind the scenes. You
can draw it out of its hiding place with the `@` syntax, which is sometimes
referred to as a "visibility override".

For example, consider what happens the following code:

{% highlight haskell %}
type Comp (f :: b -> c) (g :: a -> b) (x :: a) = f (g x)
type Ex = Comp PKRetKind PKRetKind True
{% endhighlight %}

If you follow the type synonyms, the type `Ex` eventually reduces down to
`PKRetKind (PKRetKind True)`, or `PKRetKind @Type (PKRetKind @Bool True)`
with kind arguments spelled out. Even though `PKRetKind` appeared to be used
without any arguments in `Ex`, in actuality, GHC knew about the invisible
`@Type` and `@Bool` arguments well in advance. You could just have well written
this:

{% highlight haskell %}
type Ex = Comp (PKRetKind @Type) (PKRetKind @Bool) True
{% endhighlight %}

And have achieved the same end result.

## Breaking the ranks

Most of the time, the details of invisible arguments can be completely ignored,
since they're usually a bookkeeping detail that GHC's type inference engine
accounts for. But in the case of type family arities, I argue that invisible
arguments are actually _essential_ to keep in mind, as invisible arguments can
be the difference between whether or not a type family is well formed or not.

To see what I mean, let's consider this intriguing definition:

{% highlight haskell %}
type HRK (f :: forall k. k -> Type) = (f Int, f Maybe, f True)
{% endhighlight %}

What is interesting about `HRK` is that its argument has a
_higher-rank kind_, `forall k. k -> Type`. The explicit `forall` at the
front (made possible with the `RankNTypes` language extension) indicates that
`f` can only be instantiated with types that are sufficiently polymorphic in
`k`. For instance, one cannot instantiate `f` to be `Maybe` since the kind of
`Maybe`, `Type -> Type`, is not polymorphic enough. It is a good thing that
this is the case, lest we end up with the nonsensical types `Maybe Maybe`
or `Maybe True`!

One example of something that we _can_ instantiate `f` with is `Proxy`, whose
kind is `forall k. k -> Type`. As we have seen earlier, `Proxy Int`,
`Proxy Maybe`, and `Proxy True` are all perfectly valid types, so this is a
fine thing to do.

On the other hand, something that we _cannot_ instantiate `f` with is
`Proxy @Type`. This is because the kind of `Proxy @Type` is no longer
`forall k. k -> Type`, but rather `Type -> Type`, which is not polymorphic
enough to be used in a higher-rank situation.

OK, time for a quiz. Can we instantiate `f` with `PKRetKind`? It seems like
we ought to be able to do this, since the kind of
`PKRetKind`, which is also `forall k. k -> Type`, appears to be polymorphic
enough to be
used in a higher-rank context. What happens if we try to use the type
`HRK PKRetKind`? We get this:

{% highlight plaintext %}
• Expected kind ‘forall k. k -> Type’,
    but ‘PKRetKind’ has kind ‘k0 -> Type’
• In the first argument of ‘HRK’, namely ‘PKRetKind’
  In the type ‘HRK PKRetKind’
{% endhighlight %}

I was deeply surprised to discover that GHC rejected this when I tried it
for the first time. Moreover, the rather inscrutable error message didn't
exactly make clear where I had gone wrong. What I didn't realize at the time
was that the arity of `PKRetKind` was actually the root cause.

## Return kinds, revisited

Let's take another look at the definition of `PKRetKind`, this time with a
critical eye:

{% highlight haskell %}
type family PKRetKind :: k -> Type
{% endhighlight %}

Earlier, I claimed that its kind was `forall k. k -> Type`. But where did this
`forall k` quantifier come from? It's certainly not written out in the return
kind, so that seems to indicate that the `k` is being implicitly quantified somewhere.

My gut instinct was to think that I wasn't being explicit enough. I tried
this seemingly-equivalent definition of `PKRetKind`:

{% highlight haskell %}
type family PKRetKind' :: forall k. k -> Type
{% endhighlight %}

When I did this, `HRK PKRetKind'` now worked without issue! This was great, but
something felt off about the whole thing. If the two formulations of
`PKRetKind` were equivalent, then why did only one of them work when
used as the argument to `HRK`?

Well, it turns out my original assumption was wrong: the two definitions actually
_aren't_ equivalent. That's because the first version of `PKRetKind` has a
secret power: one can match on `k` in type family equations! That is to say,
it can support things like this:

{% highlight haskell %}
type family PKRetKind :: k -> Type where
  PKRetKind @Type = Maybe
  PKRetKind @k    = Proxy
{% endhighlight %}

If `k` is `Type`, then the first equation is matched, which reduces to `Maybe`
(of kind `Type -> Type`); otherwise, it falls through to the second equation,
which reduces to `Proxy` (of kind `k -> Type`). However, this seems to fly in the face
of the earlier intuition we developed about type family arities, which states
that only things that appear to the left of the `::` contribute toward the
arity (and, as a result, are permitted to appear on the left-hand sides of
equations). How can these two facts be reconciled?

## It's kind of important

As it turns out, `k` actually _does_ appear to the left of the `::`—you just
can't see it under most circumstances. To be more precise, `k` is implicitly
quantified, not as a part of
the return kind, but as a parameter to `PKRetKind` itself. You can see this
for yourself by using the `:info` command in GHCi with the
`-fprint-explicit-kinds` flag enabled [[^1]]:

{% highlight plaintext %}
λ> :set -fprint-explicit-kinds
λ> :info PKRetKind
type family PKRetKind @k :: k -> Type where
    PKRetKind @Type = Maybe
    PKRetKind @k = Proxy @k
{% endhighlight %}

Now we've uncovered the truth behind `PKRetKind`. There actually _is_ an
invisible parameter to the left of `::`, which is why the equations of
`PKRetKind` are able to match on it. More importantly, this led me to realize
why `HRK PKRetKind` doesn't work—`PKRetKind` really has an arity of 1!
That is, while `PKRetKind` may not have any _visible_ parameters to the left
of the `::`, it does have one _invisible_ parameter, and this actually
makes a difference when it comes to its arity. Our earlier guess that
`PKRetKind` has arity 0 was wrong, and in reality, it has arity 1.

In the earlier
`Comp PKRetKind PKRetKind True` example, it seemed as though we had
partially applied `PKRetKind` with zero arguments. But this isn't strictly
true, since GHC was actually supplying invisible `@Type` and `@Bool`
arguments under the hood. In actuality, the types we had written were
`Comp (PKRetKind @Type) (PKRetKind @Bool) True`, which make those uses
of `PKRetKind` fully saturated.

On the other hand, there is no way to interpret `HRK PKRetKind` as a well
formed type. If GHC doesn't supply `PKRetKind` with any invisible arguments,
then it would fall afoul of the type family saturation check. On the other hand,
if GHC tries to insert an invisible argument to make it
become, say `HRK (PKRetKind @k0)` (for some type `k0`), then the kind of
`HRK`'s argument would be `k0 -> Type`, which isn't polymorphic enough to
be used in a higher-rank setting (note the absence of a `forall`).
It turns out that in practice, GHC actually attempts the latter approach when
typechecking `HRK PKRetKind`,
which is why the error message complains about `k0 -> Type` instead of the
number of arguments.

If you want a poly-kinded, arity-0 type family, then the
position of the `forall` ends up being extremely important. This formulation
of `PKRetKind` truly has arity zero:

{% highlight haskell %}
type family PKRetKind' :: forall k. k -> Type
{% endhighlight %}

`PKRetKind'` has no parameters, implicit or explicit, to the left of the `::`.
This means that in terms of partial applicability, `PKRetKind'` receives top
marks. But remember that while arity giveth, arity also taketh away. In
exchange for the ability to be partially applied in more contexts,
`PKRetKind'` loses the ability to match on `k`
in its type family equations. As a consequence, a definition like this one
is impossible:

{% highlight haskell %}
type family PKRetKind' :: forall k. k -> Type where
  PKRetKind' = Maybe
{% endhighlight %}

This is because `Maybe` is of kind `Type -> Type`, which is not polymorphic
enough for the expected return kind of `forall k. k -> Type`. This might work
if `k` could be matched against `Type`, but since `k` is not bound to the left
of the `::`, this cannot happen. The only valid equations that `PKRetKind'`
could possibly have must have truly polymorphic right-hand sides, such
as in this example:

{% highlight haskell %}
type family PKRetKind' :: forall k. k -> Type where
  PKRetKind' = Proxy
{% endhighlight %}

## Putting it all together

We have seen first-hand how invisible kind arguments can have a major influence
on the design considerations of a type family. In light of this, I think it is
worth giving these invisible arguments a special mention in the definition of
arity, as their presence (or absence) can affect whether GHC accepts or rejects
a partial application of a type family:

> **Definition 3**: the _arity_ of a type family is the number of parameters
> that are bound before the return kind in its definition, either visibly
> (through a user-written type variable binder) or invisibly (through
> implicit quantification).

This definition of arity is certainly the wordiest, but I think it best
captures all of the subtleties at play here. With this definition in mind,
let's recap once more all of the poly-kinded type families we have discussed
in this post, this time using `-fprint-explicit-kinds` syntax:

{% highlight haskell %}
-- Arity 2
type family PK @k (a :: k) :: Type where
  PK @Type           Int   = Char
  PK @(Type -> Type) Maybe = Double
  PK @Bool           True  = Float

-- Arity 1
type family PKRetKind @k :: k -> Type where
    PKRetKind @Type = Maybe
    PKRetKind @k    = Proxy @k

-- Arity 0
type family PKRetKind' :: forall k. k -> Type where
  PKRetKind' = Proxy
{% endhighlight %}

While the kind of each of these type families is `forall k. k -> Type`, each
one has completely different behavior _vis-à-vis_ arity. They represent
different points on a spectrum, with one end (arity 2) having the most freedom
to match on its arguments in equations and the other end (arity 0) having the
most flexibility to be partially applied.

# Aside 2: Arities and kind inference

Most of the examples of poly-kinded type families I presented earlier in this
post used explicit return kinds, which make it straightforward to figure out
their arities. If a type family doesn't have an explicit return
kind, however, then the arity isn't always so clear. Here is a particularly
tricky example of this in action:

{% highlight haskell %}
type family MyProxy where
  MyProxy = Proxy
{% endhighlight %}

What is the arity of `MyProxy`? We know that its kind is `forall k. k -> Type`,
but as we've discovered, the arity depends on where `k` is bound. There are
two reasonable guesses for where `k` might be bound:

{% highlight haskell %}
-- Guess A (arity 1)
type family MyProxy @k :: k -> Type where
  MyProxy @k = Proxy @k

-- Guess B (arity 0)
type family MyProxy :: forall k. k -> Type where
  MyProxy = Proxy
{% endhighlight %}

As it turns out, GHC will infer Guess A in practice. I'm not quite sure as to GHC's
rationale for this choice, but them's the breaks. If you want the behavior
of Guess B, you'll simply have to write out an explicit return kind.

# Aside 3: What about type synonyms?

I've restricted my focus in this blog post to type families, but much of what
I've written also applies to type synonyms. Type synonyms, after all, can be
thought of as a very special case of type families: they always have exactly one
equation, they cannot match on their arguments, and they are not allowed to
recursive.

Moreover, type synonyms also have a saturation requirement like
type families, which means that type synonyms also have arities. Here are
some examples of type synonyms, complete with their arities (I will use
`-fprint-explicit-kinds` syntax once more for poly-kinded definitions):

{% highlight haskell %}
-- Arity 2
type Const a b = a

-- Arity 1
type ConstInt b = Const Int b

-- Arity 0
type M0 = Maybe

-- Arity 1
type M1 a = Maybe a

-- Arity 2
type PK @k (a :: k) = Proxy @k a

-- Arity 1
type PKRetKind @k = (Proxy @k :: k -> Type)

-- Arity 0
type PKRetKind' = (Proxy :: forall k. k -> Type)
{% endhighlight %}

If I want to be _really_ pedantic about my definition of arity, I need to
pay mention to type synonyms. So here is my final attempt at defining arity
(for real this time):

> **Definition 4**: the _arity_ of a type family or type synonym is the number
> of parameters that are either:
>
> 1. Bound before the return kind in its definition (in the case of a type
>    family), or
> 2. Bound before the equals sign (in the case of a type synonym).
>
> A parameter may either be bound visibly
> (through a user-written type variable binder) or invisibly (through
> implicit quantification).

# Final thoughts

Through a series of increasingly complex examples, we have seen how arity
impacts the usability and power of type families. It was a somewhat long and
winding path to get here, but I hope you learned a thing or two along the way.

If the saturation restriction makes you a bit squeamish, you're not alone.
There are others who have pondered ways to drop the saturation restriction
altogether, which I'll briefly mention below:

## Defunctionalization

One way to "partially apply" type families in today's GHC is through a
technique called _defunctionalization_, which involves encoding type
families (and partial applications thereof) as datatypes and "evaluating"
them with a special-purpose type family. Defunctionalization requires quite
a bit of boilerplate, but it does get the job done. Refer to the blog posts
[_Defunctionalization for the win_](https://typesandkinds.wordpress.com/2013/04/01/defunctionalization-for-the-win/)
and
[_Haskell with only one type family_](https://blog.poisson.chat/posts/2018-08-06-one-type-family.html)
for two different approaches to defunctionalization.

## Richer kinds
Earlier in this post, I remarked that "the kind of a type family doesn't
tell you everything". This fact proved annoying enough that it convinced
multiple academics to develop alternative typing rules for type families.

Both
[Richard Eisenberg's thesis](https://cs.brynmawr.edu/~rae/papers/2016/thesis/eisenberg-thesis.pdf)
and the more recent paper
[_Higher-order Type-level Programming in Haskell_](https://www.microsoft.com/en-us/research/uploads/prod/2019/03/ho-haskell-5c8bb4918a4de.pdf)
propose systems that, among other things, would enable partially applying any
type family, regardless of what its arity may be.
To preserve type soundess, the latter paper proposes a new
function arrow `->>` that corresponds to arguments that count towards arities
in today's GHC. In other words, the arity of a type family is now just a property
of its kind.

Here are some examples of type families, along with their kinds [[^2]],
to demonstrate the idea:

{% highlight haskell %}
-- Arity 2
type Const :: Type ->> Type ->> Type
type family Const a b where
  Const a b = a

-- Arity 1
type ConstInt :: Type ->> Type
type family ConstInt b where
  ConstInt b = Const Int b

-- Arity 0
type M0 :: Type -> Type
type family M0 where
  M0 = Maybe

-- Arity 1
type M1 :: Type ->> Type
type family M1 a where
  M1 a = Maybe a
{% endhighlight %}

Since the saturation requirement is not present in their
system, the arity of a type family is no longer that interesting. Instead,
the main purpose of `->>` is to guide type inference. For example, this program,
which uses the normal `->` function arrow in the kind of `f`, is valid:

{% highlight haskell %}
d :: forall (f :: Type -> Type) (a :: Type) (b :: Type).
     (f a ~ f b) => a -> b
d x = x
{% endhighlight %}

However, if `->` is swapped out for `->>`, then `d` will no longer typecheck,
since otherwise `f` could be instantiated with something like `ConstInt`,
which would be unsound.

There's much more than can be said about this new `->>` arrow, but that's
beyond the scope of this post. Hopefully, a system like this will one day
make all type families partially applicable like they were meant to be.

-----

[^1]: I recommend using `-fprint-explicit-kinds` on GHC 8.8 or later, since it
      displays invisible arguments using `TypeApplications`-style `@`
      syntax. You could use earlier versions of GHC if you wish, but
      it renders invisible arguments _without_ `@` syntax, so you'll
      have to discern for yourself which arguments were originally visible
      to start with and which ones were not.

[^2]: I'm using a hypothetical syntax for _top-level kind signatures_, as
      detailed in
      [this GHC proposal](https://github.com/ghc-proposals/ghc-proposals/blob/ff5b36c3f2cc9b73a1a084720e638b8bfbe58675/proposals/0036-kind-signatures.rst).
