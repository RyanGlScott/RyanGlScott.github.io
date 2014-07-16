---
layout: post
title: Installing GHC on FreeBSD
---

Recently, I discovered that the [GHC](http://www.haskell.org/ghc/) developers classify the platforms that they support into different [tiers](https://ghc.haskell.org/trac/ghc/wiki/Platforms). Tier 1, somewhat surprisingly, includes *four* distinct operating systems: Windows, OS X, Linux, ...and FreeBSD. That last one surprised me, since they don't even advertise their FreeBSD support that much on the [Haskell Platform website](http://www.haskell.org/platform/). (You have to go to the [Linux](http://www.haskell.org/platform/linux.html) section to find the FreeBSD information.)

This got me curious as to how easy to use GHC is on FreeBSD, since some other OSes have some kinks that need ironed (\*cough\*[Windows](http://www.haskell.org/haskellwiki/Windows#Quickstart_on_Windows_7)\*cough\*). Since I found the process of installing GHC and `cabal` on FreeBSD to be somewhat nontrivial, I'll post the exact steps I took here.

### Prerequisites <a name="prerequisites"></a>
* A FreeBSD distribution. You can get the real deal [here](http://www.freebsd.org/where.html), or you can get a noob-friendlier derivative such as [PC-BSD](http://www.pcbsd.org/en/download.html). I used the latter.
* Internet access

### There are two paths you can go by <a name="there-are-two-paths"></a>
There are two common ways of installing GHC and its associated bells and whistles. One can install the [Haskell Platform](http://www.haskell.org/platform/), which is a collection of GHC, the [`cabal`](http://www.haskell.org/cabal/) build system, several prebuilt packages, and other utilities. Installing the Haskell Platform is quicker and less painful, but it is updated less frequently. If you prefer this, go to the [Haskell Platform](#haskell-platform) section.

Alternatively, you can install a [binary distribution](http://www.haskell.org/ghc/) of GHC and then build all of the needed tools yourself. This requires more heavy lifting, but you gain the ability to use whatever packages you want (instead of whatever the Haskell Platform is bundled with). If you prefer this, go to the [Binary distribution](#binary-distribution) section.

### Haskell Platform <a name="haskell-platform"></a>
Luckily, the Haskell Platform exists as [a FreeBSD port](http://www.freebsd.org/ports/index.html). At the time of this post, the latest version includes GHC 7.6.3. You can install the Platform by doing one of the following:

* Opening a terminal and running `pkg install hs-haskell-platform` as root
* (On PC-BSD) Go to App Cafe. Click Browser View > Raw Packages. Then search for "Haskell Platform" and install it.

If you don't want the full Haskell Platform, you can also individually install the `ghc` and `hs-cabal-install` packages.

Either way, once everything you want is installed, you should edit `~/.cshrc` and add `~/.cabal/bin` to your `PATH` so that you can run Haskell executables. Make sure that you put it before `/usr/local/bin` so that more recently installed versions of `cabal` take precedence over the Haskell Platform's version.

### Binary distribution <a name="binary-distribution"></a>
#### GHC <a name="binary-distribution-ghc"></a>
First, download the latest [GHC binary archive](http://www.haskell.org/ghc/download_ghc_7_8_3#freebsd_x86_64) (version 7.8.3 at time of writing) and [`cabal-install` source archive](http://www.haskell.org/cabal/release/cabal-install-1.20.0.3/) (version 1.20.0.3 at time of writing).

Next, we need to install some GNU software, since BSD distributions tend to spurn it. According to the FreeBSD Haskell devs, you need at least [this much](http://www.haskell.org/ghc/dist/7.8.3/README.fbsd.html) installed:

* `compat8x` (if using FreeBSD 9 or later)
* `gcc` (4.7 or later)
* `gmake`
* `perl5`
* `libiconv`

On PC-BSD, I already had `gcc47`, `perl5`, and `libiconv` installed, so I only needed to install the following:

```bash
$ pkg install gmake
$ pkg install compat8x-amd64 # Your architecture may be different
```
> You might be tempted to use `clang`, which is now FreeBSD's default compiler, instead of `gcc`, but I had nothing but problems using it. The FreeBSD Haskell devs [also advise](http://www.haskell.org/ghc/dist/7.8.3/README.fbsd.html#running-configure) not using `clang` until it is further tested.

Now we can install GHC:

```bash
$ env CC=gcc47 ./configure --with-gcc=gcc47 \
    --with-ld=/usr/local/bin/ld # Using the appropriate gcc version
$ gmake install # As root
```

> Note that this won't configure GHC's manpages correctly. If you want them, make sure to run `cp /usr/local/share/man/man1/ghc.1 /usr/local/man/man1/` as root after installing GHC.

#### `cabal-install` <a name="binary-distribution-cabal-install"></a>

Extract the `cabal-install` archive and open `bootstrap.sh` in your favorite editor. As of version 1.20.0.3, line 208 of this file, namely:

```bash
  ${GZIP} -d < "${PKG}-${VER}.tar.gz" | ${TAR} -x
```

won't work correctly on FreeBSD. To fix it, change this line to:

```bash
  ${GZIP} -d < "${PKG}-${VER}.tar.gz" | ${TAR} -xf -
```

Also, `bootstrap.sh` will try to look for `gcc`, but it is probably named `gcc47` or something similar instead. To help ease `boostrap.sh`'s burden, make a symbolic link with `ln -s /usr/local/bin/gcc47 /usr/local/bin/gcc` as root. Then install `cabal-install` by running `./boostrap.sh`.

Finally, you should edit `~/.cshrc` and add `~/.cabal/bin` to your `PATH` so that you can run `cabal` and other Haskell executables.

### But wait <a name="but-wait"></a>
Regardless of how you installed GHC, you'll notice some problems if you try to install packages that require nontrivial build tools such as `autoconf` or `hsc2hs`. The problems might manifest themselves as errors such as `runProcess: runInteractiveProcess: exec: permission denied (Permission denied)`. This hints at the problem: a build tool is trying to execute a file in `/tmp`, but `/tmp` has the `noexec` filesystem option.

In PC-BSD, this can be fixed by going to the PC-BSD Control Panel > Hardware > Disk Manager. Under the ZFS Filesystems tab, right-click `tank/tmp` and click "Edit properties". Change `exec` to "on" and click "Apply."

That should clear the way for `cabal` to do its thing. Now you can get the very latest stuff:

```bash
$ cabal update
$ cabal install cabal-install
$ cabal install happy alex # If not on the Haskell Platform
```