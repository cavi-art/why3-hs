Why3-hs
=======

A Haskell interface for using Why3 superpowers for proving formulas.

Currently this module is a bit lame: it is just a wrapper on top of
"why3 prove", and you can achieve more or less the same results by
just using "why3 prove -T why -", but it has the added benefit of
using Haskell's ADTs for dealing with the different outputs that Why3
can deal with, as well as parsing that output already.


Usage
=====

```haskell
import Why3.AsProcess

x = discharge Z3 "import int.Int" "forall a: int. a = a"
-- x: Valid :: SMTOutput

y = isValidProof $ dischargeTheory Z3 "theory T goal G: forall a: int. a = a end"
-- y: True :: Bool
```


Using this library on your project
==================================

This library is not on cabal nor stackage (yet?).

You can use this library on your stack-configured projects by adding
a key to your `stack.yaml` file:

```yaml
packages:
- location: https://github.com/cavi-art/why3-hs/archive/v0.1.0.0.tar.gz
```


Contributing
============

This project encourages the [GitHub Flow][flow] for external
contributions. Please send any improvements you may find via GitHub a
Pull Request. You can also send them by email or any other means, and
they will end up being integrated here.

By sending a Pull Request you agree to publish your own code under the same 
license as the one stated in the repository.
  

Acknowledgements
================

This work is partially supported by
the Spanish MINECO project CAVI-ART (TIN2013-44742-C4-3-R),
Madrid regional project N-GREENS Software-CM (S2013/ICE-2731) and
UCM grant GR3/14-910502.

CAVI-ART stands for Computer Assisted ValIdation by Analysis, 
tRansformation and Testing.
