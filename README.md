# hSIMD 

This is a simple eDSL in Haskell to write and test SIMD (SSE, SSE2, etc) code
using QuickCheck. You can write the SIMD code in Haskell and check that some
properties are true for a number of random inputs.


# Examples


## Bitonic Sort


In src/Example.hs there is an example of a program that sorts the integers
inside two SIMD registers (8 ints in total) in a function called bitonicSort.

If the input is [1, 3, 4, 7] and [5, 6, 8, 2], the bitonicSort is going to return
both registers sorted: [1, 2, 3, 4] and [5, 6, 7, 8].


You can write a property (function check inside src/Example.hs) to check it the
first element of the first register is less or equals to the second element, and
so on. This property should be true for every integer in every possible
register. If this property is false, then QuickCheck will return a counter
example.


# TODO and limitations


I would like to also support SBV to use SAT solvers to create proofs of the
programs instead of using random data like QuickCheck. The problem is that I
need to support Ord (normal integer comparision) and SOrd (comparision for
symbolic numbers in SBV) typeclasses at the same time in runSIMD in
src/SIMD/SIMD.hs, but I can't find a way to do it.

In a [initial
prototype](https://gist.github.com/fgaray/974f9573387fb23bd3023bdd434bd00e) of
this idea, I just changed Ord to SOrd to create the proofs.
