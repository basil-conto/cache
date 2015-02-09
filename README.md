# cache

This is a simple simulation of a CPU cache in Haskell, generalised over the
three parameters

 * `L` - Block size;
 * `K` - Degree of associativity; and
 * `N` - Number of sets (or cache lines),

as well as the memory address width.

### Future work

The current implementation is relatively naive and there is much room for
optimisation and specialisation. Possible improvements include:

 * Using more efficient containers
 * More monad!
 * Less Haskell

### Disclaimer

The program was written for recreational and educational purposes on the
author's part and was not intended for actual use.
