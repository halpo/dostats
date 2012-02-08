# Do Stats R Package #
## Introduction ##
The `dostats` R package is a compilation of helper utilities for computing statistics and operating
on R as a language.  It is particularly helpful with packages like 
[plyr](http://cran.r-project.org/package=plyr), but is useful anytime that simple functions have to
be defined.

## Details ##
There are at present two functions of note.

  * `dostats` creates a function that returns a single line `data.frame` from a list of functions.
  * `compose` createa a functional composition of an arbitrary number of functions nesting each 
     call withing the other.  `%.%` is a opperator alias for composition of two functions.  
     In other words `f%.%g` is equivalant to `function(x)f(g(x))`.
