<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- NOTE: you have to kill any R M2 process before knitting this. -->
**m2r**
=======

**m2r** is a new R package that provides a persistent connection between [R](https://www.r-project.org) and [Macaulay2 (M2)](http://www.math.uiuc.edu/Macaulay2/).

The package grew out of a collaboration at the [2016 Mathematics Research Community](http://www.ams.org/programs/research-communities/mrc-16) on algebraic statistics, funded by the [National Science Foundation](http://www.nsf.gov) through the [American Mathematical Society](http://www.ams.org/home/page).

It is currently being actively developed, so expect changes. If you have a feature request, please file an issue!

Getting started
---------------

**m2r** is loaded like any other R package:

``` r
library(m2r)
# Loading required package: mpoly
# Loading required package: stringr
#   M2 found in /Applications/Macaulay2-1.9.2/bin
```

When loaded, **m2r** initializes a persistent connection to a back-end Macaulay2 session. The basic function in R that accesses this connection is `m2()`, which simply accepts a character string that is run by the Macaulay2 session.

``` r
m2("1 + 1")
# Starting M2
# [1] "2"
```

You can see the persistence by setting variables and accessing them across different `m2()` calls:

``` r
m2("a = 1")
# [1] "1"
m2("a")
# [1] "1"
```

You can check the variables defined in the M2 session with `m2_ls()`:

``` r
m2_ls()
#  [1] "a"                      "m2o1"                  
#  [3] "m2o2"                   "m2o3"                  
#  [5] "m2o4"                   "m2rintoutline"         
#  [7] "m2rintretcode"          "m2rintruncount"        
#  [9] "m2rintoutclassclass"    "m2rintoutvalsucceeded" 
# [11] "m2rintinout"            "m2rintinline"          
# [13] "m2rintoutval"           "m2rintoutlinesucceeded"
# [15] "m2rintvarname"          "m2rintoutclass"        
# [17] "m2rintnumlines"
```

You can also check if variables exist with `m2_exists()`:

``` r
m2_exists("a")
# [1] TRUE
m2_exists(c("a","b"))
# [1]  TRUE FALSE
```

Notice that there are many variables returned by `m2_ls()` that we didn't make. Most of those are created internally by **m2r** in order to facilitate the connection, so you won't want to access them. Others, however, are ok to access directly:

``` r
m2("m2o3")
# [1] "1"
```

Apart from the basic connection to M2, **m2r** has basic data structures and methods to reference and manipulate the M2 objects within R. For more on this, see the **m2r** internals section below.

Rings, ideals, and Grobner bases
--------------------------------

**m2r** currently has basic support for [rings](https://en.wikipedia.org/wiki/Ring_(mathematics)) (think: [polynomial rings](https://en.wikipedia.org/wiki/Polynomial_ring)):

``` r
(R <- ring(c("t", "x", "y", "z"), "QQ"))
# M2 Ring: QQ[t,x,y,z], grevlex order
```

and [ideals](https://en.wikipedia.org/wiki/Ideal_(ring_theory)) of rings:

``` r
(I <- ideal(c("t^4 - x", "t^3 - y", "t^2 - z"), R))
# M2 Ideal of ring QQ[t,x,y,z] (grevlex) with generators: 
# < t^4  -  x,  t^3  -  y,  t^2  -  z >
```

You can compute [Grobner bases](https://en.wikipedia.org/wiki/Gröbner_basis) as well. The basic function to do this is `gb()`:

``` r
gb(I)
# z^2  -  x
# z t  -  y
# -1 z x  +  y^2
# -1 x  +  t y
# -1 z y  +  x t
# -1 z  +  t^2
```

Perhaps an easier way to do this is just to list off the polynomials as character strings:

``` r
gb("t^4 - x", "t^3 - y", "t^2 - z", ring = R)
# z^2  -  x
# z t  -  y
# -1 z x  +  y^2
# -1 x  +  t y
# -1 z y  +  x t
# -1 z  +  t^2
```

The result is an `mpolyList` object, from the [**mpoly** package](https://github.com/dkahle/mpoly). You can see the M2 code by adding `code = TRUE`:

``` r
gb("t^4 - x", "t^3 - y", "t^2 - z", code = TRUE)
# gens gb(ideal({t^4-x,t^3-y,t^2-z}), DegreeLimit => {})
```

You can compute the basis respective of different [monomial orders](https://en.wikipedia.org/wiki/Monomial_order) as well. The default ordering is the one in the respective ring, which defaults to `grevlex`; however, changing the order is as simple as changing the ring.

``` r
R <- ring(c("x","y","t","z"), order = "lex")
gb("t^4 - x", "t^3 - y", "t^2 - z", ring = R)
# t^2  -  z
# -1 t z  +  y
# -1 z^2  +  x
```

On a technical level, `gb()` uses [nonstandard evaluation rules](http://adv-r.had.co.nz/Computing-on-the-language.html). A more stable way to use the function is to use its standard evaluation version `gb_()`. `gb_()` accepts first a data structure describing the polynomials or ideal to fing the Grobner basis of, then the referent ring, and then a number of other objects. At a basic level this simply changes the previous syntax to

``` r
poly_chars <- c("t^4 - x", "t^3 - y", "t^2 - z")
gb_(poly_chars, ring = R)
# t^2  -  z
# -1 t z  +  y
# -1 z^2  +  x
```

`gb_()` is significantly easier than `gb()` to program with, so we strongly recommend that you use `gb_()`, especially inside of other functions.

Factoring integers and polynomials
----------------------------------

You can compute [prime decompositions](https://en.wikipedia.org/wiki/Integer_factorization) of integers with `factor_n()`:

``` r
(x <- 2^5 * 3^4 * 5^3 * 7^2 * 11^1)
# [1] 174636000
factor_n(x)
# $prime
# [1]  2  3  5  7 11
# 
# $power
# [1] 5 4 3 2 1
```

You can also [factor polynomials](https://en.wikipedia.org/wiki/Factorization) over rings using `factor_poly()`:

``` r
QQxy <- ring(c("x","y"), "QQ")
factor_poly("x^4 - y^4", QQxy)
# $factor
# x  -  y
# x  +  y
# x^2  +  y^2
# 
# $power
# [1] 1 1 1
mp("x-y") * mp("x+y") * mp("x^2+y^2")
# x^4  -  y^4
```

Smith normal form of a matrix
-----------------------------

The Smith normal form of a matrix *M* here refers to the decomposition of an integer matrix *D = PMQ*, where *D*, *P*, and *Q* are integer matrices and *D* is diagonal. *P* and *Q* are unimodular matrices (their determinants are -1 or 1), so they are invertible. This is somewhat like a singular value decomposition for integer matrices.

``` r
M <- matrix(c(
   2,  4,   4,
  -6,  6,  12,
  10, -4, -16
), nrow = 3, byrow = TRUE)

(mats <- snf(M))
# $D
#      [,1] [,2] [,3]
# [1,]   12    0    0
# [2,]    0    6    0
# [3,]    0    0    2
# M2 Matrix over ZZ[]
# $P
#      [,1] [,2] [,3]
# [1,]    1    0    1
# [2,]    0    1    0
# [3,]    0    0    1
# M2 Matrix over ZZ[]
# $Q
#      [,1] [,2] [,3]
# [1,]    4   -2   -1
# [2,]   -2    3    1
# [3,]    3   -2   -1
# M2 Matrix over ZZ[]
P <- mats$P; D <- mats$D; Q <- mats$Q

P %*% M %*% Q                # = D
#      [,1] [,2] [,3]
# [1,]   12    0    0
# [2,]    0    6    0
# [3,]    0    0    2
solve(P) %*% D %*% solve(Q)  # = M
#      [,1] [,2] [,3]
# [1,]    2    4    4
# [2,]   -6    6   12
# [3,]   10   -4  -16

det(P)
# [1] 1
det(Q)
# [1] -1
```

**m2r** internals: pointers and pointer functions
-------------------------------------------------

At a basic level, **m2r** works by passing strings between R and M2. Originating at the R side, these strings are properly formated M2 code constructed from the inputs to the R functions. That code goes to M2, is evaluated there, and then "exported" with M2's function `toExternalString()`. The resulting string often, but not always, produces the M2 code needed to recreate the object resulting from the evaluation, and in that sense is M2's version of R's `dput()`. That string is passed back into R and parsed there into R-style data structures, typically S3-classed lists.

The R-side parsing of the external string from M2 is an expensive process because it is currently implemented in R as opposed to C++. Consequently (and for other reasons, too!), in some cases you'll want to do a M2 computation from R, but leave the output in M2. Since you will ultimately want something in R referring to the result, nearly every **m2r** function that performs M2 computations as a pointer version. As a simple naming convention, the function that returns the pointer, called the reference function, is determined by the ordinary function, called the value function, by appending a `.`.

For example, we've seen that `factor_n()` computes the prime decomposition of a number. The corresponding reference function is `factor_n.()`:

``` r
(x <- 2^5 * 3^4 * 5^3 * 7^2 * 11^1)
# [1] 174636000
factor_n.(x)
# M2 Pointer Object
#   ExternalString : new Product from {new Power from {2,5},new Power fro...
#          M2 Name : m2o204
#         M2 Class : Product (WrapperType)
```

All value functions simply wrap reference functions and parse the output with `m2_parse()`, a general M2 parser, often with little more parsing. The general principle we follow here is this: if the M2 object has a direct analogue in R, this is the kind of object it is parsed into and additional M2 properties are kept as metadata; if there is no direct analogue in R, the object is `NA` with the metadata.

Perhaps the easiest way to see this is with a matrix. The `m2_matrix()` creates a matrix on the M2 side from input on the R side. In the following, I make use of [**magrittr**'s pipe operator](https://github.com/tidyverse/magrittr) with which the following calls are semantically equivalent: `g(f(x))` and `x %>% f %>% g`.

``` r
library(magrittr)
mat <- matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2)
m2_matrix.(mat)
m2_matrix.(mat) %>% m2_parse
m2_matrix.(mat) %>% m2_parse %>% str
m2_matrix(mat) # = m2_parse(m2_matrix.(mat))
```

It may be helpful to think of every `m2` object as being a missing value (`NA`, a `logical(1)`) with M2 metadata stored as attributes. This can be accessed with `m2_meta()`. For example, a ring:

``` r
r <- ring(c("x","y"), "QQ")
str(r)
m2_meta(r)
```

Creating your own **m2r** wrapper
---------------------------------

To create your own wrapper function of something in Macaulay2, you'll need to create an R file that looks like the one below. This will create both value (e.g. `f`) and reference/pointer (e.g. `f.`) versions of the function. As a good example of these at work, see the scripts for [`factor_n()`](https://github.com/musicman3320/m2r/blob/master/R/factor_n.R) or [`factor_poly()`](https://github.com/musicman3320/m2r/blob/master/R/factor_poly.R).

``` r
#' Function documentation header
#'
#' Function header explanation, can run several lines. Function
#' header explanation, can run several lines. Function header
#' explanation, can run several lines.
#'
#' @param esntl_parm_1 esntl_parm_1 description
#' @param esntl_parm_2 esntl_parm_2 description
#' @param code return only the M2 code? (default: \code{FALSE})
#' @param parse_parm_1 parse_parm_1 description
#' @param parse_parm_2 parse_parm_2 description
#' @param ... ...
#' @name f
#' @return (value version) parsed output or (reference/dot version)
#'   \code{m2_pointer}
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#' # put examples here
#' 1 + 1
#'
#' }
#'





# value version of f (standard user version)
#' @rdname f
#' @export
f <- function(esntl_parm_1, esntl_parm_2, code = FALSE, parse_parm_1, parse_parm_2, ...) {

  # run m2
  args <- as.list(match.call())[-1]
  eargs <- lapply(args, eval, envir = parent.frame())
  pointer <- do.call(f., eargs)
  if(code) return(invisible(pointer))

  # parse output
  parsed_out <- m2_parse(pointer)

  # more parsing
  TRUE

  # return
  TRUE

}




# reference version of f (returns pointer to m2 object)
#' @rdname f
#' @export
f. <- function(esntl_parm_1, esntl_parm_2, code = FALSE, ...) {

  # basic arg checking
  TRUE

  # create essential parameters to pass to m2 this step regularizes input to m2, so it
  # is the one that deals with pointers, chars, rings, ideals, mpolyLists, etc.
  TRUE

  # construct m2_code from regularized essential parameters
  TRUE

  # message
  if(code) { message(m2_code); return(invisible(m2_code)) }

  # run m2 and return pointer
  m2.(m2_code)

}
```

Acknowledgements
----------------

This material is based upon work supported by the National Science Foundation under [Grant No. 1321794](http://nsf.gov/awardsearch/showAward?AWD_ID=1321794).

Installation
------------

Here's how you can install the current *developmental* version of **m2r**. Remember you need to have [Macaulay2](http://www.math.uiuc.edu/Macaulay2/) downloaded; **m2r** will look for it in your path variable (in the terminal, `echo $PATH`) as set by `~/.bash_profile` or, if nonexistent, then `~/.bashrc`, then `~/.profile`.

``` r
# install.packages("devtools")
devtools::install_github("musicman3320/m2r")
```
