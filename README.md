<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- NOTE: you have to kill any R M2 process before knitting this. -->
**m2r**
=======

**m2r** is a very new R package that provides a persistent connection between [R](https://www.r-project.org) and [Macaulay2](http://www.math.uiuc.edu/Macaulay2/).

The package grew out of a collaboration at the [2016 Mathematics Research Community](http://www.ams.org/programs/research-communities/mrc-16), funded by the [National Science Foundation](http://www.nsf.gov) through the [American Mathematical Society](http://www.ams.org/home/page).

It is currently being actively developed, so expect changes. If you have a feature request, please file an issue!

Getting started
---------------

**m2r** is loaded like any other R package:

``` r
library(m2r)
#> Loading required package: mpoly
#> Loading required package: stringr
#>   M2 found in /Applications/Macaulay2-1.5/bin
```

When loaded, **m2r** initializes a persistent connection to a back-end Macaulay2 session. The basic function in R that accesses this connection is `m2()`, which simply accepts a character string that is run by the Macaulay2 session.

``` r
m2("1 + 1")
#> Starting M2
#> [1] "2"
```

You can see the persistence by setting variables and accessing them across different `m2()` calls:

``` r
m2("a = 1")
#> [1] "1"
m2("a")
#> [1] "1"
```

Apart from the basic connection to M2, **m2r** has basic data structures and methods to reference and manipulate the M2 objects within R. These are being actively developed.

Rings, ideals, and Grobner bases
--------------------------------

**m2r** currently has basic support for [rings](https://en.wikipedia.org/wiki/Ring_(mathematics)):

``` r
R <- ring(c("x", "y", "z"))
R
#> M2 PolynomialRing: CC[x,y,z], grevlex order
str_m2(R)
#> M2 Object
#>     Type : PolynomialRing
#>   R Name : R
#>  M2 Name : m2rintring00000001
#>     Vars : x, y, z
#>    Order : grevlex
```

You can compute [Grobner bases](https://en.wikipedia.org/wiki/Gröbner_basis) as well. The basic function to do this is `gb()`:

``` r
gb("t^4 - x", "t^3 - y", "t^2 - z")
#> z^2  -  x
#> z t  -  y
#> -1 z x  +  y^2
#> -1 x  +  t y
#> -1 z y  +  x t
#> -1 z  +  t^2
```

The result is an `mpolyList` object, from the [**mpoly** package](https://github.com/dkahle/mpoly). You can see the M2 code by adding `code = TRUE`:

``` r
gb("t^4 - x", "t^3 - y", "t^2 - z", code = TRUE)
#> R := QQ[t,x,y,z]
#> I := ideal(t^4  -  x, t^3  -  y, t^2  -  z)
#> gens gb I
#> z^2  -  x
#> z t  -  y
#> -1 z x  +  y^2
#> -1 x  +  t y
#> -1 z y  +  x t
#> -1 z  +  t^2
```

You can compute the basis respective of different orders as follows. The default ordering is the [grevlex order](https://en.wikipedia.org/wiki/Monomial_order) on the variables given by `mpoly::vars()` applied to the `mpolyList` given by the polynomials.

``` r
R <- ring(c("x","y","t","z"), order = "lex")
gb("t^4 - x", "t^3 - y", "t^2 - z", ring = R)
#> t^2  -  z
#> -1 t z  +  y
#> -1 z^2  +  x
```

`gb()` also accepts `mpoly` objects, like `gb(p1, p2, p3)` where `p1`, `p2`, and `p3` are `mpoly` objects (e.g. `p1 <- mp("t^4 - x")`). There is also a [standard evaluation](http://adv-r.had.co.nz/Computing-on-the-language.html) version of `gb()` called `gb_()`, which takes in a `mpolyList` object directly.

``` r
(ps <- mp(c("t^4 - x", "t^3 - y", "t^2 - z")))
#> t^4  -  x
#> t^3  -  y
#> t^2  -  z
gb_(ps)
#> z^2  -  x
#> z t  -  y
#> -1 z x  +  y^2
#> -1 x  +  t y
#> -1 z y  +  x t
#> -1 z  +  t^2
```

Acknowledgements
----------------

This material is based upon work supported by the National Science Foundation under [Grant No. 1321794](http://nsf.gov/awardsearch/showAward?AWD_ID=1321794).

Installation
------------

Here's how you can install this *very developmental* version of **m2r**. Remember you need to have [Macaulay2](http://www.math.uiuc.edu/Macaulay2/) downloaded; **m2r** will look for it in your path variable (in the terminal, `echo $PATH`) as set by `~/.bash_profile` or, if nonexistent, then `~/.bashrc`, then `~/.profile`.

``` r
# install.packages("devtools")
devtools::install_github("musicman3320/m2r")
```
