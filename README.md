<!-- README.md is generated from README.Rmd. Please edit that file -->
**m2r**
=======

**m2r** is a very new package that provides a persistent connection between [R](https://www.r-project.org) and [Macaulay2](http://www.math.uiuc.edu/Macaulay2/).

The package grew out of a collaboration at the algebraic statistics [2016 Mathematics Research Community](http://www.ams.org/programs/research-communities/mrc-16) and is currently being actively developed.

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
m2("x = 1")
#> [1] "1"
m2("x")
#> [1] "1"
```

Rings, ideals, and Grobner bases
--------------------------------

Installation
------------

Here's how you can install this *very developmental* version of **m2r**. Remember you need to have [Macaulay2](http://www.math.uiuc.edu/Macaulay2/) downloaded; **m2r** will look for it in your path variable (in the terminal, `echo $PATH`) as set by `~/.bash_profile` or, if nonexistent, then `~/.bashrc`, then `~/.profile`.

``` r
# install.packages("devtools")
devtools::install_github("musicman3320/m2r")
```
