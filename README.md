LinkedMatrix
============

[![Travis-CI Build Status](https://travis-ci.org/QuantGen/LinkedMatrix.svg?branch=master)](https://travis-ci.org/QuantGen/LinkedMatrix)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/LinkedMatrix)](http://cran.r-project.org/package=LinkedMatrix)

LinkedMatrix is an R package that provides matrices implemented as lists of matrix-like nodes, linked by columns or rows.

It was originally developed for the [BGData](https://github.com/QuantGen/BGData) package to address the array size limit of [ff](http://cran.r-project.org/package=ff) (`length must be between 1 and .Machine$integer.max`) by chaining multiple `ff` objects together.


Example
-------

The following code generates two random memory-mapped matrices using `ff` and links them together by rows in a `RowLinkedMatrix`. The `LinkedMatrix` instance can then be treated like any other regular matrix.

```R
library(LinkedMatrix)
library(ff)

ff1 <- ff(dim = c(5, 10), vmode = "double", initdata = rnorm(50))
ff2 <- ff(dim = c(5, 10), vmode = "double", initdata = rnorm(50))
m <- RowLinkedMatrix(ff1, ff2)

dim(m)
m[1, ]
m[, 1]
```

This can also be expressed as:

```R
library(LinkedMatrix)
library(ff)

m <- LinkedMatrix(nrow = 10, ncol = 10, nNodes = 2, linkedBy = "rows",
                  nodeInitializer = "ffNodeInitializer", vmode = "double")
m[] <- rnorm(100)

dim(m)
m[1, ]
m[, 1]
```
**[Further examples](https://github.com/QuantGen/BGData/blob/master/README.md)**

Installation
------------

To get the current released version from CRAN:

```r
install.packages("LinkedMatrix")
```

To get the current development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("QuantGen/LinkedMatrix")
```
