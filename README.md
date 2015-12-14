LinkedMatrix
============

[![Travis-CI Build Status](https://travis-ci.org/QuantGen/LinkedMatrix.svg?branch=master)](https://travis-ci.org/QuantGen/LinkedMatrix)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/LinkedMatrix)](http://cran.r-project.org/package=LinkedMatrix)

LinkedMatrix is an R package that provides matrices implemented as collections of matrix-like nodes, linked by columns or rows.

This was originally developed for the [BGData](https://github.com/QuantGen/BGData) package to address the 32-bit integer limit of [ff](https://cran.r-project.org/web/packages/ff/index.html) by chaining multiple `ff` objects together.


Example
-------

The following code generates two random memory-mapped matrices using `ff` and links them together by rows in a `RowLinkedMatrix`. The `LinkedMatrix` instance can then be treated like any other regular matrix.

```R
library(LinkedMatrix)
library(ff)

ff1 <- ff(vmode = 'double', dim = c(5, 10), initdata = rnorm(50))
ff2 <- ff(vmode = 'double', dim = c(5, 10), initdata = rnorm(50))
m <- RowLinkedMatrix(ff1, ff2)

dim(m)
m[1, ]
m[, 1]
```


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


Classes & Methods
-----------------

### Class `ColumnLinkedMatrix`

An S4 class to represent column-linked matrices, inherits from `list`.

#### Constructor

- `ColumnLinkedMatrix(...)` where `...` is a sequence of matrix-like objects of the same row-dimension. If no matrix-like objects are given, a single 1x1 node filled with `NA` is returned.

### Class `RowLinkedMatrix`

An S4 class to represent row-linked matrices, inherits from `list`.

#### Constructor

- `RowLinkedMatrix(...)` where `...` is a sequence of matrix-like objects of the same column-dimension. If no matrix-like objects are given, a single 1x1 node filled with `NA` is returned.

### Class `LinkedMatrix`

An S4 class union of `ColumnLinkedMatrix` and `RowLinkedMatrix`.

#### Methods

All methods described below are available on both `ColumnLinkedMatrix` and `RowLinkedMatrix` objects.

- `[` and `[<-` for subsetting and replacing matrix elements
- `dim(x)`, `nrow(x)`, `ncol(x)`, `length(x)`
- `dimnames(x)`, `colnames(x)`, `rownames(x)`
- `as.matrix(x)` converts a `LinkedMatrix` object to a matrix (if small enough)
- `nNodes(x)` return the number iof nodes
- `nodes(x)` returns the column or row indexes at which each node starts and ends
- `index(x)` maps each column or row index of a linked matrix to the column or row index of its corresponding node
- `[[` and `[[<-` for subsetting and replacing nodes
