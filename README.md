LinkedMatrix
============

[![Travis-CI Build Status](https://travis-ci.org/QuantGen/LinkedMatrix.svg?branch=master)](https://travis-ci.org/QuantGen/LinkedMatrix)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/LinkedMatrix)](https://CRAN.R-project.org/package=LinkedMatrix)
[![Rdoc](http://www.rdocumentation.org/badges/version/LinkedMatrix)](http://www.rdocumentation.org/packages/LinkedMatrix)

LinkedMatrix is an R package that provides matrices implemented as lists of matrix-like nodes, linked by columns or rows.

It was originally developed for the [BGData](https://github.com/QuantGen/BGData) package to address the array size limit of [ff](http://cran.r-project.org/package=ff) (`length must be between 1 and .Machine$integer.max`) by chaining multiple `ff` objects together.

This package is deliberately kept simple. For computational methods that use LinkedMatrix check out the [BGData package](https://github.com/QuantGen/BGData).


Example
-------

The following code generates three different matrix-like objects and links them together by rows in a `RowLinkedMatrix`. The `LinkedMatrix` instance can then be treated like any other regular matrix.

```R
library(LinkedMatrix)

m1 <- ff::ff(initdata = rnorm(50), dim = c(5, 10))
m2 <- bigmemory::big.matrix(init = rnorm(50), nrow = 5, ncol = 10)
m3 <- matrix(data = rnorm(50), nrow = 5, ncol = 10)
m <- RowLinkedMatrix(m1, m2, m3)

dim(m)
m[1, ]
m[, 1]
```


Installation
------------

Install the stable version from CRAN:

```R
install.packages("LinkedMatrix")
```

Alternatively, install the development version from GitHub:

```R
# install.packages("devtools")
devtools::install_github("QuantGen/LinkedMatrix")
```


Contribute
----------

- Issue Tracker: https://github.com/QuantGen/LinkedMatrix/issues
- Source Code: https://github.com/QuantGen/LinkedMatrix


Documentation
-------------

Further documentation can be found on [RDocumentation](http://www.rdocumentation.org/packages/LinkedMatrix).
