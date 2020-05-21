# LinkedMatrix 1.4.0

* Follow [Bioconductor S4 practices][2]. If you have used `new()` to create
  `ColumnLinkedMatrix` and `RowLinkedMatrix` instances, please use the
  `ColumnLinkedMatrix()` and `RowLinkedMatrix()` constructor functions instead.
* Drop defunct `cbind.RowLinkedMatrix` and `rbind.ColumnLinkedMatrix` methods.
* Update citation instructions.
* Use `inherits(., *)` instead of `class(.) == *` (R4 compat).
* Use tinytest instead of testthat.


# LinkedMatrix 1.3.1

* Fix `rownames()` (for `RowLinkedMatrix`) or `colnames()` (for
  `ColumnLinkedMatrix`) returning `NULL` if first node does not have dimnames,
  but other nodes do. Missing entries will be denoted empty string, just like in
  base `dimnames()`.
* Do not exclude NULL when checking if rownames (for `ColumnLinkedMatrix`) or
  colnames (for `RowLinkedMatrix`) do not match.


# LinkedMatrix 1.3.0

* Add [crochet][1] subsetting and replacement support.
* Add generic `as.ColumnLinkedMatrix` and `as.RowLinkedMatrix` for easy
  creation of `LinkedMatrix` objects from `list`s of matrix-like objects
  without `do.call`.
* Warn if rownames (for `ColumnLinkedMatrix`) or colnames (for
  `RowLinkedMatrix`) do not match.
* Implement `str` method.
* Implement `is.matrix` method.
* Slight performance improvements in `nodes` methods.
* New `sort` parameter in `index()` which is set by default.
* Add examples.


# LinkedMatrix 1.2.0

* Fix wrong order in result when subsetting using unordered positive integers.
* Add `i` and `j` parameters in `index` to only generate entries for those
  indexes.
* Various subsetting optimizations.


# LinkedMatrix 1.1.0

* Add `LinkedMatrix` constructor that creates either a `ColumnLinkedMatrix` or
  `RowLinkedMatrix` (controlled by `linkedBy`) of certain dimensions and of
  certain type.
* Add `rbind` for `RowLinkedMatrix` and `cbind` for `ColumnLinkedMatrix`.
* Remove `apply` and derivative functions to keep the package minimal.
* Change `length` method to behave similarly to `length` for matrices.
* Export `nNodes` function to get the number of nodes.
* Only allow matrix-like objects with matching row or column dimensions (same
  number of rows in case of `ColumnLinkedMatrix` and same number of columns in
  case of `RowLinkedMatrix`) when creating linked matrices.


# LinkedMatrix 1.0.0

Initial release.

[1]: https://CRAN.R-project.org/package=crochet
[2]: https://bioconductor.org/help/course-materials/2017/Zurich/S4-classes-and-methods.html
