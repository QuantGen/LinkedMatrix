# LinkedMatrix 1.3.0

* Add [crochet](https://cran.r-project.org/package=crochet) subsetting and
  replacement support.
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
