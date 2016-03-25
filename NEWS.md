# LinkedMatrix 1.1.0.9000

* Optimize subsetting by avoiding copies:
  * the return matrix is initialized as `integer()` instead of `logical()`
  * if row or column indices are sorted, do not resort return matrix
  * if data from a single node is requested, pass through result instead of
    buffering it in return matrix
* Add `i` and `j` parameters in `index` to only generate entries for those
  indexes.

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
