# LinkedMatrix 1.1.0.9000

* Optimize subsetting by avoiding copies.

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
