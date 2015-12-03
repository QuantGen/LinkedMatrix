# LinkedMatrix 1.0.0.9000

* Remove `apply` and derivative functions to keep the package minimal.
* Change `length` method to behave similarly to `length` for matrices.
* Export `nNodes` function to get the number of nodes.
* Only allow matrix-like objects with matching row or column dimensions (same
  number of rows in case of `ColumnLinkedMatrix` and same number of columns in
  case of `RowLinkedMatrix`) when creating linked matrices.

# LinkedMatrix 1.0.0

Initial release.
