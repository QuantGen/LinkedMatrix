#' A Package for Linking Matrices by Columns or Rows.
#'
#' The `LinkedMatrix` package provides classes that behave similarly to regular
#' matrices, but represent a sequence of matrix-like objects linked together by
#' columns or rows. This approach is particularly useful for very large
#' datasets that are distributed in chunks and can be mapped into memory using
#' packages such as [bigmemory][bigmemory::big.matrix-class], [ff][ff::ff], and
#' [BEDMatrix::BEDMatrix-class].
#'
#' @seealso The [ColumnLinkedMatrix-class] and [RowLinkedMatrix-class] classes.
#' @docType package
#' @name LinkedMatrix-package
#' @aliases LinkedMatrix-package
#' @import methods
NULL
