\docType{package}
\name{LinkedMatrix-package}
\alias{LinkedMatrix-package}
\title{A Package for Linking Matrices by Columns or Rows}
\description{
The \code{LinkedMatrix} package provides classes that behave similarly to regular
matrices, but represent a sequence of matrix-like objects linked together by
columns or rows. This approach is particularly useful for very large
datasets that are distributed in chunks and can be mapped into memory using
packages such as \link[bigmemory:big.matrix-class]{bigmemory}, \link[ff:ff]{ff}, and
\link[BEDMatrix:BEDMatrix-class]{BEDMatrix::BEDMatrix}.
}
\seealso{
\code{\link{ColumnLinkedMatrix-class}}, \code{\link{RowLinkedMatrix-class}},
and \code{\link{LinkedMatrix-class}} for more information on the
\code{ColumnLinkedMatrix}, \code{RowLinkedMatrix}, and \code{LinkedMatrix}
classes.
}
