\name{cbind.ColumnLinkedMatrix}
\alias{cbind.ColumnLinkedMatrix}
\alias{cbind.RowLinkedMatrix}
\title{Combine Matrix-Like Objects by Columns}
\description{
Compared to the \code{ColumnLinkedMatrix} and \code{RowLinkedMatrix}
constructor functions, nested \code{LinkedMatrix} objects that are passed via
\code{...} will not be treated as matrix-like objects, but their nodes will be
extracted and merged with the new \code{ColumnLinkedMatrix} object for a more
compact representation. This method will currently only work for
\code{ColumnLinkedMatrix} objects.
}
\usage{
\method{cbind}{ColumnLinkedMatrix}(..., deparse.level = 0L)

\method{cbind}{RowLinkedMatrix}(..., deparse.level = 0L)
}
\arguments{
  \item{...}{
    Matrix-like objects to be combined by columns.
  }
  \item{deparse.level}{
    Currently unused, defaults to 0.
  }
}
\seealso{
\code{\link{ColumnLinkedMatrix-class}}, \code{\link{RowLinkedMatrix-class}},
and \code{\link{LinkedMatrix-class}} for more information on the
\code{ColumnLinkedMatrix}, \code{RowLinkedMatrix}, and \code{LinkedMatrix}
classes.
}
