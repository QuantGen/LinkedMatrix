\name{cbind.ColumnLinkedMatrix}
\alias{cbind.ColumnLinkedMatrix}
\alias{cbind.RowLinkedMatrix}
\alias{rbind.ColumnLinkedMatrix}
\alias{rbind.RowLinkedMatrix}
\title{Combine Matrix-Like Objects by Columns or Rows}
\description{
Compared to the \code{ColumnLinkedMatrix} and \code{RowLinkedMatrix}
constructor functions, nested \code{LinkedMatrix} objects that are passed via
\code{...} will not be treated as matrix-like objects, but their nodes will be
extracted and merged with the new \code{ColumnLinkedMatrix} (for
\code{cbind.ColumnLinkedMatrix}) or \code{RowLinkedMatrix} (for
\code{rbind.RowLinkedMatrix}) object for a more compact representation.
\code{cbind.ColumnLinkedMatrix} currently only works for
\code{ColumnLinkedMatrix} objects, \code{rbind.RowLinkedMatrix} only for
\code{RowLinkedMatrix}.
}
\usage{
\method{cbind}{ColumnLinkedMatrix}(..., deparse.level = 0L)

\method{cbind}{RowLinkedMatrix}(..., deparse.level = 0L)

\method{rbind}{ColumnLinkedMatrix}(..., deparse.level = 1L)

\method{rbind}{RowLinkedMatrix}(..., deparse.level = 1L)
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
