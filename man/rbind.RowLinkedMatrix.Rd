\name{rbind.ColumnLinkedMatrix}
\alias{rbind.ColumnLinkedMatrix}
\alias{rbind.RowLinkedMatrix}
\title{Combine Matrix-Like Objects by Rows}
\description{
Compared to the \code{ColumnLinkedMatrix} and \code{RowLinkedMatrix}
constructor functions, nested \code{LinkedMatrix} objects that are passed via
\code{...} will not be treated as matrix-like objects, but their nodes will be
extracted and merged with the new \code{RowLinkedMatrix} object for a more
compact representation.  This method will currently only work for
\code{RowLinkedMatrix} objects.
}
\usage{
\method{rbind}{ColumnLinkedMatrix}(..., deparse.level = 1L)

\method{rbind}{RowLinkedMatrix}(..., deparse.level = 1L)
}
\arguments{
  \item{...}{
    Matrix-like objects to be combined by rows.
  }
  \item{deparse.level}{
    Currently unused, defaults to 0.
  }
}
\seealso{
\code{\link{ColumnLinkedMatrix-class}}, \code{\link{RowLinkedMatrix-class}},
and \code{\link{LinkedMatrix}} for more information on the
\code{ColumnLinkedMatrix}, \code{RowLinkedMatrix}, and \code{LinkedMatrix}
classes.
}
