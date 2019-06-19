\name{as.matrix.LinkedMatrix}
\alias{as.matrix.LinkedMatrix}
\title{Converts a LinkedMatrix Instance to a Matrix (if Small Enough)}
\description{
    Converts a LinkedMatrix Instance to a Matrix (if Small Enough).
}
\usage{
\method{as.matrix}{LinkedMatrix}(x, ...)
}
\arguments{
    \item{x}{
        Either a \code{ColumnLinkedMatrix} or a \code{RowLinkedMatrix} object.
    }
    \item{...}{
        Additional arguments (unused).
    }
}
\value{
    A matrix.
}
\seealso{
    \code{\link{ColumnLinkedMatrix-class}},
    \code{\link{RowLinkedMatrix-class}}, and \code{\link{LinkedMatrix-class}}
    for more information on the \code{ColumnLinkedMatrix},
    \code{RowLinkedMatrix}, and \code{LinkedMatrix} classes.
}
