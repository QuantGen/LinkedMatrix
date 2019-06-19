\name{index}
\alias{index}
\title{
    Maps Each Column or Row Index of a Linked Matrix to the Column or Row Index
    of Its Corresponding Node
}
\description{
    If \code{j} for \code{ColumnLinkedMatrix} or \code{i} for
    \code{RowLinkedMatrix} is passed, it will only generate entries for the
    given indices. \code{sort}, which is set by default, determines whether
    \code{j} or \code{i} should be sorted before building the index.
}
\usage{
index(x, ...)
}
\arguments{
    \item{x}{
        Either a \code{ColumnLinkedMatrix} or a \code{RowLinkedMatrix} object.
    }
    \item{...}{
        Additional arguments (see Details).
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
