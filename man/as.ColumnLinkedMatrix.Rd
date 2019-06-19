\name{as.ColumnLinkedMatrix}
\alias{as.ColumnLinkedMatrix}
\alias{as.ColumnLinkedMatrix.list}
\alias{as.RowLinkedMatrix}
\alias{as.RowLinkedMatrix.list}
\title{Converts an Object to a LinkedMatrix Object}
\description{
    Converts an Object to a LinkedMatrix Object.
}
\usage{
as.ColumnLinkedMatrix(x, ...)

\method{as.ColumnLinkedMatrix}{list}(x, ...)

as.RowLinkedMatrix(x, ...)

\method{as.RowLinkedMatrix}{list}(x, ...)
}
\arguments{
    \item{x}{
        An object to convert to a \code{LinkedMatrix} object.
    }
    \item{...}{
        Additional arguments.
    }
}
\value{
    A \code{LinkedMatrix} object.
}
\seealso{
    \code{\link{ColumnLinkedMatrix-class}}, \code{\link{RowLinkedMatrix-class}},
    and \code{\link{LinkedMatrix-class}} for more information on the
    \code{ColumnLinkedMatrix}, \code{RowLinkedMatrix}, and \code{LinkedMatrix}
    classes.
}
\examples{
m1 <- ff::ff(initdata = rnorm(50), dim = c(5, 10))
m2 <- bigmemory::big.matrix(init = rnorm(50), nrow = 5, ncol = 10)
m3 <- matrix(data = rnorm(50), nrow = 5, ncol = 10)

myList <- list(m1, m2, m3)

m <- as.ColumnLinkedMatrix(myList)
}
