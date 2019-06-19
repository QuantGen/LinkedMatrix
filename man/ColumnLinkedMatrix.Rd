\name{ColumnLinkedMatrix}
\alias{ColumnLinkedMatrix}
\alias{RowLinkedMatrix}
\title{Create a LinkedMatrix Object}
\description{
    This function constructs a new \code{ColumnLinkedMatrix} or
    \code{RowLinkedMatrix} object from a list of matrix-like objects.
}
\usage{
ColumnLinkedMatrix(...)

RowLinkedMatrix(...)
}
\arguments{
    \item{...}{
        A sequence of matrix-like objects of the same row-dimension (for
        \code{ColumnLinkedMatrix}) or column-dimension (for
        \code{RowLinkedMatrix}).
    }
}
\details{
    A matrix-like object is one that has two dimensions and implements at least
    \code{dim} and \code{[}. Each object needs to have the same number of rows
    (for \code{ColumnLinkedMatrix}) or columns (for \code{RowLinkedMatrix}) to
    be linked together. If no matrix-like objects are given, a single 1x1 node
    of type \code{matrix} filled with \code{NA} is returned.
    \code{LinkedMatrix} objects can be nested as long as they are conformable.
}
\value{
    Either a \code{ColumnLinkedMatrix} or a \code{RowLinkedMatrix} object.
}
\seealso{
    \code{\link{LinkedMatrix}} to create an empty, prespecified
    \code{LinkedMatrix} object.
}
\examples{
# Create various matrix-like objects that correspond in dimensions
m1 <- ff::ff(initdata = rnorm(50), dim = c(5, 10))
m2 <- bigmemory::big.matrix(init = rnorm(50), nrow = 5, ncol = 10)
m3 <- matrix(data = rnorm(50), nrow = 5, ncol = 10)

# Create a ColumnLinkedMatrix object
cm <- ColumnLinkedMatrix(m1, m2, m3)

# To specify the matrix-like objects as a list, use the `do.call` function
rm <- do.call(RowLinkedMatrix, list(m1, m2, m3))
}
