\name{nNodes}
\alias{nNodes}
\title{Returns the Number of Nodes}
\description{
    Returns the number of nodes.
}
\usage{
nNodes(x)
}
\arguments{
    \item{x}{
        Either a \code{ColumnLinkedMatrix} or a \code{RowLinkedMatrix} object.
    }
}
\value{
    The number of nodes.
}
\seealso{
    \code{\link{ColumnLinkedMatrix-class}},
    \code{\link{RowLinkedMatrix-class}}, and \code{\link{LinkedMatrix-class}}
    for more information on the \code{ColumnLinkedMatrix},
    \code{RowLinkedMatrix}, and \code{LinkedMatrix} classes.
}
\examples{
# Create an example RowLinkedMatrix from various matrix-like objects that
# correspond in dimensions
m <- RowLinkedMatrix(
    ff::ff(initdata = rnorm(50), dim = c(5, 10)),
    bigmemory::big.matrix(init = rnorm(50), nrow = 5, ncol = 10),
    matrix(data = rnorm(50), nrow = 5, ncol = 10)
)

# Get the number of nodes of the RowLinkedMatrix
nNodes(m)
}
