\docType{class}
\name{LinkedMatrix-class}
\alias{LinkedMatrix-class}
\alias{initialize,LinkedMatrix-method}
\alias{show,LinkedMatrix-method}
\title{A Class Union of ColumnLinkedMatrix and RowLinkedMatrix}
\description{
    This class is abstract and no objects can be created from it. It can be
    used to check whether an object is either of type \code{ColumnLinkedMatrix}
    or of type \code{RowLinkedMatrix} using \code{is(x, "LinkedMatrix")} and to
    assign methods for both \code{ColumnLinkedMatrix} and
    \code{RowLinkedMatrix} classes, e.g. \code{show}.
}
\section{Methods}{
    \itemize{
        \item \code{length}
        \item \code{as.matrix}
        \item \code{show}
        \item \code{initialize}
    }
}
\seealso{
    \code{\link{ColumnLinkedMatrix-class}} and
    \code{\link{RowLinkedMatrix-class}} for implementations of column-linked
    and row-linked matrices.
}
\examples{
# Create an example RowLinkedMatrix from various matrix-like objects that
# correspond in dimensions
m <- RowLinkedMatrix(
    ff::ff(initdata = rnorm(50), dim = c(5, 10)),
    bigmemory::big.matrix(init = rnorm(50), nrow = 5, ncol = 10),
    matrix(data = rnorm(50), nrow = 5, ncol = 10)
)

# Test if m is an object of either type ColumnLinkedMatrix or RowLinkedMatrix
if (is(m, "LinkedMatrix")) {
    message("m is a LinkedMatrix")
}
}
\keyword{classes}
