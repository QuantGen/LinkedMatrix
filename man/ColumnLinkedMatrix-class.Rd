\docType{class}
\name{ColumnLinkedMatrix-class}
\alias{ColumnLinkedMatrix-class}
\alias{RowLinkedMatrix-class}
\title{A Class for Linking Matrices by Columns or Rows}
\description{
    This class treats a list of matrix-like objects that are linked together by
    columns (\code{ColumnLinkedMatrix}) or rows (\code{RowLinkedMatrix}) and
    have the same number of rows similarly to a regular \code{matrix} by
    implementing key methods such as \code{[} and \code{[<-} for extracting and
    replacing matrix elements, \code{dim} to retrieve dimensions, and
    \code{dimnames} and \code{dimnames<-} to retrieve and set dimnames. Each
    list element is called a node and can be extracted or replaced using
    \code{[[} and \code{[[<-}. A matrix-like object is one that has two
    dimensions and implements at least \code{dim} and \code{[}.
}
\details{
    Internally, this class is an S4 class that contains \code{list}. Each node
    can be accessed using the \code{[[} operator. \code{lapply} is also
    possible.  \code{ColumnLinkedMatrix} and \code{RowLinkedMatrix} form a
    class union called \code{LinkedMatrix}.
}
\section{Methods}{
  \itemize{
      \item \code{[}
      \item \code{[<-}
      \item \code{dim}
      \item \code{dimnames}
      \item \code{dimnames<-}
      \item \code{as.matrix}
      \item \code{is.matrix}
      \item \code{length}
      \item \code{print}
      \item \code{str}
      \item \code{cbind} (for \code{ColumnLinkedMatrix})
      \item \code{rbind} (for \code{RowLinkedMatrix})
  }
}
\seealso{
    \code{\link{ColumnLinkedMatrix}} and \code{\link{RowLinkedMatrix}} to
    create a \code{ColumnLinkedMatrix} and \code{RowLinkedMatrix} objects from
    scratch.  \code{\link{as.ColumnLinkedMatrix}} and
    \code{\link{as.RowLinkedMatrix}} to create a \code{ColumnLinkedMatrix} and
    \code{RowLinkedMatrix} objects from other objects.
    \code{\link{LinkedMatrix}} to create an empty, prespecified
    \code{LinkedMatrix} object.  \code{\link{nNodes}} to get the number of
    nodes of a \code{LinkedMatrix} object.
}
\examples{
# Create various matrix-like objects that correspond in dimensions
m1 <- ff::ff(initdata = rnorm(50), dim = c(5, 10))
m2 <- bigmemory::big.matrix(init = rnorm(50), nrow = 5, ncol = 10)
m3 <- matrix(data = rnorm(50), nrow = 5, ncol = 10)

# Link random matrices by columns
cm <- ColumnLinkedMatrix(m1, m2, m3)
dim(cm)

# Link random matrices by rows
rm <- RowLinkedMatrix(m1, m2, m3)
dim(rm)

# Get the number of nodes of each linked matrix
nNodes(cm)
nNodes(rm)

# Extract specific rows of linked matrix
cm[1, ]
cm[1:3, ]
rm[1, ]
rm[1:3, ]

# Extract specific columns of linked matrix
cm[, 1]
cm[, 1:3]
rm[, 1]
rm[, 1:3]

# Extract specific rows and columns of linked matrix
cm[1, 1]
cm[1:3, 1:3]
rm[1, 1]
rm[1:3, 1:3]

# Get a reference to one of the nodes
n <- cm[[2]]
class(n) == "big.matrix"

# LinkedMatrix objects are matrix-like and can be nested
rcm <- RowLinkedMatrix(cm, cm)
}
\keyword{classes}
