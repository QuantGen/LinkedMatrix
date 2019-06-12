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
\item{x}{Either a \linkS4class{ColumnLinkedMatrix} or a \linkS4class{RowLinkedMatrix}
object.}

\item{...}{Additional arguments (unused).}
}
\value{
A matrix.
}
