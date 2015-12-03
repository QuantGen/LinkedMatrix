#' @include ColumnLinkedMatrix.R RowLinkedMatrix.R
NULL


show <- function(object) {
    d <- dim(object)
    cat(d[1], "x", d[2], "linked matrix of class", class(object), "\n")
}


#' @export
length.LinkedMatrix <- function(x) {
    prod(dim(x))
}


#' @export
nNodes <- function(x) {
    length(slot(x, ".Data"))
}


#' Returns the column or row indexes at which each node starts and ends.
#' 
#' @param x Either a \code{\linkS4class{ColumnLinkedMatrix}} or a 
#'   \code{\linkS4class{RowLinkedMatrix}} object
#' @return A matrix.
#' @export
nodes <- function(x) {
    UseMethod("nodes")
}


#' Maps each column or row index of a linked matrix to the column or row index
#' of its corresponding node.
#' 
#' @param x Either a \code{\linkS4class{ColumnLinkedMatrix}} or a 
#'   \code{\linkS4class{RowLinkedMatrix}} object
#' @return A matrix.
#' @export
index <- function(x) {
    UseMethod("index")
}


#' An abstract S4 class to represent linked matrices.
#' 
#' @name LinkedMatrix-class
#' @docType class
#' @seealso \code{\linkS4class{ColumnLinkedMatrix}} or
#'   \code{\linkS4class{RowLinkedMatrix}} for implementations of column-linked
#'   matrices or row-linked matrices, respectively.
#' @exportClass LinkedMatrix
setClassUnion("LinkedMatrix", c("ColumnLinkedMatrix", "RowLinkedMatrix"))


#' Show a LinkedMatrix object.
#' 
#' @param object Either a \code{\linkS4class{ColumnLinkedMatrix}} or a 
#'   \code{\linkS4class{RowLinkedMatrix}} object.
#' @export
setMethod("show", signature(object = "LinkedMatrix"), show)
