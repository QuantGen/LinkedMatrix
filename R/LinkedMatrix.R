#' @include ColumnLinkedMatrix.R RowLinkedMatrix.R
NULL


#' Initializes either a \code{\linkS4class{ColumnLinkedMatrix}} or
#' \code{\linkS4class{RowLinkedMatrix}} of certain dimensions and of certain
#' type.
#' 
#' @param nrow The number of rows.
#' @param ncol The number of columns.
#' @param nNodes The number of nodes.
#' @param linkedBy Whether the matrix is linked by \code{rows} or
#'   \code{columns}.
#' @param nodeInitializer The name of a function or a function with four
#'   parameters \code{nodeIndex}, \code{ncol}, \code{nrow}, and \code{...} that
#'   initializes each node by returning a matrix-like node. Pre-defined node
#'   initializers include \code{matrixNodeInitializer}.
#' @param ... Additional arguments passed into \code{nodeInitializer}.
#' @export
LinkedMatrix <- function(nrow, ncol, nNodes, linkedBy, nodeInitializer, ...) {
    class <- ifelse(linkedBy == "columns", "ColumnLinkedMatrix", "RowLinkedMatrix")
    # Look for an internal function first
    ex <- try(nodeInitializer <- get(nodeInitializer), silent = TRUE)
    if (class(ex) == "try-error") {
        nodeInitializer <- match.fun(nodeInitializer)
    }
    linkedMatrix <- new(class)
    ranges <- chunkRanges(ifelse(class == "ColumnLinkedMatrix", ncol, nrow), nNodes)
    for (i in seq_len(nNodes)) {
        if (class == "RowLinkedMatrix") {
            n <- ranges[2, i] - ranges[1, i] + 1
            p <- ncol
        } else {
            n <- nrow
            p <- ranges[2, i] - ranges[1, i] + 1
        }
        linkedMatrix[[i]] <- nodeInitializer(nodeIndex = i, nrow = n, ncol = p, ...)
    }
    return(linkedMatrix)
}


matrixNodeInitializer <- function(nodeIndex, nrow, ncol, ...) {
    matrix(nrow = nrow, ncol = ncol, ...)
}


ffNodeInitializer <- function(nodeIndex, nrow, ncol, vmode, ...) {
    if (!requireNamespace("ff", quietly = TRUE)) {
        stop("The ff package is needed for this function to work. Please install it.", call. = FALSE)
    }
    ff::ff(dim = c(nrow, ncol), vmode = vmode, ...)
}


show <- function(object) {
    d <- dim(object)
    cat(d[1], "x", d[2], "linked matrix of class", class(object), "\n")
}


#' @export
length.LinkedMatrix <- function(x) {
    prod(dim(x))
}


#' Returns the number of nodes.
#' 
#' @param x Either a \code{\linkS4class{ColumnLinkedMatrix}} or a 
#'   \code{\linkS4class{RowLinkedMatrix}} object
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
