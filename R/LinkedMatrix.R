#' @include ColumnLinkedMatrix.R RowLinkedMatrix.R
NULL


#' Create an Empty, Prespecified LinkedMatrix Object.
#'
#' This function creates an empty [LinkedMatrix-class] object of a certain
#' size, a certain number of nodes, and certain types of nodes.
#'
#' @param nrow The number of rows of the whole matrix.
#' @param ncol The number of columns of the whole matrix.
#' @param nNodes The number of nodes.
#' @param linkedBy Whether the matrix is linked by `columns` or `rows`.
#' @param nodeInitializer The name of a function or a function `(nodeIndex,
#' nrow, ncol, ...)` where `nodeIndex` is the index of the node, `nrow` is a
#' partition of the total number of rows, `ncol` is a partition of the total
#' number of columns, and `...` are additional parameters passed into the
#' function. The function is expected to return a matrix-like object of
#' dimensions `nrow` and `ncol`. Pre-defined node initializers include
#' `matrixNodeInitializer` to initialize matrices and `ffNodeInitializer` to
#' initialize `ff` objects.
#' @param ... Additional arguments passed into the `nodeInitializer` function.
#' @return A [ColumnLinkedMatrix-class] object if `linkedBy` is `columns` or a
#' [RowLinkedMatrix-class] object if `linkedBy` is `rows`.
#' @seealso [initialize()][initialize,ColumnLinkedMatrix-method()] to create a
#' [ColumnLinkedMatrix-class] or [RowLinkedMatrix-class] object from a list of
#' matrix-like objects.
#' @example man/examples/LinkedMatrix.R
#' @export
LinkedMatrix <- function(nrow, ncol, nNodes, linkedBy, nodeInitializer, ...) {
    nrow <- as.integer(nrow)
    ncol <- as.integer(ncol)
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
            n <- ranges[2L, i] - ranges[1L, i] + 1L
            p <- ncol
        } else {
            n <- nrow
            p <- ranges[2L, i] - ranges[1L, i] + 1L
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
    cat(d[1L], "x", d[2L], "linked matrix of class", class(object), "\n")
}


#' @export
str.LinkedMatrix <- function(object, ...) {
    show(object)
    for (i in 1:nNodes(object)) {
        d <- dim(object[[i]])
        cat("  * Node ", i, ": ", d[1L], " x ", d[2L], " matrix-like object of class ", class(object[[i]]), "\n", sep = "")
    }
}


#' @export
length.LinkedMatrix <- function(x) {
    prod(dim(x))
}


#' @export
is.matrix.LinkedMatrix <- function(x) {
    TRUE # needed for diag()
}


#' Converts a LinkedMatrix Instance to a Matrix (if Small Enough).
#'
#' @param x Either a [ColumnLinkedMatrix-class] or a [RowLinkedMatrix-class]
#' object.
#' @param ... Additional arguments (unused).
#' @return A matrix.
#' @export
as.matrix.LinkedMatrix <- function(x, ...) {
    x[, , drop = FALSE]
}


#' Returns the Number of Nodes.
#'
#' @param x Either a [ColumnLinkedMatrix-class] or a [RowLinkedMatrix-class]
#' object.
#' @return The number of nodes.
#' @example man/examples/nNodes.R
#' @export
nNodes <- function(x) {
    length(slot(x, ".Data"))
}


#' Returns the Column or Row Indexes at Which Each Node Starts and Ends.
#'
#' @param x Either a [ColumnLinkedMatrix-class] or a [RowLinkedMatrix-class]
#' object.
#' @return A matrix.
#' @export
nodes <- function(x) {
    UseMethod("nodes")
}


#' Maps Each Column or Row Index of a Linked Matrix to the Column or Row Index
#' of Its Corresponding Node.
#'
#' If `j` for [ColumnLinkedMatrix-class] or `i` for [RowLinkedMatrix-class] is
#' passed, it will only generate entries for the given indices. `sort`, which
#' is set by default, determines whether `j` or `i` should be sorted before
#' building the index.
#'
#' @param x Either a [ColumnLinkedMatrix-class] or a [RowLinkedMatrix-class]
#' object.
#' @param ... Additional arguments (see Details).
#' @return A matrix.
#' @export
index <- function(x, ...) {
    UseMethod("index")
}


#' A Class Union of ColumnLinkedMatrix and RowLinkedMatrix.
#'
#' This class is abstract and no objects can be created from it. It can be used
#' to check whether an object is either of type [ColumnLinkedMatrix-class] or
#' of type [RowLinkedMatrix-class] using `is(x, "LinkedMatrix")` and to assign
#' methods for both `ColumnLinkedMatrix` and `RowLinkedMatrix` classes, e.g.
#' `show`.
#'
#' @section Methods:
#' - `length`
#' - `as.matrix`
#' - `show`
#'
#' @seealso [ColumnLinkedMatrix-class] and [RowLinkedMatrix-class] for
#' implementations of column-linked and row-linked matrices, respectively.
#' @example man/examples/LinkedMatrix-class.R
#' @name LinkedMatrix-class
#' @docType class
#' @exportClass LinkedMatrix
setClassUnion("LinkedMatrix", c("ColumnLinkedMatrix", "RowLinkedMatrix"))


#' Show a LinkedMatrix Object.
#'
#' Display the object, by printing, plotting or whatever suits its class.
#'
#' @param object A [LinkedMatrix-class] object.
#' @export
setMethod("show", signature(object = "LinkedMatrix"), show)
