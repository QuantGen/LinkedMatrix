#' @include ColumnLinkedMatrix.R RowLinkedMatrix.R
NULL


#' Initializes Either a ColumnLinkedMatrix or RowLinkedMatrix Instance of
#' Certain Dimensions with a Configurable Number and Type of Nodes.
#'
#' @param nrow The number of rows.
#' @param ncol The number of columns.
#' @param nNodes The number of nodes.
#' @param linkedBy Whether the matrix is linked by `rows` or `columns`.
#' @param nodeInitializer The name of a function or a function with four
#' parameters `nodeIndex`, `ncol`, `nrow`, and `...` that initializes each node
#' by returning a matrix-like object Pre-defined node initializers include
#' `matrixNodeInitializer` to initialize matrices and `ffNodeInitializer` to
#' initialize `ff` objects.
#' @param ... Additional arguments passed into `nodeInitializer`.
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


#' Converts a LinkedMatrix Instance to a Matrix (if Small Enough).
#'
#' @param x Either a [ColumnLinkedMatrix-class] or a [RowLinkedMatrix-class]
#' object.
#' @param ... Additional arguments (unused).
#' @export
as.matrix.LinkedMatrix <- function(x, ...) {
    x[, , drop = FALSE]
}


#' Returns the Number of Nodes.
#'
#' @param x Either a [ColumnLinkedMatrix-class] or a [RowLinkedMatrix-class]
#' object.
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
#' passed, it will only generate entries for the given indices.
#'
#' @param x Either a [ColumnLinkedMatrix-class] or a [RowLinkedMatrix-class]
#' object.
#' @param ... Additional arguments (see Details).
#' @return A matrix.
#' @export
index <- function(x, ...) {
    UseMethod("index")
}


#' An Abstract S4 Class Union of ColumnLinkedMatrix and RowLinkedMatrix.
#'
#' This class is a class union and can therefore not be initialized. It can be
#' used to check whether an object is either of type [ColumnLinkedMatrix-class]
#' or of type [RowLinkedMatrix-class] using `is(x, "LinkedMatrix")` and to
#' assign methods for both [ColumnLinkedMatrix-class] and
#' [RowLinkedMatrix-class] classes, e.g.  `show`.
#'
#' @name LinkedMatrix-class
#' @docType class
#' @seealso [ColumnLinkedMatrix-class] or [RowLinkedMatrix-class] for
#' implementations of column-linked matrices or row-linked matrices,
#' respectively.
#' @exportClass LinkedMatrix
setClassUnion("LinkedMatrix", c("ColumnLinkedMatrix", "RowLinkedMatrix"))


#' Show a LinkedMatrix Object.
#'
#' This method is run when a [LinkedMatrix-class] object is printed.
#'
#' @param object Either a [ColumnLinkedMatrix-class] or a
#' [RowLinkedMatrix-class] object.
#' @export
setMethod("show", signature(object = "LinkedMatrix"), show)
