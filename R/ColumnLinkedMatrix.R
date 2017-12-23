extract_matrix.ColumnLinkedMatrix <- function(x, i, j, ...) {
    # Handle x[, FALSE]
    if (length(j) == 0L) {
        Z <- matrix(data = integer(), nrow = length(i), ncol = 0L, dimnames = list(rownames(x)[i], NULL))
    } else {
        # Determine nodes and node boundaries for query
        index <- index(x, j = j, sort = FALSE)
        nodeList <- unique(index[, 1L])
        # If there are several nodes involved in the query, aggregate the
        # result in a separate matrix, otherwise pass through result
        if (length(nodeList) > 1L) {
            # Initialize result matrix as integer() because it does not take up
            # as much space as double() but is more common than logical()
            Z <- matrix(data = integer(), nrow = length(i), ncol = length(j), dimnames = list(rownames(x)[i], colnames(x)[j]))
            for (curNode in nodeList) {
                if (is.na(curNode)) {
                    nodeIndex = is.na(index[, 1L])
                    Z[, nodeIndex] <- NA_integer_
                } else {
                    nodeIndex <- index[, 1L] == curNode
                    nodeIndex[is.na(nodeIndex)] <- FALSE
                    # Convert to matrix to support data frames
                    Z[, nodeIndex] <- as.matrix(x[[curNode]][i, index[nodeIndex, 3L], drop = FALSE])
                }
            }
        } else {
            # Handle x[, NA] or out of bounds
            if (is.na(nodeList)) {
                Z <- matrix(data = NA_integer_, nrow = length(i), ncol = length(j), dimnames = list(rownames(x)[i], rep(NA_character_, length(j))))
            } else {
                # Convert to matrix to support data frames
                Z <- as.matrix(x[[nodeList]][i, index[, 3L], drop = FALSE])
            }
        }
    }
    return(Z)
}


extract_vector.ColumnLinkedMatrix <- function(x, i, ...) {
    if (length(i) == 0L) {
        Z <- integer(0L)
    } else {
        # Determine nodes and node boundaries for query (in this case index()
        # cannot be used as it is easier to go by the number of elements per
        # block rather than the number of columns because of column-major
        # nature of the matrix)
        # Note that length() could be used here, but it is not required as per
        # our definition of a matrix-like object.
        elementsPerNode <- apply(sapply(x, dim), 2, prod)
        nodeBoundaries <- c(0L, cumsum(elementsPerNode))
        nodeMembership <- .bincode(i, nodeBoundaries)
        nodeList <- unique(nodeMembership)
        # If there are several nodes involved in the query, aggregate the
        # result in a separate matrix, otherwise pass through result
        if (length(nodeList) > 1L) {
            # Initialize result vector as integer() because it does not take up
            # as much space as double() but is more common than logical()
            Z <- vector(mode = "integer", length = length(i))
            for (curNode in nodeList) {
                if (is.na(curNode)) {
                    nodeIndex <- is.na(nodeMembership)
                    Z[nodeIndex] <- NA_integer_
                } else {
                    nodeIndex <- nodeMembership == curNode
                    nodeIndex[is.na(nodeIndex)] <- FALSE
                    localIndex <- i[nodeIndex] - nodeBoundaries[curNode]
                    Z[nodeIndex] <- x[[curNode]][localIndex]
                }
            }
        } else {
            # Handle x[NA] or out of bounds
            if (is.na(nodeList)) {
                Z <- rep(NA_integer_, length(i))
            } else {
                Z <- x[[nodeList]][i - nodeBoundaries[nodeList]]
            }
        }
    }
    return(Z)
}


replace_matrix.ColumnLinkedMatrix <- function(x, i, j, ..., value) {
    # Convert value vector to matrix
    dim(value) <- c(length(i), length(j))
    # Determine node boundaries
    index <- index(x, j = j, sort = FALSE)
    nodeList <- unique(index[, 1L])
    # Replace elements in each node
    for (curNode in nodeList) {
        nodeIndex <- index[, 1L] == curNode
        x[[curNode]][i, index[nodeIndex, 3L]] <- value[, nodeIndex]
    }
    return(x)
}


replace_vector.ColumnLinkedMatrix <- function(x, i, ..., value) {
    # Determine nodes and node boundaries for query (in this case index()
    # cannot be used as it is easier to go by the number of elements per
    # block rather than the number of columns because of column-major
    # nature of the matrix)
    # Note that length() could be used here, but it is not required as per
    # our definition of a matrix-like object.
    elementsPerNode <- apply(sapply(x, dim), 2, prod)
    nodeBoundaries <- c(0L, cumsum(elementsPerNode))
    nodeMembership <- .bincode(i, nodeBoundaries)
    nodeList <- unique(nodeMembership)
    # Replace elements in each node
    for (curNode in nodeList) {
        nodeIndex <- nodeMembership == curNode
        localIndex <- i[nodeIndex] - nodeBoundaries[curNode]
        x[[curNode]][localIndex] <- value[nodeIndex]
    }
    return(x)
}


#' @export
dim.ColumnLinkedMatrix <- function(x) {
    n <- nrow(x[[1L]])
    p <- 0L
    for (i in 1L:nNodes(x)) {
        p <- p + ncol(x[[i]])
    }
    return(c(n, p))
}


# This function looks like an S3 method, but isn't one.
rownames.ColumnLinkedMatrix <- function(x) {
    rownames(x[[1L]])
}


# This function looks like an S3 method, but isn't one.
colnames.ColumnLinkedMatrix <- function(x) {
    names <- NULL
    if (!is.null(colnames(x[[1L]]))) {
        p <- dim(x)[2L]
        names <- rep("", p)
        nodes <- nodes(x)
        for (i in 1L:nrow(nodes)) {
            names[(nodes[i, 2L]:nodes[i, 3L])] <- colnames(x[[i]])
        }
    }
    return(names)
}


#' @export
dimnames.ColumnLinkedMatrix <- function(x) {
    list(rownames.ColumnLinkedMatrix(x), colnames.ColumnLinkedMatrix(x))
}


# This function looks like an S3 method, but isn't one.
`rownames<-.ColumnLinkedMatrix` <- function(x, value) {
    for (i in 1L:nNodes(x)) {
        rownames(x[[i]]) <- value
    }
    return(x)
}


# This function looks like an S3 method, but isn't one.
`colnames<-.ColumnLinkedMatrix` <- function(x, value) {
    nodes <- nodes(x)
    for (i in 1L:nrow(nodes)) {
        colnames(x[[i]]) <- value[(nodes[i, 2L]:nodes[i, 3L])]
    }
    return(x)
}


#' @export
`dimnames<-.ColumnLinkedMatrix` <- function(x, value) {
    d <- dim(x)
    rownames <- value[[1L]]
    colnames <- value[[2L]]
    if (!is.list(value) || length(value) != 2L || !(is.null(rownames) || length(rownames) == d[1L]) || !(is.null(colnames) ||
        length(colnames) == d[2L])) {
        stop("invalid dimnames")
    }
    x <- `rownames<-.ColumnLinkedMatrix`(x, rownames)
    x <- `colnames<-.ColumnLinkedMatrix`(x, colnames)
    return(x)
}


#' Combine Matrix-Like Objects by Columns.
#'
#' Compared to the [initialize()][initialize,ColumnLinkedMatrix-method()]
#' method, nested [LinkedMatrix-class] objects that are passed via `...` will
#' not be treated as matrix-like objects, but their nodes will be extracted and
#' merged with the new [ColumnLinkedMatrix-class] object for a more compact
#' representation. This method will currently only work for
#' [ColumnLinkedMatrix-class] objects.
#'
#' @param ... Matrix-like objects to be combined by columns.
#' @param deparse.level Currently unused, defaults to 0.
#' @export
cbind.ColumnLinkedMatrix <- function(..., deparse.level = 0L) {
    dotdotdot <- list(...)
    nodes <- list()
    for (i in seq_len(length(dotdotdot))) {
        node <- dotdotdot[[i]]
        if (is(node, "LinkedMatrix")) {
            # Extract nodes from LinkedMatrix object
            nodes <- append(nodes, slot(node, ".Data"))
        } else {
            nodes <- append(nodes, node)
        }
    }
    do.call(ColumnLinkedMatrix, nodes)
}


#' @rdname rbind.RowLinkedMatrix
#' @export
rbind.ColumnLinkedMatrix <- function(..., deparse.level = 1L) {
    stop("rbind is currently undefined for ColumnLinkedMatrix")
}


#' @export
nodes.ColumnLinkedMatrix <- function(x) {
    colsPerNode <- sapply(x, ncol)
    colUpperBoundaries <- cumsum(colsPerNode)
    colLowerBoundaries <- colUpperBoundaries - colsPerNode + 1
    n <- length(colsPerNode)
    nodes <- matrix(data = c(1:n, colLowerBoundaries, colUpperBoundaries), nrow = n, ncol = 3L, dimnames = list(NULL, c("node", "col.ini", "col.end")))
    return(nodes)
}


#' @export
index.ColumnLinkedMatrix <- function(x, j = NULL, sort = TRUE, ...) {
    nodes <- nodes(x)
    if (!is.null(j)) {
        j <- as.integer(j)
        if (sort) {
            j <- sort(j)
        }
    } else {
        j <- seq_len(nodes[nrow(nodes), 3L])
    }
    nodeBoundaries <- c(0L, nodes[, 3L])
    nodeMembership <- .bincode(j, breaks = nodeBoundaries)
    index <- matrix(data = c(nodeMembership, j, j - nodeBoundaries[nodeMembership]), nrow = length(j), ncol = 3L, dimnames = list(NULL, c("node", "col.global", "col.local")))
    return(index)
}


#' Converts an Object to a LinkedMatrix Object.
#'
#' @param x An object to convert to a [LinkedMatrix-class] object.
#' @param ... Additional arguments.
#' @return A [LinkedMatrix-class] object.
#' @example man/examples/as.ColumnLinkedMatrix.R
#' @export
as.ColumnLinkedMatrix <- function(x, ...) {
    UseMethod("as.ColumnLinkedMatrix")
}


#' @rdname as.ColumnLinkedMatrix
#' @export
as.ColumnLinkedMatrix.list <- function(x, ...) {
    do.call(ColumnLinkedMatrix, x, ...)
}


#' A Class for Linking Matrices by Columns or Rows.
#'
#' This class treats a list of matrix-like objects that are linked together by
#' columns (`ColumnLinkedMatrix`) or rows (`RowLinkedMatrix`) and have the same
#' number of rows similarly to a regular `matrix` by implementing key methods
#' such as `[` and `[<-` for extracting and replacing matrix elements, `dim` to
#' retrieve dimensions, and `dimnames` and `dimnames<-` to retrieve and set
#' dimnames. Each list element is called a node and can be extracted or
#' replaced using `[[` and `[[<-`. A matrix-like object is one that has two
#' dimensions and implements at least `dim` and `[`.
#'
#' Internally, this class is an S4 class that contains `list`. Each node can be
#' accessed using the `[[` operator. `lapply` is also possible.
#' `ColumnLinkedMatrix` and `RowLinkedMatrix` form a class union called
#' [LinkedMatrix-class].
#'
#' @section Methods:
#' - `[`
#' - `[<-`
#' - `dim`
#' - `dimnames`
#' - `dimnames<-`
#' - `as.matrix`
#' - `is.matrix`
#' - `length`
#' - `print`
#' - `str`
#' - `cbind` (for `ColumnLinkedMatrix`)
#' - `rbind` (for `RowLinkedMatrix`)
#'
#' @seealso [initialize()][initialize,ColumnLinkedMatrix-method()] to create a
#' `ColumnLinkedMatrix` or `RowLinkedMatrix` object from scratch,
#' [as.ColumnLinkedMatrix()] to create a `ColumnLinkedMatrix` or
#' `RowLinkedMatrix` object from other objects, [LinkedMatrix()] to create an
#' empty, prespecified `LinkedMatrix` object, [nNodes()] to get the number of
#' nodes of a `LinkedMatrix` object.
#' @example man/examples/ColumnLinkedMatrix.R
#' @export ColumnLinkedMatrix
#' @exportClass ColumnLinkedMatrix
ColumnLinkedMatrix <- setClass("ColumnLinkedMatrix", contains = "list")


#' Create a LinkedMatrix Object.
#'
#' This function constructs a new [ColumnLinkedMatrix-class] or
#' [RowLinkedMatrix-class] object from a list of matrix-like objects.
#'
#' A matrix-like object is one that has two dimensions and implements at least
#' `dim` and `[`. Each object needs to have the same number of rows (for
#' `ColumnLinkedMatrix`) or columns (for `RowLinkedMatrix`) to be linked
#' together. If no matrix-like objects are given, a single 1x1 node of type
#' `matrix` filled with `NA` is returned. [LinkedMatrix-class] objects can be
#' nested as long as they are conformable.
#'
#' @inheritParams base::list
#' @param .Object Internal, used by [methods::initialize()] generic.
#' @param ... A sequence of matrix-like objects of the same row-dimension (for
#' `ColumnLinkedMatrix`) or column-dimension (for `RowLinkedMatrix`).
#' @return Either a `ColumnLinkedMatrix` or a `RowLinkedMatrix` object.
#' @seealso [LinkedMatrix()] to create an empty, prespecified
#' [LinkedMatrix-class] object.
#' @example man/examples/initialize.R
#' @export
setMethod("initialize", signature(.Object = "ColumnLinkedMatrix"), function(.Object, ...) {
    nodes <- list(...)
    # Append at least one matrix
    if (length(nodes) == 0L) {
        nodes[[1L]] <- matrix()
    } else {
        # Stop if matrices are not matrix-like
        if (!all(sapply(nodes, crochet:::isMatrixLike))) {
            stop("arguments need to be matrix-like")
        }
        # Stop if dimensions of matrices do not match
        if (length(unique(sapply(nodes, nrow))) != 1L) {
            stop("arguments need the same number of rows")
        }
        # Warn if rownames of matrices do not match
        names <- lapply(nodes, rownames)
        names <- names[!sapply(names, is.null)]
        if (length(names) > 1L && !all(duplicated(names) | duplicated(names, fromLast = TRUE))) {
            warning("row names of matrix-like objects do not match")
        }
    }
    .Object <- callNextMethod(.Object, nodes)
    return(.Object)
})


#' @export
`[.ColumnLinkedMatrix` <- crochet::extract(
    extract_vector = extract_vector.ColumnLinkedMatrix,
    extract_matrix = extract_matrix.ColumnLinkedMatrix,
    allowDoubles = TRUE # this may not be compatible with all matrix-like objects
)


#' @export
`[<-.ColumnLinkedMatrix` <- crochet::replace(
    replace_vector = replace_vector.ColumnLinkedMatrix,
    replace_matrix = replace_matrix.ColumnLinkedMatrix,
    allowDoubles = TRUE # this may not be compatible with all matrix-like objects
)
