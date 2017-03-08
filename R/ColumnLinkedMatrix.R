subset.ColumnLinkedMatrix <- function(x, i, j, ..., drop = TRUE) {
    # Check indices and dimensions
    nX <- nrow(x)
    pX <- ncol(x)
    if (missing(i)) {
        i <- 1:nX
    }
    if (missing(j)) {
        j <- 1:pX
    }
    if (class(i) == "logical") {
        i <- rep_len(i, nX)
        i <- which(i)
    } else if (class(i) == "character") {
        i <- match(i, rownames(x))
    }
    if (class(j) == "logical") {
        j <- rep_len(j, pX)
        j <- which(j)
    } else if (class(j) == "character") {
        j <- match(j, colnames(x))
    }
    n <- length(i)
    p <- length(j)
    if (p > pX || n > nX) {
        stop("Either the number of columns or number of rows requested exceed the number of rows or columns in x, try dim(x)...")
    }
    # Providing a sorted column index will eliminate the need to reorder the
    # result matrix later (avoiding a copy)
    isUnsorted <- is.unsorted(j)
    if (isUnsorted) {
        # Reorder columns for sequential retrieval by node
        originalOrder <- rank(j, ties.method = "first")
        sortedColumns <- sort(j)
    } else {
        sortedColumns <- j
    }
    # Compute node inventory
    globalIndex <- index(x, sortedColumns)
    whichNodes <- unique(globalIndex[, 1])
    # If there are several nodes involved, aggregate the result in a separate
    # matrix, otherwise pass through result
    if (length(whichNodes) > 1) {
        # Initialize result matrix as integer matrix because it does not take up as
        # much space as double() but is more useful than logical()
        Z <- matrix(data = integer(), nrow = n, ncol = p)
        # Use dimnames instead of rownames and colnames to avoid copy
        dimnames(Z) <- list(rownames(x)[i], colnames(x)[sortedColumns])
        end <- 0
        for (k in whichNodes) {
            localIndex <- globalIndex[globalIndex[, 1] == k, , drop = FALSE]
            ini <- end + 1
            end <- ini + nrow(localIndex) - 1
            # Convert to matrix to support data frames
            Z[, ini:end] <- as.matrix(x[[k]][i, localIndex[, 3], drop = FALSE])
        }
    } else {
        Z <- as.matrix(x[[whichNodes]][i, globalIndex[, 3], drop = FALSE])
    }
    if (isUnsorted) {
        # Return to original order
        Z <- Z[, originalOrder, drop = FALSE]
    }
    if (drop == TRUE && (n == 1 || p == 1)) {
        # Let R handle drop behavior
        return(Z[, ])
    } else {
        return(Z)
    }
}


replace.ColumnLinkedMatrix <- function(x, i, j, ..., value) {
    if (missing(i)) {
        i <- 1:nrow(x)
    }
    if (missing(j)) {
        j <- 1:ncol(x)
    }
    Z <- matrix(nrow = length(i), ncol = length(j), data = value)
    # Retrieve nodes and index from ... to speed up sequential writes
    dotdotdot <- list(...)
    if (is.null(dotdotdot$nodes)) {
        nodes <- nodes(x)
    } else {
        nodes <- dotdotdot$nodes
    }
    if (is.null(dotdotdot$index)) {
        index <- index(x)
    } else {
        index <- dotdotdot$index
    }
    for (k in 1:nrow(nodes)) {
        col_z <- (j >= nodes[k, 2]) & (j <= nodes[k, 3])
        colLocal <- index[j[col_z], 3]
        x[[k]][i, colLocal] <- Z[, col_z]
    }
    return(x)
}


#' @export
dim.ColumnLinkedMatrix <- function(x) {
    n <- nrow(x[[1]])
    p <- 0L
    for (i in 1:nNodes(x)) {
        p <- p + ncol(x[[i]])
    }
    return(c(n, p))
}


# This function looks like an S3 method, but isn't one.
rownames.ColumnLinkedMatrix <- function(x) {
    out <- rownames(x[[1]])
    return(out)
}


# This function looks like an S3 method, but isn't one.
colnames.ColumnLinkedMatrix <- function(x) {
    out <- NULL
    if (!is.null(colnames(x[[1]]))) {
        p <- dim(x)[2]
        out <- rep("", p)
        nodes <- nodes(x)
        for (i in 1:nrow(nodes)) {
            out[(nodes[i, 2]:nodes[i, 3])] <- colnames(x[[i]])
        }
    }
    return(out)
}


#' @export
dimnames.ColumnLinkedMatrix <- function(x) {
    list(rownames.ColumnLinkedMatrix(x), colnames.ColumnLinkedMatrix(x))
}


# This function looks like an S3 method, but isn't one.
`rownames<-.ColumnLinkedMatrix` <- function(x, value) {
    for (i in 1:nNodes(x)) {
        rownames(x[[i]]) <- value
    }
    return(x)
}


# This function looks like an S3 method, but isn't one.
`colnames<-.ColumnLinkedMatrix` <- function(x, value) {
    nodes <- nodes(x)
    for (i in 1:nrow(nodes)) {
        colnames(x[[i]]) <- value[(nodes[i, 2]:nodes[i, 3])]
    }
    return(x)
}


#' @export
`dimnames<-.ColumnLinkedMatrix` <- function(x, value) {
    d <- dim(x)
    rownames <- value[[1]]
    colnames <- value[[2]]
    if (!is.list(value) || length(value) != 2 || !(is.null(rownames) || length(rownames) == d[1]) || !(is.null(colnames) ||
        length(colnames) == d[2])) {
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
cbind.ColumnLinkedMatrix <- function(..., deparse.level = 0) {
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
rbind.ColumnLinkedMatrix <- function(..., deparse.level = 1) {
    stop("rbind is currently undefined for ColumnLinkedMatrix")
}


#' @export
nodes.ColumnLinkedMatrix <- function(x) {
    n <- nNodes(x)
    nodes <- matrix(integer(), nrow = n, ncol = 3, dimnames = list(NULL, c("node", "col.ini", "col.end")))
    end <- 0L
    for (node in 1:n) {
        ini <- end + 1L
        end <- ini + ncol(x[[node]]) - 1L
        nodes[node, ] <- c(node, ini, end)
    }
    return(nodes)
}


#' @export
index.ColumnLinkedMatrix <- function(x, j = NULL, ...) {
    nodes <- nodes(x)
    p <- nodes[nrow(nodes), 3]
    if (!is.null(j)) {
        j <- as.integer(j)
        if (is.unsorted(j)) {
            j <- sort(j)
        }
    } else {
        j <- seq_len(p)
    }
    index <- matrix(data = integer(), nrow = length(j), ncol = 3, dimnames = list(NULL, c("node", "col.global", "col.local")))
    index[, 2] <- j
    for (node in 1:nrow(nodes)) {
        globalIdx <- which(j >= nodes[node, 2] & j <= nodes[node, 3])
        if (node > 1) {
            localIdx <- j[globalIdx] - nodes[node - 1, 3]
        } else {
            localIdx <- j[globalIdx]
        }
        index[globalIdx, 1] <- node
        index[globalIdx, 3] <- localIdx
    }
    return(index)
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
#' `ColumnLinkedMatrix` or `RowLinkedMatrix` object, [LinkedMatrix()] to create
#' an empty, prespecified `LinkedMatrix` object, [nNodes()] to get the number
#' of nodes of a `LinkedMatrix` object.
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
    if (length(nodes) == 0) {
        nodes[[1]] <- matrix()
    } else {
        # Stop if matrices are not matrix-like
        if (any(sapply(nodes, function(x) length(dim(x)) != 2))) {
            stop("arguments need to be matrix-like")
        }
        # Stop if dimensions of matrices do not match
        if (length(unique(sapply(nodes, nrow))) != 1) {
            stop("arguments need the same number of rows")
        }
        # Warn if rownames of matrices do not match
        names <- lapply(nodes, rownames)
        names <- names[!sapply(names, is.null)]
        if (length(names) > 1 && !all(duplicated(names) | duplicated(names, fromLast = TRUE))) {
            warning("row names of matrix-like objects do not match")
        }
    }
    .Object <- callNextMethod(.Object, nodes)
    return(.Object)
})


#' @export
`[.ColumnLinkedMatrix` <- subset.ColumnLinkedMatrix


#' @export
`[<-.ColumnLinkedMatrix` <- replace.ColumnLinkedMatrix
