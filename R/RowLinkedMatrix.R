subset.RowLinkedMatrix <- function(x, i, j, ..., drop = TRUE) {
    # Check indices and dimensions
    nX <- nrow(x)
    pX <- ncol(x)
    if (missing(i)) {
        i <- 1L:nX
    }
    if (missing(j)) {
        j <- 1L:pX
    }
    if (typeof(i) == "logical") {
        i <- rep_len(i, nX)
        i <- which(i)
    } else if (typeof(i) == "character") {
        i <- match(i, rownames(x))
    } else if (typeof(i) == "double") {
        i <- as.integer(i)
    }
    if (typeof(j) == "logical") {
        j <- rep_len(j, pX)
        j <- which(j)
    } else if (typeof(j) == "character") {
        j <- match(j, colnames(x))
    } else if (typeof(j) == "double") {
        j <- as.integer(j)
    }
    n <- length(i)
    p <- length(j)
    if (p > pX || n > nX) {
        stop("Either the number of columns or number of rows requested exceed the number of rows or columns in x, try dim(x)...")
    }
    # Providing a sorted row index will eliminate the need to reorder the
    # result matrix later (avoiding a copy)
    isUnsorted <- is.unsorted(i)
    if (isUnsorted) {
        # Reorder rows for sequential retrieval by node
        originalOrder <- rank(i, ties.method = "first")
        sortedRows <- sort(i)
    } else {
        sortedRows <- i
    }
    # Compute node inventory
    globalIndex <- index(x, sortedRows)
    whichNodes <- unique(globalIndex[, 1L])
    # If there are several nodes involved, aggregate the result in a separate
    # matrix, otherwise pass through result
    if (length(whichNodes) > 1L) {
        # Initialize result matrix as integer matrix because it does not take up as
        # much space as double() but is more useful than logical()
        Z <- matrix(data = integer(), nrow = n, ncol = p)
        # Use dimnames instead of rownames and colnames to avoid copy
        dimnames(Z) <- list(rownames(x)[sortedRows], colnames(x)[j])
        end <- 0L
        for (k in whichNodes) {
            localIndex <- globalIndex[globalIndex[, 1L] == k, , drop = FALSE]
            ini <- end + 1L
            end <- ini + nrow(localIndex) - 1L
            # Convert to matrix to support data frames
            Z[ini:end, ] <- as.matrix(x[[k]][localIndex[, 3L], j, drop = FALSE])
        }
    } else {
        Z <- as.matrix(x[[whichNodes]][globalIndex[, 3L], j, drop = FALSE])
    }
    if (isUnsorted) {
        # Return to original order
        Z <- Z[originalOrder, , drop = FALSE]
    }
    if (drop == TRUE && (n == 1L || p == 1L)) {
        # Let R handle drop behavior
        return(Z[, ])
    } else {
        return(Z)
    }
}


replace.RowLinkedMatrix <- function(x, i, j, ..., value) {
    if (missing(i)) {
        i <- 1L:nrow(x)
    }
    if (missing(j)) {
        j <- 1L:ncol(x)
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
    for (k in 1L:nrow(nodes)) {
        rows_z <- (i >= nodes[k, 2L]) & (i <= nodes[k, 3L])
        rowLocal <- index[i[rows_z], 3L]
        x[[k]][rowLocal, j] <- Z[rows_z, ]
    }
    return(x)
}


#' @export
dim.RowLinkedMatrix <- function(x) {
    p <- ncol(x[[1L]])
    n <- 0L
    for (i in 1L:nNodes(x)) {
        n <- n + nrow(x[[i]])
    }
    return(c(n, p))
}


# This function looks like an S3 method, but isn't one.
rownames.RowLinkedMatrix <- function(x) {
    names <- NULL
    if (!is.null(rownames(x[[1L]]))) {
        n <- dim(x)[1L]
        names <- rep("", n)
        nodes <- nodes(x)
        for (i in 1L:nrow(nodes)) {
            names[(nodes[i, 2L]:nodes[i, 3L])] <- rownames(x[[i]])
        }
    }
    return(names)
}


# This function looks like an S3 method, but isn't one.
colnames.RowLinkedMatrix <- function(x) {
    colnames(x[[1L]])
}


#' @export
dimnames.RowLinkedMatrix <- function(x) {
    list(rownames.RowLinkedMatrix(x), colnames.RowLinkedMatrix(x))
}


# This function looks like an S3 method, but isn't one.
`rownames<-.RowLinkedMatrix` <- function(x, value) {
    nodes <- nodes(x)
    for (i in 1L:nrow(nodes)) {
        rownames(x[[i]]) <- value[(nodes[i, 2L]:nodes[i, 3L])]
    }
    return(x)
}


# This function looks like an S3 method, but isn't one.
`colnames<-.RowLinkedMatrix` <- function(x, value) {
    for (i in 1L:nNodes(x)) {
        colnames(x[[i]]) <- value
    }
    return(x)
}


#' @export
`dimnames<-.RowLinkedMatrix` <- function(x, value) {
    d <- dim(x)
    rownames <- value[[1L]]
    colnames <- value[[2L]]
    if (!is.list(value) || length(value) != 2L || !(is.null(rownames) || length(rownames) == d[1L]) || !(is.null(colnames) ||
        length(colnames) == d[2L])) {
        stop("invalid dimnames")
    }
    x <- `rownames<-.RowLinkedMatrix`(x, rownames)
    x <- `colnames<-.RowLinkedMatrix`(x, colnames)
    return(x)
}


#' @rdname cbind.ColumnLinkedMatrix
#' @export
cbind.RowLinkedMatrix <- function(..., deparse.level = 0L) {
    stop("cbind is currently undefined for RowLinkedMatrix")
}


#' Combine Matrix-Like Objects by Rows.
#'
#' Compared to the [initialize()][initialize,RowLinkedMatrix-method()] method,
#' nested [LinkedMatrix-class] objects that are passed via `...` will not be
#' treated as matrix-like objects, but their nodes will be extracted and merged
#' with the new [RowLinkedMatrix-class] object for a more compact
#' representation. This method will currently only work for
#' [RowLinkedMatrix-class] objects.
#'
#' @param ... Matrix-like objects to be combined by rows.
#' @param deparse.level Currently unused, defaults to 0.
#' @export
rbind.RowLinkedMatrix <- function(..., deparse.level = 1L) {
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
    do.call(RowLinkedMatrix, nodes)
}


#' @export
nodes.RowLinkedMatrix <- function(x) {
    n <- nNodes(x)
    nodes <- matrix(integer(), nrow = n, ncol = 3L, dimnames = list(NULL, c("node", "row.ini", "row.end")))
    end <- 0L
    for (node in seq_len(n)) {
        ini <- end + 1L
        end <- ini + nrow(x[[node]]) - 1L
        nodes[node, ] <- c(node, ini, end)
    }
    return(nodes)
}


#' @export
index.RowLinkedMatrix <- function(x, i = NULL, ...) {
    nodes <- nodes(x)
    if (!is.null(i)) {
        i <- as.integer(i)
        if (is.unsorted(i)) {
            i <- sort(i)
        }
    } else {
        i <- seq_len(nodes[nrow(nodes), 3L])
    }
    index <- matrix(data = integer(), nrow = length(i), ncol = 3L, dimnames = list(NULL, c("node", "row.global", "row.local")))
    whichNode <- .bincode(i, breaks = c(0L, nodes[, 3L]))
    index[, 1L] <- whichNode
    index[, 2L] <- i
    index[, 3L] <- i - nodes[whichNode, 2L] + 1L
    return(index)
}


#' @rdname as.ColumnLinkedMatrix
#' @export
as.RowLinkedMatrix <- function(x, ...) {
    UseMethod("as.RowLinkedMatrix")
}


#' @rdname as.ColumnLinkedMatrix
#' @export
as.RowLinkedMatrix.list <- function(x, ...) {
    do.call("RowLinkedMatrix", x, ...)
}


#' @rdname ColumnLinkedMatrix-class
#' @export RowLinkedMatrix
#' @exportClass RowLinkedMatrix
RowLinkedMatrix <- setClass("RowLinkedMatrix", contains = "list")


#' @rdname initialize-ColumnLinkedMatrix-method
#' @export
setMethod("initialize", signature(.Object = "RowLinkedMatrix"), function(.Object, ...) {
    nodes <- list(...)
    # Append at least one matrix
    if (length(nodes) == 0L) {
        nodes[[1L]] <- matrix()
    } else {
        # Stop if matrices are not matrix-like
        if (!all(sapply(nodes, isMatrixLike))) {
            stop("arguments need to be matrix-like")
        }
        # Stop if dimensions of matrices do not match
        if (length(unique(sapply(nodes, ncol))) != 1L) {
            stop("arguments need the same number of columns")
        }
        # Warn if colnames of matrices do not match
        names <- lapply(nodes, colnames)
        names <- names[!sapply(names, is.null)]
        if (length(names) > 1L && !all(duplicated(names) | duplicated(names, fromLast = TRUE))) {
            warning("column names of matrix-like objects do not match")
        }
    }
    .Object <- callNextMethod(.Object, nodes)
    return(.Object)
})


#' @export
`[.RowLinkedMatrix` <- subset.RowLinkedMatrix


#' @export
`[<-.RowLinkedMatrix` <- replace.RowLinkedMatrix
