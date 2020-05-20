extract_matrix.RowLinkedMatrix <- function(x, i, j, ...) {
    # Handle x[FALSE, ]
    if (length(i) == 0L) {
        Z <- matrix(data = integer(), nrow = 0L, ncol = length(j), dimnames = list(NULL, colnames(x)[j]))
    } else {
        # Determine nodes and node boundaries for query
        index <- index(x, i = i, sort = FALSE)
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
                    Z[nodeIndex, ] <- NA_integer_
                } else {
                    nodeIndex <- index[, 1L] == curNode
                    nodeIndex[is.na(nodeIndex)] <- FALSE
                    # Convert to matrix to support data frames
                    Z[nodeIndex, ] <- as.matrix(x[[curNode]][index[nodeIndex, 3L], j, drop = FALSE])
                }
            }
        } else {
            # Handle x[NA, ] or out of bounds
            if (is.na(nodeList)) {
                Z <- matrix(data = NA_integer_, nrow = length(i), ncol = length(j), dimnames = list(rep(NA_character_, length(i)), colnames(x)[j]))
            } else {
                # Convert to matrix to support data frames
                Z <- as.matrix(x[[nodeList]][index[, 3L], j, drop = FALSE])
            }
        }
    }
    return(Z)
}

extract_vector.RowLinkedMatrix <- function(x, i, ...) {
    if (length(i) == 0L) {
        Z <- integer(0L)
    } else {
        # Convert one-dimensional index to two-dimensional index
        ij <- ktoij(x, i)
        # Determine nodes and node boundaries for query (in this case we cannot
        # use index() as rowsPerNode is needed to recalculate the single index)
        rowsPerNode <- sapply(x, nrow)
        nodeBoundaries <- c(0L, cumsum(rowsPerNode))
        nodeMembership <- .bincode(ij[["i"]], nodeBoundaries)
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
                    # Convert two-dimensional index back to one-dimensional index
                    localIndex <- ((ij[["j"]][nodeIndex] - 1L) * rowsPerNode[curNode] + ij[["i"]][nodeIndex]) - nodeBoundaries[curNode]
                    Z[nodeIndex] <- x[[curNode]][localIndex]
                }
            }
        } else {
            # Handle x[NA] or out of bounds
            if (is.na(nodeList)) {
                Z <- rep(NA_integer_, length(i))
            } else {
                # Convert two-dimensional index back to one-dimensional index
                localIndex <- ((ij[["j"]] - 1L) * rowsPerNode[nodeList] + ij[["i"]]) - nodeBoundaries[nodeList]
                Z <- x[[nodeList]][localIndex]
            }
        }
    }
    return(Z)
}

replace_matrix.RowLinkedMatrix <- function(x, i, j, ..., value) {
    # Convert value vector to matrix
    dim(value) <- c(length(i), length(j))
    # Determine node boundaries
    index <- index(x, i = i, sort = FALSE)
    nodeList <- unique(index[, 1L])
    # Replace elements in each node
    for (curNode in nodeList) {
        nodeIndex <- index[, 1L] == curNode
        x[[curNode]][index[nodeIndex, 3L], j] <- value[nodeIndex, ]
    }
    return(x)
}

replace_vector.RowLinkedMatrix <- function(x, i, ..., value) {
    # Convert one-dimensional index to two-dimensional index
    ij <- ktoij(x, i)
    # Determine nodes and node boundaries for query (in this case we cannot
    # use index() as rowsPerNode is needed to recalculate the single index)
    rowsPerNode <- sapply(x, nrow)
    nodeBoundaries <- c(0L, cumsum(rowsPerNode))
    nodeMembership <- .bincode(ij[["i"]], nodeBoundaries)
    nodeList <- unique(nodeMembership)
    # Replace elements in each node
    for (curNode in nodeList) {
        nodeIndex <- nodeMembership == curNode
        # Convert two-dimensional index back to one-dimensional index
        localIndex <- ((ij[["j"]][nodeIndex] - 1L) * rowsPerNode[curNode] + ij[["i"]][nodeIndex]) - nodeBoundaries[curNode]
        x[[curNode]][localIndex] <- value[nodeIndex]
    }
    return(x)
}

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
    nodes <- nodes(x)
    names <- rep("", nodes[nrow(nodes), 3L])
    for (i in seq_len(nrow(nodes))) {
        nodeNames <- rownames(x[[i]])
        if (!is.null(nodeNames)) {
            names[(nodes[i, 2L]:nodes[i, 3L])] <- nodeNames
        }
    }
    if (all(names == "")) {
        names <- NULL
    }
    return(names)
}

# This function looks like an S3 method, but isn't one.
colnames.RowLinkedMatrix <- function(x) {
    colnames(x[[1L]])
}

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

rbind.RowLinkedMatrix <- function(..., deparse.level = 1L) {
    dotdotdot <- list(...)
    nodes <- list()
    for (i in seq_along(dotdotdot)) {
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

nodes.RowLinkedMatrix <- function(x) {
    rowsPerNode <- sapply(x, nrow)
    rowUpperBoundaries <- cumsum(rowsPerNode)
    rowLowerBoundaries <- rowUpperBoundaries - rowsPerNode + 1
    n <- length(rowsPerNode)
    nodes <- matrix(data = c(1:n, rowLowerBoundaries, rowUpperBoundaries), nrow = n, ncol = 3L, dimnames = list(NULL, c("node", "row.ini", "row.end")))
    return(nodes)
}

index.RowLinkedMatrix <- function(x, i = NULL, sort = TRUE, ...) {
    nodes <- nodes(x)
    if (!is.null(i)) {
        i <- as.integer(i)
        if (sort) {
            i <- sort(i)
        }
    } else {
        i <- seq_len(nodes[nrow(nodes), 3L])
    }
    nodeBoundaries <- c(0L, nodes[, 3L])
    nodeMembership <- .bincode(i, breaks = nodeBoundaries)
    index <- matrix(data = c(nodeMembership, i, i - nodeBoundaries[nodeMembership]), nrow = length(i), ncol = 3L, dimnames = list(NULL, c("node", "row.global", "row.local")))
    return(index)
}

as.RowLinkedMatrix <- function(x, ...) {
    UseMethod("as.RowLinkedMatrix")
}

as.RowLinkedMatrix.list <- function(x, ...) {
    do.call(RowLinkedMatrix, x, ...)
}

RowLinkedMatrix <- setClass("RowLinkedMatrix", contains = "list")

setValidity("RowLinkedMatrix", function(object) {
    nodes <- slot(object, ".Data")
    # Stop unless there is more than one node
    if (length(nodes) == 0L) {
        return("there needs to be at least one node")
    }
    # Stop if matrices are not matrix-like
    if (!all(sapply(nodes, isMatrixLike))) {
        return("arguments need to be matrix-like")
    }
    # Stop if dimensions of matrices do not match
    if (length(unique(sapply(nodes, ncol))) != 1L) {
        return("arguments need the same number of columns")
    }
    # Warn if colnames of matrices do not match
    names <- lapply(nodes, colnames)
    if (length(names) > 1L && !all(duplicated(names) | duplicated(names, fromLast = TRUE))) {
        warning("column names of matrix-like objects do not match: colnames() only uses the column names of the first node")
    }
    return(TRUE)
})

`[.RowLinkedMatrix` <- extract(
    extract_vector = extract_vector.RowLinkedMatrix,
    extract_matrix = extract_matrix.RowLinkedMatrix,
    allowDoubles = TRUE # this may not be compatible with all matrix-like objects
)

`[<-.RowLinkedMatrix` <- replace(
    replace_vector = replace_vector.RowLinkedMatrix,
    replace_matrix = replace_matrix.RowLinkedMatrix,
    allowDoubles = TRUE # this may not be compatible with all matrix-like objects
)
