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
    nodes <- nodes(x)
    names <- rep("", nodes[nrow(nodes), 3L])
    for (i in seq_len(nrow(nodes))) {
        nodeNames <- colnames(x[[i]])
        if (!is.null(nodeNames)) {
            names[(nodes[i, 2L]:nodes[i, 3L])] <- nodeNames
        }
    }
    if (all(names == "")) {
        names <- NULL
    }
    return(names)
}

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

cbind.ColumnLinkedMatrix <- function(..., deparse.level = 0L) {
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
    do.call(ColumnLinkedMatrix, nodes)
}

nodes.ColumnLinkedMatrix <- function(x) {
    colsPerNode <- sapply(x, ncol)
    colUpperBoundaries <- cumsum(colsPerNode)
    colLowerBoundaries <- colUpperBoundaries - colsPerNode + 1
    n <- length(colsPerNode)
    nodes <- matrix(data = c(1:n, colLowerBoundaries, colUpperBoundaries), nrow = n, ncol = 3L, dimnames = list(NULL, c("node", "col.ini", "col.end")))
    return(nodes)
}

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

as.ColumnLinkedMatrix <- function(x, ...) {
    UseMethod("as.ColumnLinkedMatrix")
}

as.ColumnLinkedMatrix.list <- function(x, ...) {
    do.call(ColumnLinkedMatrix, x, ...)
}

ColumnLinkedMatrix <- setClass("ColumnLinkedMatrix", contains = "list")

setValidity("ColumnLinkedMatrix", function(object) {
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
    if (length(unique(sapply(nodes, nrow))) != 1L) {
        return("arguments need the same number of rows")
    }
    # Warn if rownames of matrices do not match
    names <- lapply(nodes, rownames)
    if (length(names) > 1L && !all(duplicated(names) | duplicated(names, fromLast = TRUE))) {
        warning("row names of matrix-like objects do not match: rownames() only uses the row names of the first node")
    }
    return(TRUE)
})

`[.ColumnLinkedMatrix` <- extract(
    extract_vector = extract_vector.ColumnLinkedMatrix,
    extract_matrix = extract_matrix.ColumnLinkedMatrix,
    allowDoubles = TRUE # this may not be compatible with all matrix-like objects
)

`[<-.ColumnLinkedMatrix` <- replace(
    replace_vector = replace_vector.ColumnLinkedMatrix,
    replace_matrix = replace_matrix.ColumnLinkedMatrix,
    allowDoubles = TRUE # this may not be compatible with all matrix-like objects
)
