subset.RowLinkedMatrix <- function(x, i, j, ..., drop) {
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
    if (p > pX | n > nX) {
        stop("Either the number of columns or number of rows requested exceed the number of rows or columns in x, try dim(x)...")
    }
    # Providing a sorted row index will eliminate the need to reorder the
    # result matrix later (avoiding a copy)
    isUnsorted <- is.unsorted(i)
    if (isUnsorted) {
        # Reorder rows for sequential retrieval by chunk
        originalOrder <- rank(i, ties.method = "first")
        sortedRows <- sort(i)
    } else {
        sortedRows <- i
    }
    # Compute node inventory
    globalIndex <- index(x, sortedRows)
    whatChunks <- unique(globalIndex[, 1])
    # If there are several chunks involved, aggregate the result in a separate
    # matrix, otherwise pass through result
    if (length(whatChunks) > 1) {
        # Initialize result matrix as integer matrix because it does not take up as
        # much space as double() but is more useful than logical()
        Z <- matrix(data = integer(), nrow = n, ncol = p)
        # Use dimnames instead of rownames and colnames to avoid copy
        dimnames(Z) <- list(rownames(x)[sortedRows], colnames(x)[j])
        end <- 0
        for (k in whatChunks) {
            localIndex <- globalIndex[globalIndex[, 1] == k, , drop = FALSE]
            ini <- end + 1
            end <- ini + nrow(localIndex) - 1
            # Convert to matrix to support data frames
            Z[ini:end, ] <- as.matrix(x[[k]][localIndex[, 3], j, drop = FALSE])
        }
    } else {
        Z <- as.matrix(x[[whatChunks]][globalIndex[, 3], j, drop = FALSE])
    }
    if (isUnsorted) {
        # Return to original order
        Z <- Z[originalOrder, , drop = FALSE]
    }
    if (drop == TRUE && (n == 1 || p == 1)) {
        # Let R handle drop behavior
        return(Z[, ])
    } else {
        return(Z)
    }
}


replace.RowLinkedMatrix <- function(x, i, j, ..., value) {
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
        rows_z <- (i >= nodes[k, 2]) & (i <= nodes[k, 3])
        rowLocal <- index[i[rows_z], 3]
        x[[k]][rowLocal, j] <- Z[rows_z, ]
    }
    return(x)
}


#' @export
dim.RowLinkedMatrix <- function(x) {
    p <- ncol(x[[1]])
    n <- 0
    for (i in 1:nNodes(x)) {
        n <- n + nrow(x[[i]])
    }
    return(c(n, p))
}


# This function looks like an S3 method, but isn't one.
rownames.RowLinkedMatrix <- function(x) {
    out <- NULL
    if (!is.null(rownames(x[[1]]))) {
        n <- dim(x)[1]
        out <- rep("", n)
        nodes <- nodes(x)
        for (i in 1:nrow(nodes)) {
            out[(nodes[i, 2]:nodes[i, 3])] <- rownames(x[[i]])
        }
    }
    return(out)
}


# This function looks like an S3 method, but isn't one.
colnames.RowLinkedMatrix <- function(x) {
    out <- colnames(x[[1]])
    return(out)
}


#' @export
dimnames.RowLinkedMatrix <- function(x) {
    list(rownames.RowLinkedMatrix(x), colnames.RowLinkedMatrix(x))
}


# This function looks like an S3 method, but isn't one.
`rownames<-.RowLinkedMatrix` <- function(x, value) {
    nodes <- nodes(x)
    for (i in 1:nrow(nodes)) {
        rownames(x[[i]]) <- value[(nodes[i, 2]:nodes[i, 3])]
    }
    return(x)
}


# This function looks like an S3 method, but isn't one.
`colnames<-.RowLinkedMatrix` <- function(x, value) {
    for (i in 1:nNodes(x)) {
        colnames(x[[i]]) <- value
    }
    return(x)
}


#' @export
`dimnames<-.RowLinkedMatrix` <- function(x, value) {
    d <- dim(x)
    rownames <- value[[1]]
    colnames <- value[[2]]
    if (!is.list(value) || length(value) != 2 || !(is.null(rownames) || length(rownames) == d[1]) || !(is.null(colnames) ||
        length(colnames) == d[2])) {
        stop("invalid dimnames")
    }
    x <- `rownames<-.RowLinkedMatrix`(x, rownames)
    x <- `colnames<-.RowLinkedMatrix`(x, colnames)
    return(x)
}


#' Combine matrix-like objects by columns.
#'
#' This method is currently undefined for
#' \code{\link[=RowLinkedMatrix-class]{RowLinkedMatrix}} objects.
#'
#' @param ... Matrix-like objects to be combined by columns.
#' @param deparse.level Currently unused, defaults to 0.
#' @export
cbind.RowLinkedMatrix <- function(..., deparse.level = 0) {
    stop("cbind is currently undefined for RowLinkedMatrix")
}


#' Combine matrix-like objects by rows.
#'
#' Compared to the
#' \code{\link[=initialize,RowLinkedMatrix-method]{RowLinkedMatrix}}
#' constructor, nested \code{\link[=LinkedMatrix-class]{LinkedMatrix}} objects
#' that are passed via \code{...} will not be treated as matrix-like objects,
#' but their nodes will be extracted and merged with the new
#' \code{\link[=RowLinkedMatrix-class]{RowLinkedMatrix}} object for a more
#' compact representation.
#'
#' @param ... Matrix-like objects to be combined by rows.
#' @param deparse.level Currently unused, defaults to 0.
#' @export
rbind.RowLinkedMatrix <- function(..., deparse.level = 1) {
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
    OUT <- matrix(integer(), nrow = n, ncol = 3, dimnames = list(NULL, c("node", "row.ini", "row.end")))
    end <- 0
    for (node in seq_len(n)) {
        ini <- end + 1
        end <- ini + nrow(x[[node]]) - 1
        OUT[node, ] <- c(node, ini, end)
    }
    return(OUT)
}


#' @export
index.RowLinkedMatrix <- function(x, i = NULL, ...) {
    nodes <- nodes(x)
    n <- nodes[nrow(nodes), 3]
    if (!is.null(i)) {
        if (is.unsorted(i)) {
            i <- sort(i)
        }
    } else {
        i <- seq_len(n)
    }
    OUT <- matrix(nrow = length(i), ncol = 3, dimnames = list(NULL, c("node", "row.global", "row.local")))
    OUT[, 2] <- i
    for (node in 1:nrow(nodes)) {
        globalIdx <- which(i >= nodes[node, 2] & i <= nodes[node, 3])
        if (node > 1) {
            localIdx <- i[globalIdx] - nodes[node - 1, 3]
        } else {
            localIdx <- i[globalIdx]
        }
        OUT[globalIdx, 1] <- node
        OUT[globalIdx, 3] <- localIdx
    }
    return(OUT)
}


#' An S4 class to represent a row-linked
#' \code{\link[=LinkedMatrix-class]{LinkedMatrix}}.
#'
#' This class treats a list of matrix-like objects that are linked together by
#' rows and have the same number of columns similarly to a regular \code{matrix}
#' by implementing key methods such as \code{[} and \code{[<-} for extracting
#' and replacing matrix elements, \code{dim} to retrieve dimensions, and
#' \code{dimnames} and \code{dimnames<-} to retrieve and set dimnames. Each list
#' element is called a node and can be extracted or replaced using \code{[[} and
#' \code{[[<-}. A matrix-like object is one that has two dimensions and
#' implements at least \code{dim} and \code{[}.
#'
#' There are several ways to create an instance of this class: either by using
#' one of the constructors
#' \code{\link[=initialize,RowLinkedMatrix-method]{RowLinkedMatrix(...)}} or
#' \code{\link[=initialize,RowLinkedMatrix-method]{new("RowLinkedMatrix",...)}},
#' or by using the more general \code{\link{LinkedMatrix}} function that
#' constructs objects of certain dimensions with a configurable number and type
#' of nodes.
#'
#' @export RowLinkedMatrix
#' @exportClass RowLinkedMatrix
RowLinkedMatrix <- setClass("RowLinkedMatrix", contains = "list")


#' Creates a new \code{\link[=RowLinkedMatrix-class]{RowLinkedMatrix}} instance.
#'
#' This method is run when a
#' \code{\link[=RowLinkedMatrix-class]{RowLinkedMatrix}} object is created using
#' \code{\link[=initialize,RowLinkedMatrix-method]{RowLinkedMatrix(...)}} or
#' \code{\link[=initialize,RowLinkedMatrix-method]{new("RowLinkedMatrix",...)}}
#' and accepts a list of matrix-like objects as \code{...}. A matrix-like object
#' is one that has two dimensions and implements at least \code{dim} and
#' \code{[}. Each object needs to have the same number of columns to be linked
#' together. \code{\link[=LinkedMatrix-class]{LinkedMatrix}} can be nested as
#' long as they are conformable. If no matrix-like objects are given, a single
#' 1x1 node of type \code{matrix} filled with \code{NA} is returned.
#'
#' @inheritParams base::list
#' @param .Object The \code{\link[=RowLinkedMatrix-class]{RowLinkedMatrix}}
#'   instance to be initialized. This argument is passed in by R and can be
#'   ignored, but still needs to be documented.
#' @param ... A sequence of matrix-like objects of the same column-dimension.
#' @export
setMethod("initialize", signature(.Object = "RowLinkedMatrix"), function(.Object, ...) {
    nodes <- list(...)
    # Append at least one matrix
    if (length(nodes) == 0) {
        nodes[[1]] <- matrix()
    } else {
        # Detect non-matrix objects by checking dimensions
        if (any(sapply(nodes, function(x) length(dim(x)) != 2))) {
            stop("arguments need to be matrix-like")
        }
        # Detect matrices that do not match in dimensions
        if (length(unique(sapply(nodes, ncol))) != 1) {
            stop("arguments need the same number of columns")
        }
    }
    .Object <- callNextMethod(.Object, nodes)
    return(.Object)
})


#' Extract parts of a \code{\link[=RowLinkedMatrix-class]{RowLinkedMatrix}}.
#'
#' This method is run when the \code{[]} operator is used on a
#' \code{\link[=RowLinkedMatrix-class]{RowLinkedMatrix}} object.
#'
#' @inheritParams base::`[`
#' @param j Column indices.
#' @param ... Additional arguments
#' @export
setMethod("[", signature(x = "RowLinkedMatrix"), subset.RowLinkedMatrix)


#' Replace parts of a \code{\link[=RowLinkedMatrix-class]{RowLinkedMatrix}}.
#'
#' This method is run when the \code{[]} operator is used in an assignment on a
#' \code{\link[=RowLinkedMatrix-class]{RowLinkedMatrix}} object.
#'
#' @inheritParams base::`[<-`
#' @param j Column indices.
#' @param ... Additional arguments
#' @export
setReplaceMethod("[", signature(x = "RowLinkedMatrix"), replace.RowLinkedMatrix)
