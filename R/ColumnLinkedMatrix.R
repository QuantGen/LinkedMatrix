subset.ColumnLinkedMatrix <- function(x, i, j, drop) {
    if (missing(i)) {
        i <- 1:nrow(x)
    }
    if (missing(j)) {
        j <- 1:ncol(x)
    }
    if (class(i) == "logical") {
        i <- which(i)
    } else if (class(i) == "character") {
        i <- sapply(i, function(name) {
            which(rownames(x) == name)
        }, USE.NAMES = FALSE)
    }
    if (class(j) == "logical") {
        j <- which(j)
    } else if (class(j) == "character") {
        j <- sapply(j, function(name) {
            which(colnames(x) == name)
        }, USE.NAMES = FALSE)
    }
    n <- length(i)
    p <- length(j)
    originalOrder <- (1:p)[order(j)]
    sortedColumns <- sort(j)
    dimX <- dim(x)
    if (p > dimX[2] | n > dimX[1]) {
        stop("Either the number of columns or number of rows requested exceed the number of rows or columns in x, try dim(x)...")
    }
    Z <- matrix(nrow = n, ncol = p, NA)
    colnames(Z) <- colnames(x)[j]
    rownames(Z) <- rownames(x)[i]
    INDEX <- index(x)[sortedColumns, , drop = FALSE]
    whatChunks <- unique(INDEX[, 1])
    end <- 0
    for (k in whatChunks) {
        TMP <- matrix(data = INDEX[INDEX[, 1] == k, ], ncol = 3)
        ini <- end + 1
        end <- ini + nrow(TMP) - 1
        Z[, ini:end] <- x[[k]][i, TMP[, 3], drop = FALSE]
    }
    if (length(originalOrder) > 1) {
        Z[] <- Z[, originalOrder]
    }
    if (drop == TRUE && (n == 1 || p == 1)) {
        # Revert drop.
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
    ellipsis <- list(...)
    if (is.null(ellipsis$nodes)) {
        nodes <- nodes(x)
    } else {
        nodes <- ellipsis$nodes
    }
    if (is.null(ellipsis$index)) {
        index <- index(x)
    } else {
        index <- ellipsis$index
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
    p <- 0
    for (i in 1:length(x)) {
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
    for (i in 1:length(x)) {
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


#' @export
as.matrix.ColumnLinkedMatrix <- function(x, ...) {
    x[, , drop = FALSE]
}


#' @export
nodes.ColumnLinkedMatrix <- function(x) {
    n <- length(x)
    OUT <- matrix(nrow = n, ncol = 3, NA)
    colnames(OUT) <- c("node", "col.ini", "col.end")
    end <- 0
    for (i in 1:n) {
        ini <- end + 1
        end <- ini + ncol(x[[i]]) - 1
        OUT[i, ] <- c(i, ini, end)
    }
    return(OUT)
}


#' @export
index.ColumnLinkedMatrix <- function(x) {
    nodes <- nodes(x)
    nColIndex <- nodes[nrow(nodes), 3]
    INDEX <- matrix(nrow = nColIndex, ncol = 3)
    colnames(INDEX) <- c("node", "col.global", "col.local")
    INDEX[, 2] <- 1:nColIndex
    end <- 0
    for (i in 1:nrow(nodes)) {
        nColChunk <- nodes[i, 3] - nodes[i, 2] + 1
        ini <- end + 1
        end <- ini + nColChunk - 1
        INDEX[ini:end, 1] <- i
        INDEX[ini:end, 3] <- 1:nColChunk
    }
    return(INDEX)
}


#' An S4 class to represent a column-linked \code{LinkedMatrix}.
#' 
#' \code{ColumnLinkedMatrix} inherits from \code{\link{list}}.
#' 
#' @export ColumnLinkedMatrix
#' @exportClass ColumnLinkedMatrix
ColumnLinkedMatrix <- setClass("ColumnLinkedMatrix", contains = "list")


#' Creates a new \code{ColumnLinkedMatrix} instance.
#' 
#' @inheritParams base::list
#' @param .Object The \code{ColumnLinkedMatrix} instance to be initialized.
#' @export
setMethod("initialize", "ColumnLinkedMatrix", function(.Object, ...) {
    list <- list(...)
    # Append at least one matrix
    if (length(list) == 0) {
        list[[1]] <- matrix()
    }
    .Object <- callNextMethod(.Object, list)
    return(.Object)
})


#' Extract parts of a \code{ColumnLinkedMatrix}.
#' 
#' @inheritParams base::`[`
#' @param j Column indices.
#' @export
setMethod("[", signature(x = "ColumnLinkedMatrix"), subset.ColumnLinkedMatrix)


#' Replace parts of a \code{ColumnLinkedMatrix}.
#' 
#' @inheritParams base::`[<-`
#' @param j Column indices.
#' @param ... Optional arguments.
#' @export
setReplaceMethod("[", signature(x = "ColumnLinkedMatrix"), replace.ColumnLinkedMatrix)
