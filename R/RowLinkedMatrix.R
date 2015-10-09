subset.RowLinkedMatrix <- function(x, i, j, drop) {
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
    originalOrder <- (1:n)[order(i)]
    sortedRows <- sort(i)
    dimX <- dim(x)
    if (p > dimX[2] | n > dimX[1]) {
        stop("Either the number of columns or number of rows requested exceed the number of rows or columns in x, try dim(x)...")
    }
    Z <- matrix(nrow = n, ncol = p, NA)
    colnames(Z) <- colnames(x)[j]
    rownames(Z) <- rownames(x)[i]
    INDEX <- index(x)[sortedRows, , drop = FALSE]
    whatChunks <- unique(INDEX[, 1])
    end <- 0
    for (k in whatChunks) {
        TMP <- matrix(data = INDEX[INDEX[, 1] == k, ], ncol = 3)
        ini <- end + 1
        end <- ini + nrow(TMP) - 1
        Z[ini:end, ] <- x[[k]][TMP[, 3], j, drop = FALSE]
    }
    if (length(originalOrder) > 1) {
        Z[] <- Z[originalOrder, ]
    }
    if (drop == TRUE && (n == 1 || p == 1)) {
        # Revert drop.
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
    for (i in 1:length(x)) {
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
    for (i in 1:length(x)) {
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


#' @export
as.matrix.RowLinkedMatrix <- function(x, ...) {
    x[, , drop = FALSE]
}


#' @export
nodes.RowLinkedMatrix <- function(x) {
    n <- length(x)
    OUT <- matrix(nrow = n, ncol = 3, NA)
    colnames(OUT) <- c("node", "row.ini", "row.end")
    end <- 0
    for (i in 1:n) {
        ini <- end + 1
        end <- ini + nrow(x[[i]]) - 1
        OUT[i, ] <- c(i, ini, end)
    }
    return(OUT)
}


#' @export
index.RowLinkedMatrix <- function(x) {
    nodes <- nodes(x)
    nRowIndex <- nodes[nrow(nodes), 3]
    INDEX <- matrix(nrow = nRowIndex, ncol = 3)
    colnames(INDEX) <- c("node", "row.global", "row.local")
    INDEX[, 2] <- 1:nRowIndex
    end <- 0
    for (i in 1:nrow(nodes)) {
        nRowChunk <- nodes[i, 3] - nodes[i, 2] + 1
        ini <- end + 1
        end <- ini + nRowChunk - 1
        INDEX[ini:end, 1] <- i
        INDEX[ini:end, 3] <- 1:nRowChunk
    }
    return(INDEX)
}


#' An S4 class to represent a row-linked \code{LinkedMatrix}
#'
#' \code{RowLinkedMatrix} inherits from \code{\link{list}}.
#'
#' @export RowLinkedMatrix
#' @exportClass RowLinkedMatrix
RowLinkedMatrix <- setClass("RowLinkedMatrix", contains = "list")


#' Creates a new RowLinkedMatrix instance.
#' 
#' @inheritParams base::list
#' @param .Object The \code{RowLinkedMatrix} instance to be initialized.
#' @export
setMethod("initialize", "RowLinkedMatrix", function(.Object, ...) {
    list <- list(...)
    # Append at least one matrix
    if (length(list) == 0) {
        list[[1]] <- matrix()
    }
    .Object <- callNextMethod(.Object, list)
    return(.Object)
})


#' Extract parts of a \code{RowLinkedMatrix}.
#' 
#' @inheritParams base::`[`
#' @param j Column indices.
#' @export
setMethod("[", signature(x = "RowLinkedMatrix"), subset.RowLinkedMatrix)


#' Replace parts of a ColumnLinkedMatrix.
#' 
#' @inheritParams base::`[<-`
#' @param j Column indices.
#' @param ... Optional arguments.
#' @export
setReplaceMethod("[", signature(x = "RowLinkedMatrix"), replace.RowLinkedMatrix)
