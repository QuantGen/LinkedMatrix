LinkedMatrix <- function(nrow, ncol, nNodes, linkedBy, nodeInitializer, ...) {
    nrow <- as.integer(nrow)
    ncol <- as.integer(ncol)
    class <- ifelse(linkedBy == "columns", "ColumnLinkedMatrix", "RowLinkedMatrix")
    # Look for an internal function first
    ex <- try(nodeInitializer <- get(nodeInitializer), silent = TRUE)
    if (inherits(ex, "try-error")) {
        nodeInitializer <- match.fun(nodeInitializer)
    }
    linkedMatrix <- get(class)() # call default contructor
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

str.LinkedMatrix <- function(object, ...) {
    show(object)
    for (i in 1:nNodes(object)) {
        d <- dim(object[[i]])
        cat("  * Node ", i, ": ", d[1L], " x ", d[2L], " matrix-like object of class ", class(object[[i]]), "\n", sep = "")
    }
}

length.LinkedMatrix <- function(x) {
    prod(dim(x))
}

is.matrix.LinkedMatrix <- function(x) {
    TRUE # needed for diag()
}

as.matrix.LinkedMatrix <- function(x, ...) {
    x[, , drop = FALSE]
}

nNodes <- function(x) {
    length(slot(x, ".Data"))
}

nodes <- function(x) {
    UseMethod("nodes")
}

index <- function(x, ...) {
    UseMethod("index")
}

setClassUnion("LinkedMatrix", c("ColumnLinkedMatrix", "RowLinkedMatrix"))

setMethod("show", signature(object = "LinkedMatrix"), show)
