#' @include ColumnLinkedMatrix.R RowLinkedMatrix.R
NULL


#' @exportClass LinkedMatrix
setClassUnion("LinkedMatrix", c("ColumnLinkedMatrix", "RowLinkedMatrix"))


show <- function(object) {
    d <- dim(object)
    cat(d[1], "x", d[2], "linked matrix of class", class(object))
    NULL
}


#' Show a LinkedMatrix object.
#' 
#' @param object Either a \code{\linkS4class{ColumnLinkedMatrix}} or a 
#'   \code{\linkS4class{RowLinkedMatrix}} object.
#' @export
setMethod("show", signature(object = "LinkedMatrix"), show)


apply.LinkedMatrix <- function(X, MARGIN, FUN, chunkSize = 1000, verbose = FALSE, ...) {
    FUN <- match.fun(FUN)
    n <- ifelse(MARGIN == 1, nrow(X), ncol(X))
    # Apply function on first element to check output type.
    x <- ifelse(MARGIN == 1, X[1, ], X[, 1])
    outputType <- FUN(x, ...)
    if (is.atomic(outputType)) {
        ANS <- matrix(nrow = length(outputType), ncol = n, NA)
        rownames(ANS) <- names(outputType)
        if (MARGIN == 1) {
            colnames(ANS) <- rownames(X)
        } else {
            colnames(ANS) <- colnames(X)
        }
        nChunks <- ceiling(n/chunkSize)
        end <- 0
        for (i in 1:nChunks) {
            if (verbose) {
                cat(i, " out of ", nChunks, " \n")
            }
            ini <- end + 1
            end <- min(ini + chunkSize - 1, n)
            if (MARGIN == 1) {
                Z <- X[ini:end, ]
            } else {
                Z <- X[, ini:end]
            }
            ANS[, ini:end] <- apply(FUN = FUN, MARGIN = MARGIN, X = Z, ...)
        }
    } else {
        ANS <- vector("list", n)
        names(ANS) <- ifelse(MARGIN == 1, rownames(X), colnames(X))
        end <- 0
        for (i in 1:n) {
            if (verbose) {
                cat(i, " out of ", n, " \n")
            }
            if (MARGIN == 1) {
                ANS[[i]] <- FUN(X[i, ], ...)
            } else {
                ANS[[i]] <- FUN(X[, i], ...)
            }
        }
    }
    return(ANS[, , drop = TRUE])
}

#' Apply function for \code{\linkS4class{ColumnLinkedMatrix}} or 
#' \code{\linkS4class{RowLinkedMatrix}} objects.
#' 
#' This function brings chunks (of size \code{chunkSize}) of rows (if 
#' \code{MARGIN} is 1) or columns (if \code{MARGIN} is 2) of the 
#' \code{LinkedMatrix} instance into RAM as \code{matrix} objects and calls the 
#' \code{apply} function of the base package for each chunk. Results from all
#' the chunks are collected and returned.
#' 
#' @param X Either a \code{\linkS4class{ColumnLinkedMatrix}} or a 
#'   \code{\linkS4class{RowLinkedMatrix}} object.
#' @param MARGIN Use 1 to apply function over rows or 2 to apply function over
#'   columns.
#' @param FUN The function to be applied.
#' @param chunkSize The number of columns or rows that are processed at a time 
#'   (see Details).
#' @param verbose Whether to print additional information.
#' @param ... Optional arguments to FUN.
#' @return Returns a \code{matrix} or a \code{list} with results from FUN.
#' @export
setMethod("apply", signature("LinkedMatrix"), apply.LinkedMatrix)


colMeans.LinkedMatrix <- function(x, na.rm = TRUE, chunkSize = 1000, ...) {
    if (na.rm) {
        warning("Ignoring missing values")
    }
    ANS <- apply.LinkedMatrix(X = x, MARGIN = 2, FUN = mean, chunkSize = chunkSize, na.rm = na.rm, ...)
    return(ANS)
}

#' Form column means.
#' 
#' @inheritParams base::colMeans
#' @param chunkSize The number of columns that are processed at a time.
#' @param ... Optional arguments to \code{mean}.
#' @export
setMethod("colMeans", signature("LinkedMatrix"), colMeans.LinkedMatrix)


colSums.LinkedMatrix <- function(x, na.rm = TRUE, chunkSize = 1000, ...) {
    if (na.rm) {
        warning("Ignoring missing values")
    }
    ANS <- apply.LinkedMatrix(X = x, MARGIN = 2, FUN = sum, chunkSize = chunkSize, na.rm = na.rm, ...)
    return(ANS)
}

#' Form column sums.
#' 
#' @inheritParams base::colSums
#' @param chunkSize The number of columns that are processed at a time.
#' @param ... Optional arguments to \code{mean}.
#' @export
setMethod("colSums", signature("LinkedMatrix"), colSums.LinkedMatrix)


rowMeans.LinkedMatrix <- function(x, na.rm = TRUE, chunkSize = 1000, ...) {
    if (na.rm) {
        warning("Ignoring missing values")
    }
    ANS <- apply.LinkedMatrix(X = x, MARGIN = 1, FUN = mean, chunkSize = chunkSize, na.rm = na.rm, ...)
    return(ANS)
}

#' Form row means.
#' 
#' @inheritParams base::rowMeans
#' @param chunkSize The number of rows that are processed at a time.
#' @param ... Optional arguments to \code{mean}.
#' @export
setMethod("rowMeans", signature("LinkedMatrix"), rowMeans.LinkedMatrix)


rowSums.LinkedMatrix <- function(x, na.rm = TRUE, chunkSize = 1000, ...) {
    if (na.rm) {
        warning("Ignoring missing values")
    }
    ANS <- apply.LinkedMatrix(X = x, MARGIN = 1, FUN = sum, chunkSize = chunkSize, na.rm = na.rm, ...)
    return(ANS)
}

#' Form row sums.
#' 
#' @inheritParams base::rowSums
#' @param chunkSize The number of rows that are processed at a time.
#' @param ... Optional arguments to \code{sum}.
#' @export
setMethod("rowSums", signature("LinkedMatrix"), rowSums.LinkedMatrix)


summary.num <- function(x) {
    out <- c(range(x, na.rm = T), mean(x, na.rm = T), sd(x, na.rm = T), mean(is.na(x)))
    names(out) <- c("min", "max", "mean", "sd", "prop NAs")
    return(out)
}

summary.char <- function(x) {
    out <- table(x, useNA = "always")
    out <- out/length(x)
    return(out)
}

summary.LinkedMatrix <- function(object, MARGIN = 2, chunkSize = 1000, ...) {
    # If MARGIN==1 summaries of columns are provided, this is the default, otherwise, row-summaries are returned.
    sample <- object[1, 1]
    if (is.numeric()) {
        fun <- summary.num
    } else if (is.character(sample) | is.logical(sample)) {
        fun <- summary.char
    } else {
        fun <- summary
    }
    apply.LinkedMatrix(X = object, MARGIN = MARGIN, FUN = fun, chunkSize = chunkSize, ...)
}

#' @export
setMethod("summary", signature("LinkedMatrix"), summary.LinkedMatrix)


#' Returns the column or row indexes at which each node starts and ends.
#' 
#' @param x Either a \code{\linkS4class{ColumnLinkedMatrix}} or a 
#'   \code{\linkS4class{RowLinkedMatrix}} object
#' @return A matrix.
#' @export
nodes <- function(x) {
    UseMethod("nodes")
}


#' Maps each column or row index of a linked matrix to the column or row index
#' of its corresponding node.
#' 
#' @param x Either a \code{\linkS4class{ColumnLinkedMatrix}} or a 
#'   \code{\linkS4class{RowLinkedMatrix}} object
#' @return A matrix.
#' @export
index <- function(x) {
    UseMethod("index")
} 
