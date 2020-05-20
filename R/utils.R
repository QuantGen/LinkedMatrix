chunkRanges <- function(a, n, i = NULL) {
    if (n > a) {
        stop(paste("Cannot split", a, "into", n, "chunks. Reduce the number of chunks."))
    }
    k <- as.integer(a / n)
    r <- as.integer(a %% n)
    range <- function(i, k, r) {
        c((i - 1L) * k + min(i - 1L, r) + 1L, i * k + min(i, r))
    }
    if (!is.null(i)) {
        range(i, k, r)
    } else {
        sapply(seq_len(n), range, k, r)
    }
}

# Incomplete check if x is matrix-like. Should check for length as well.
isMatrixLike <- function(x) {
    length(dim(x)) == 2L
}
