source("setup.R")

n <- 4L
p <- 4L
dimnames <- list(
    paste0("row_", seq_len(n)),
    paste0("col_", seq_len(p))
)

# Test extraction
extractionTests <- new.env()
extractionTests$COMPARE_OBJECT <- createMatrix(n, p, dimnames)
extractionTests$OUT_OF_BOUNDS_INT <- (n * p) + 1L
extractionTests$OUT_OF_BOUNDS_CHAR <- "col_1000"

# ColumnLinkedMatrix with 2 nodes
extractionTests$CUSTOM_OBJECT <- createLinkedMatrix(n, p, dimnames, "ColumnLinkedMatrix", 2L)
source(
    file = system.file("test-suite", "crochet-extract.R", package = "crochet"),
    local = extractionTests
)

# RowLinkedMatrix with 2 nodes
extractionTests$CUSTOM_OBJECT <- createLinkedMatrix(n, p, dimnames, "RowLinkedMatrix", 2L)
source(
    file = system.file("test-suite", "crochet-extract.R", package = "crochet"),
    local = extractionTests
)


# Test replacement
replacementTests <- new.env()
replacementTests$COMPARE_OBJECT <- createMatrix(n, p, dimnames)
replacementTests$VALUE_POOL <- 0:9
replacementTests$OUT_OF_BOUNDS_INT <- (n * p) + 1L
replacementTests$OUT_OF_BOUNDS_CHAR <- "snp1000_U"
replacementTests$SKIP_OUT_OF_BOUNDS_TESTS <- TRUE

# ColumnLinkedMatrix with 2 nodes
replacementTests$CUSTOM_OBJECT <- createLinkedMatrix(n, p, dimnames, "ColumnLinkedMatrix", 2L)
replacementTests$RESET <- function() {
    replacementTests$COMPARE_OBJECT <- createMatrix(n, p, dimnames)
    replacementTests$CUSTOM_OBJECT <- createLinkedMatrix(n, p, dimnames, "ColumnLinkedMatrix", 2L)
}
source(
    file = system.file("test-suite", "crochet-replace.R", package = "crochet"),
    local = replacementTests
)

# RowLinkedMatrix with 2 nodes
replacementTests$CUSTOM_OBJECT <- createLinkedMatrix(n, p, dimnames, "RowLinkedMatrix", 2L)
replacementTests$RESET <- function() {
    replacementTests$COMPARE_OBJECT <- createMatrix(n, p, dimnames)
    replacementTests$CUSTOM_OBJECT <- createLinkedMatrix(n, p, dimnames, "RowLinkedMatrix", 2L)
}
source(
    file = system.file("test-suite", "crochet-replace.R", package = "crochet"),
    local = replacementTests
)
