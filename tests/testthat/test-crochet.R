n <- 4L
p <- 4L
dimnames <- list(
    paste0("row_", seq_len(n)),
    paste0("col_", seq_len(p))
)

# Test extraction
CROCHET_EXTRACT_ENV <- new.env()
CROCHET_EXTRACT_ENV$COMPARE_OBJECT <- createMatrix(n, p, dimnames)
CROCHET_EXTRACT_ENV$OUT_OF_BOUNDS_INT <- (n * p) + 1L
CROCHET_EXTRACT_ENV$OUT_OF_BOUNDS_CHAR <- "col_1000"

context("ColumnLinkedMatrix with 2 nodes")
CROCHET_EXTRACT_ENV$CUSTOM_OBJECT <- createLinkedMatrix(n, p, dimnames, "ColumnLinkedMatrix", 2L)
source(system.file("test-suite", "crochet-extract.R", package = "crochet"), local = TRUE)

context("RowLinkedMatrix with 2 nodes")
CROCHET_EXTRACT_ENV$CUSTOM_OBJECT <- createLinkedMatrix(n, p, dimnames, "RowLinkedMatrix", 2L)
source(system.file("test-suite", "crochet-extract.R", package = "crochet"), local = TRUE)


# Test replacement
CROCHET_REPLACE_ENV <- new.env()
CROCHET_REPLACE_ENV$COMPARE_OBJECT <- createMatrix(n, p, dimnames)
CROCHET_REPLACE_ENV$VALUE_POOL <- 0:9
CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_INT <- (n * p) + 1L
CROCHET_REPLACE_ENV$OUT_OF_BOUNDS_CHAR <- "snp1000_U"
CROCHET_REPLACE_ENV$SKIP_OUT_OF_BOUNDS_TESTS <- TRUE

context("ColumnLinkedMatrix with 2 nodes")
CROCHET_REPLACE_ENV$CUSTOM_OBJECT <- createLinkedMatrix(n, p, dimnames, "ColumnLinkedMatrix", 2L)
CROCHET_REPLACE_ENV$RESET <- function() {
    CROCHET_REPLACE_ENV$COMPARE_OBJECT <- createMatrix(n, p, dimnames)
    CROCHET_REPLACE_ENV$CUSTOM_OBJECT <- createLinkedMatrix(n, p, dimnames, "ColumnLinkedMatrix", 2L)
}
source(system.file("test-suite", "crochet-replace.R", package = "crochet"), local = TRUE)

context("RowLinkedMatrix with 2 nodes")
CROCHET_REPLACE_ENV$CUSTOM_OBJECT <- createLinkedMatrix(n, p, dimnames, "RowLinkedMatrix", 2L)
CROCHET_REPLACE_ENV$RESET <- function() {
    CROCHET_REPLACE_ENV$COMPARE_OBJECT <- createMatrix(n, p, dimnames)
    CROCHET_REPLACE_ENV$CUSTOM_OBJECT <- createLinkedMatrix(n, p, dimnames, "RowLinkedMatrix", 2L)
}
source(system.file("test-suite", "crochet-replace.R", package = "crochet"), local = TRUE)
