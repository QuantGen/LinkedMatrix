extract_tests <- system.file("tests", "testthat", "test-crochet-extract.R", package = "crochet")

if (extract_tests == "") {

    test_that("subsetting", {
        skip("crochet tests have to be installed")
    })

} else {

    # Prepare dummy data
    dummy <- matrix(data = seq_len(16), nrow = 4, ncol = 4)
    rownames(dummy) <- paste0("row_", seq_len(nrow(dummy)))
    colnames(dummy) <- paste0("col_", seq_len(ncol(dummy)))

    createLinkedMatrix <- function(class, nNodes) {
        linkedBy <- ifelse(class == "ColumnLinkedMatrix", "columns", "rows")
        linkedMatrix <- LinkedMatrix(nrow = nrow(dummy), ncol = ncol(dummy), nNodes = nNodes, linkedBy = linkedBy, nodeInitializer = "matrixNodeInitializer")
        linkedMatrix[] <- dummy
        dimnames(linkedMatrix) <- dimnames(dummy)
        return(linkedMatrix)
    }

    CROCHET_EXTRACT_ENV <- new.env()
    CROCHET_EXTRACT_ENV$COMPARE_OBJECT <- dummy
    CROCHET_EXTRACT_ENV$OUT_OF_BOUNDS_INT <- length(dummy) + 1
    CROCHET_EXTRACT_ENV$OUT_OF_BOUNDS_CHAR <- "col_1000"

    context("ColumnLinkedMatrix with 2 nodes")
    CROCHET_EXTRACT_ENV$CUSTOM_OBJECT <- createLinkedMatrix("ColumnLinkedMatrix", 2)
    source(extract_tests, local = TRUE)

    context("RowLinkedMatrix with 2 nodes")
    CROCHET_EXTRACT_ENV$CUSTOM_OBJECT <- createLinkedMatrix("RowLinkedMatrix", 2)
    source(extract_tests, local = TRUE)

}
