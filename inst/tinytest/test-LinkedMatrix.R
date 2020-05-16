source("setup.R")

n <- 4L
p <- 4L
dimnames <- list(
    paste0("row_", seq_len(n)),
    paste0("col_", seq_len(p))
)

dummy <- createMatrix(n, p, dimnames)

for (class in c("ColumnLinkedMatrix", "RowLinkedMatrix")) {

    linkedBy <- ifelse(class == "ColumnLinkedMatrix", "columns", "rows")

    # LinkedMatrix creation

    for (nNodes in c(1, 2)) {

        linkedMatrix <- LinkedMatrix(nrow = n, ncol = p, nNodes = nNodes, linkedBy = linkedBy, nodeInitializer = "matrixNodeInitializer")
        expect_equal(nNodes(linkedMatrix), nNodes)
        expect_true(is(linkedMatrix[[1]], "matrix"))

        if (requireNamespace("ff", quietly = TRUE)) {
            linkedMatrix <- LinkedMatrix(nrow = n, ncol = p, nNodes = nNodes, linkedBy = linkedBy, nodeInitializer = "ffNodeInitializer", vmode = "integer")
            expect_equal(nNodes(linkedMatrix), nNodes)
            expect_true(is(linkedMatrix[[1]], "ff_matrix"))
        }

    }

    # Object creation

    constructor <- get(class)

    expect_error(constructor(c(1, 2, 3)), "*arguments need to be matrix-like*")

    # No input
    linkedMatrix <- constructor()
    expect_equal(nNodes(linkedMatrix), 1)
    expect_true(is.na(linkedMatrix[1, 1]))

    # Single matrix input
    linkedMatrix <- constructor(matrix(data = 0, nrow = 1, ncol = 1))
    expect_equal(nNodes(linkedMatrix), 1)
    expect_equal(dim(linkedMatrix), c(1, 1))

    # Single LinkedMatrix input
    linkedMatrix <- constructor(createLinkedMatrix(n, p, dimnames, class, 2))
    expect_equal(nNodes(linkedMatrix), 1)
    expect_equal(dim(linkedMatrix), dim(dummy))

    # Multiple matrix inputs of same order
    linkedMatrix <- constructor(matrix(data = 0, nrow = 1, ncol = 1), matrix(data = 0, nrow = 1, ncol = 1))
    expect_equal(nNodes(linkedMatrix), 2)
    if (class == "ColumnLinkedMatrix") {
        expect_equal(dim(linkedMatrix), c(1, 2))
    } else {
        expect_equal(dim(linkedMatrix), c(2, 1))
    }

    # Multiple LinkedMatrix inputs of same order
    linkedMatrix <- constructor(createLinkedMatrix(n, p, dimnames, class, 2), createLinkedMatrix(n, p, dimnames, class, 2))
    expect_equal(nNodes(linkedMatrix), 2)
    if (class == "ColumnLinkedMatrix") {
        expect_equal(dim(linkedMatrix), c(n, p * 2))
    } else {
        expect_equal(dim(linkedMatrix), c(p * 2, n))
    }

    # Multiple conformable matrix inputs of different order
    if (class == "ColumnLinkedMatrix") {
        args <- list(matrix(data = 0, nrow = 1, ncol = 3), matrix(data = 0, nrow = 1, ncol = 5))
        dims <- c(1, 8)
    } else {
        args <- list(matrix(data = 0, nrow = 3, ncol = 1), matrix(data = 0, nrow = 5, ncol = 1))
        dims <- c(8, 1)
    }
    linkedMatrix <- do.call(class, args)
    expect_equal(nNodes(linkedMatrix), 2)
    expect_equal(dim(linkedMatrix), dims)

    # Multiple unconformable matrix inputs
    if (class == "ColumnLinkedMatrix") {
        args <- list(matrix(data = 0, nrow = 3, ncol = 1), matrix(data = 0, nrow = 5, ncol = 1))
    } else {
        args <- list(matrix(data = 0, nrow = 1, ncol = 3), matrix(data = 0, nrow = 1, ncol = 5))
    }
    expect_error(do.call(class, args), "*arguments need the same number of*")

    # Warning if dimnames do not match
    dimnamesMismatches <- list(
        list(warning = FALSE, dimnames = list(NULL, NULL, NULL)),
        list(warning = FALSE, dimnames = list(letters[1:3], letters[1:3], letters[1:3])),
        list(warning = TRUE, dimnames = list(letters[1:3], NULL, NULL)),
        list(warning = TRUE, dimnames = list(letters[1:3], letters[4:6], NULL))
    )
    for (dimnamesMismatch in dimnamesMismatches) {
        if (class == "ColumnLinkedMatrix") {
            args <- list(
                matrix(data = 0, nrow = 3, ncol = 1, dimnames = list(dimnamesMismatch$dimnames[[1]], NULL)),
                matrix(data = 0, nrow = 3, ncol = 1, dimnames = list(dimnamesMismatch$dimnames[[2]], NULL)),
                matrix(data = 0, nrow = 3, ncol = 1, dimnames = list(dimnamesMismatch$dimnames[[3]], NULL))
            )
        } else {
            args <- list(
                matrix(data = 0, nrow = 1, ncol = 3, dimnames = list(NULL, dimnamesMismatch$dimnames[[1]])),
                matrix(data = 0, nrow = 1, ncol = 3, dimnames = list(NULL, dimnamesMismatch$dimnames[[2]])),
                matrix(data = 0, nrow = 1, ncol = 3, dimnames = list(NULL, dimnamesMismatch$dimnames[[3]]))
            )
        }
        if (dimnamesMismatch$warning) {
            expect_warning(do.call(class, args))
        } else {
            expect_silent(do.call(class, args))
        }
    }

    for (nNodes in seq_len(ifelse(class == "ColumnLinkedMatrix", p, n))) {

        # Prepare LinkedMatrix object
        linkedMatrix <- createLinkedMatrix(n, p, dimnames, class, nNodes)

        # dim
        expect_equal(dim(linkedMatrix), dim(dummy))

        # length
        expect_equal(length(linkedMatrix), length(dummy))

        # nNodes
        expect_equal(nNodes(linkedMatrix), nNodes)

        # bind
        if (class == "RowLinkedMatrix") {
            boundLinkedMatrix <- rbind(linkedMatrix, linkedMatrix)
            expect_equal(dim(boundLinkedMatrix), c(n * 2, p))
            expect_equal(nNodes(boundLinkedMatrix), nNodes * 2)
        } else {
            boundLinkedMatrix <- cbind(linkedMatrix, linkedMatrix)
            expect_equal(dim(boundLinkedMatrix), c(n, p * 2))
            expect_equal(nNodes(boundLinkedMatrix), nNodes * 2)
        }

    }

}
