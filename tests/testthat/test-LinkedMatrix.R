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

for (class in c("ColumnLinkedMatrix", "RowLinkedMatrix")) {

    context(class)

    linkedBy <- ifelse(class == "ColumnLinkedMatrix", "columns", "rows")

    test_that("LinkedMatrix creation", {

        for (nNodes in c(1, 2)) {

            linkedMatrix <- LinkedMatrix(nrow = nrow(dummy), ncol = ncol(dummy), nNodes = nNodes, linkedBy = linkedBy, nodeInitializer = "matrixNodeInitializer")
            expect_equal(nNodes(linkedMatrix), nNodes)
            expect_is(linkedMatrix[[1]], "matrix")

            if (requireNamespace("ff", quietly = TRUE)) {
                linkedMatrix <- LinkedMatrix(nrow = nrow(dummy), ncol = ncol(dummy), nNodes = nNodes, linkedBy = linkedBy, nodeInitializer = "ffNodeInitializer", vmode = "integer")
                expect_equal(nNodes(linkedMatrix), nNodes)
                expect_is(linkedMatrix[[1]], "ff_matrix")
            }

        }

    })

    test_that(paste(class, "creation"), {

        expect_error(new(class, c(1, 2, 3)), "*arguments need to be matrix-like*")

        # No input
        linkedMatrix <- new(class)
        expect_equal(nNodes(linkedMatrix), 1)
        expect_true(is.na(linkedMatrix[1, 1]))

        # Single matrix input
        linkedMatrix <- new(class, matrix(data = 0, nrow = 1, ncol = 1))
        expect_equal(nNodes(linkedMatrix), 1)
        expect_equal(dim(linkedMatrix), c(1, 1))

        # Single LinkedMatrix input
        linkedMatrix <- new(class, createLinkedMatrix(class, 2))
        expect_equal(nNodes(linkedMatrix), 1)
        expect_equal(dim(linkedMatrix), dim(dummy))

        # Multiple matrix inputs of same order
        linkedMatrix <- new(class, matrix(data = 0, nrow = 1, ncol = 1), matrix(data = 0, nrow = 1, ncol = 1))
        expect_equal(nNodes(linkedMatrix), 2)
        if (class == "ColumnLinkedMatrix") {
            expect_equal(dim(linkedMatrix), c(1, 2))
        } else {
            expect_equal(dim(linkedMatrix), c(2, 1))
        }

        # Multiple LinkedMatrix inputs of same order
        linkedMatrix <- new(class, createLinkedMatrix(class, 2), createLinkedMatrix(class, 2))
        expect_equal(nNodes(linkedMatrix), 2)
        if (class == "ColumnLinkedMatrix") {
            expect_equal(dim(linkedMatrix), c(nrow(dummy), ncol(dummy) * 2))
        } else {
            expect_equal(dim(linkedMatrix), c(ncol(dummy) * 2, nrow(dummy)))
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
            list(regexp = NA, dimnames = list(NULL, NULL, NULL)),
            list(regexp = NA, dimnames = list(letters[1:3], NULL, NULL)),
            list(regexp = NULL, dimnames = list(letters[1:3], letters[4:6], NULL))
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
            expect_warning(do.call(class, args), regexp = dimnamesMismatch$regexp)
        }
    })

    for (nNodes in seq_len(ifelse(class == "ColumnLinkedMatrix", ncol(dummy), nrow(dummy)))) {

        context(paste0(class, " with ", nNodes, " nodes"))

        # Prepare LinkedMatrix object
        linkedMatrix <- createLinkedMatrix(class, nNodes)

        test_that("replacement", {

            # Generate new dummy for replacement
            replacement <- matrix(data = seq_len(16) * 10, nrow = 4, ncol = 4)
            rownames(replacement) <- paste0("row_", seq_len(nrow(replacement)))
            colnames(replacement) <- paste0("col_", seq_len(ncol(replacement)))
            comparison <- dummy

            idx2 <- expand.grid(seq_len(nrow(dummy)), seq_len(ncol(dummy)))

            testAndRestore <- function(info) {
                expect_equal(linkedMatrix[], comparison, info = info)
                linkedMatrix <- createLinkedMatrix(class, nNodes)
                assign("linkedMatrix", linkedMatrix, parent.frame())
                assign("comparison", dummy, parent.frame())
            }

            linkedMatrix[] <- replacement
            comparison[] <- replacement
            testAndRestore("[]")

            for (i in seq_len(nrow(dummy))) {

                linkedMatrix[i, ] <- replacement[i, ]
                comparison[i, ] <- replacement[i, ]
                testAndRestore(paste0("[", i, ", ]"))

                linkedMatrix[i, ] <- NA
                comparison[i, ] <- NA
                testAndRestore(paste0("[", i, ", ] <- NA"))

            }

            for (i in seq_len(ncol(dummy))) {

                linkedMatrix[, i] <- replacement[, i]
                comparison[, i] <- replacement[, i]
                testAndRestore(paste0("[, ", i, "]"))

                linkedMatrix[, i] <- NA
                comparison[, i] <- NA
                testAndRestore(paste0("[, ", i, "] <- NA"))

            }

            for (i in seq_len(nrow(idx2))) {

                linkedMatrix[idx2[i, 1], idx2[i, 2]] <- replacement[idx2[i, 1], idx2[i, 2]]
                comparison[idx2[i, 1], idx2[i, 2]] <- replacement[idx2[i, 1], idx2[i, 2]]
                testAndRestore(paste0("[", idx2[i, 1], ", ", idx2[i, 2], "]"))

                linkedMatrix[idx2[i, 1]:idx2[i, 2], ] <- replacement[idx2[i, 1]:idx2[i, 2], ]
                comparison[idx2[i, 1]:idx2[i, 2], ] <- replacement[idx2[i, 1]:idx2[i, 2], ]
                testAndRestore(paste0("[", idx2[i, 1], ":", idx2[i, 2], ", ]"))

                linkedMatrix[, idx2[i, 1]:idx2[i, 2]] <- replacement[, idx2[i, 1]:idx2[i, 2]]
                comparison[, idx2[i, 1]:idx2[i, 2]] <- replacement[, idx2[i, 1]:idx2[i, 2]]
                testAndRestore(paste0("[, ", idx2[i, 1], ":", idx2[i, 2], "]"))

                linkedMatrix[idx2[i, 1]:idx2[i, 2], idx2[i, 1]:idx2[i, 2]] <- replacement[idx2[i, 1]:idx2[i, 2], idx2[i, 1]:idx2[i, 2]]
                comparison[idx2[i, 1]:idx2[i, 2], idx2[i, 1]:idx2[i, 2]] <- replacement[idx2[i, 1]:idx2[i, 2], idx2[i, 1]:idx2[i, 2]]
                testAndRestore(paste0("[", idx2[i, 1], ":", idx2[i, 2], ", ", idx2[i, 1], ":", idx2[i, 2], "]"))

                linkedMatrix[c(idx2[i, 1], idx2[i, 2]), ] <- replacement[c(idx2[i, 1], idx2[i, 2]), ]
                comparison[c(idx2[i, 1], idx2[i, 2]), ] <- replacement[c(idx2[i, 1], idx2[i, 2]), ]
                testAndRestore(paste0("[c(", idx2[i, 1], ", ", idx2[i, 2], "), ]"))

                linkedMatrix[, c(idx2[i, 1], idx2[i, 2])] <- replacement[, c(idx2[i, 1], idx2[i, 2])]
                comparison[, c(idx2[i, 1], idx2[i, 2])] <- replacement[, c(idx2[i, 1], idx2[i, 2])]
                testAndRestore(paste0("[, c(", idx2[i, 1], ", ", idx2[i, 2], ")]"))

                linkedMatrix[c(idx2[i, 1], idx2[i, 2]), c(idx2[i, 1], idx2[i, 2])] <- replacement[c(idx2[i, 1], idx2[i, 2]), c(idx2[i, 1], idx2[i, 2])]
                comparison[c(idx2[i, 1], idx2[i, 2]), c(idx2[i, 1], idx2[i, 2])] <- replacement[c(idx2[i, 1], idx2[i, 2]), c(idx2[i, 1], idx2[i, 2])]
                testAndRestore(paste0("[c(", idx2[i, 1], ", ", idx2[i, 2], "), c(", idx2[i, 1], ", ", idx2[i, 2], ")]"))

                linkedMatrix[idx2[i, 1]:idx2[i, 2], idx2[i, 1]:idx2[i, 2]] <- NA
                comparison[idx2[i, 1]:idx2[i, 2], idx2[i, 1]:idx2[i, 2]] <- NA
                testAndRestore(paste0("[", idx2[i, 1], ", ", idx2[i, 2], "] <- NA"))

            }

        })

        test_that("dim", {
            expect_equal(dim(linkedMatrix), dim(dummy))
        })

        test_that("length", {
            expect_equal(length(linkedMatrix), length(dummy))
        })

        test_that("nNodes", {
            expect_equal(nNodes(linkedMatrix), nNodes)
        })

        test_that("bind", {

            if (class == "RowLinkedMatrix") {

                boundLinkedMatrix <- rbind(linkedMatrix, linkedMatrix)
                expect_equal(dim(boundLinkedMatrix), c(nrow(dummy) * 2, ncol(dummy)))
                expect_equal(nNodes(boundLinkedMatrix), nNodes * 2)

                expect_error(cbind(linkedMatrix, linkedMatrix))

            } else {

                boundLinkedMatrix <- cbind(linkedMatrix, linkedMatrix)
                expect_equal(dim(boundLinkedMatrix), c(nrow(dummy), ncol(dummy) * 2))
                expect_equal(nNodes(boundLinkedMatrix), nNodes * 2)

                expect_error(rbind(linkedMatrix, linkedMatrix))

            }

        })

    }

}
