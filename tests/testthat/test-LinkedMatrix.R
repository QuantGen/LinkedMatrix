# Prepare dummy data
genotypes <- matrix(c(4, 4, 4, 3, 2, 3, 1, 2, 1), nrow = 3, ncol = 3)
rownames(genotypes) <- paste0("id_", 1:nrow(genotypes))
colnames(genotypes) <- paste0("mrk_", 1:ncol(genotypes))

createLinkedMatrix <- function(class, nNodes) {
    linkedBy <- ifelse(class == "ColumnLinkedMatrix", "columns", "rows")
    linkedMatrix <- LinkedMatrix(nrow = nrow(genotypes), ncol = ncol(genotypes), nNodes = nNodes, linkedBy = linkedBy, nodeInitializer = "matrixNodeInitializer")
    rownames(linkedMatrix) <- paste0("id_", 1:nrow(genotypes))
    colnames(linkedMatrix) <- paste0("mrk_", 1:ncol(genotypes))
    linkedMatrix[] <- genotypes
    return(linkedMatrix)
}

for (class in c("ColumnLinkedMatrix", "RowLinkedMatrix")) {

    context(class)

    linkedBy <- ifelse(class == "ColumnLinkedMatrix", "columns", "rows")

    test_that("LinkedMatrix creation", {

        for (nNodes in c(1, 2)) {

            linkedMatrix <- LinkedMatrix(nrow = nrow(genotypes), ncol = ncol(genotypes), nNodes = nNodes, linkedBy = linkedBy, nodeInitializer = "matrixNodeInitializer")
            expect_equal(nNodes(linkedMatrix), nNodes)
            expect_is(linkedMatrix[[1]], "matrix")

            if (requireNamespace("ff", quietly = TRUE)) {
                linkedMatrix <- LinkedMatrix(nrow = nrow(genotypes), ncol = ncol(genotypes), nNodes = nNodes, linkedBy = linkedBy, nodeInitializer = "ffNodeInitializer", vmode = "integer")
                expect_equal(nNodes(linkedMatrix), nNodes)
                expect_is(linkedMatrix[[1]], "ff_matrix")
            }

        }

    })

    test_that(paste(class, "creation"), {

        linkedMatrix <- new(class)
        expect_equal(length(linkedMatrix), 1)
        expect_equal(linkedMatrix[1, 1], NA)

        linkedMatrix <- new(class, matrix(nrow = 1, ncol = 1, 0))
        expect_equal(length(linkedMatrix), 1)
        expect_equal(linkedMatrix[1, 1], 0)

        linkedMatrix <- new(class, matrix(nrow = 1, ncol = 1, 0), matrix(nrow = 1, ncol = 1, 0))
        expect_equal(length(linkedMatrix), 2)
        expect_equal(linkedMatrix[1, 1], 0)

        expect_error(new(class, c(1, 2, 3)), "*arguments need to be matrix-like*")

        if (class == "ColumnLinkedMatrix") {
            args <- list(matrix(nrow = 1, ncol = 1, 0), matrix(nrow = 2, ncol = 1, 0))
        } else {
            args <- list(matrix(nrow = 1, ncol = 1, 0), matrix(nrow = 1, ncol = 2, 0))
        }
        expect_error(do.call(class, args), "*arguments need the same number of*")

    })

    for (nNodes in 1:ifelse(class == "ColumnLinkedMatrix", ncol(genotypes), nrow(genotypes))) {

        context(paste0(class, " with ", nNodes, " nodes"))

        # Prepare LinkedMatrix object
        linkedMatrix <- createLinkedMatrix(class, nNodes)

        test_that("subsetting", {

            expect_equal(linkedMatrix[], genotypes)

            # expect_equal(linkedMatrix[1], genotypes[1]) Not implemented yet
            expect_equal(linkedMatrix[1, ], genotypes[1, ])
            expect_equal(linkedMatrix[, 1], genotypes[, 1])
            expect_equal(linkedMatrix[1, 1], genotypes[1, 1])
            expect_equal(linkedMatrix[1, , drop = FALSE], genotypes[1, , drop = FALSE])
            expect_equal(linkedMatrix[, 1, drop = FALSE], genotypes[, 1, drop = FALSE])
            expect_equal(linkedMatrix[1, 1, drop = FALSE], genotypes[1, 1, drop = FALSE])

            # expect_equal(linkedMatrix[1:2], genotypes[1:2]) Not implemented yet
            expect_equal(linkedMatrix[1:2, ], genotypes[1:2, ])
            expect_equal(linkedMatrix[, 1:2], genotypes[, 1:2])
            expect_equal(linkedMatrix[1:2, 1:2], genotypes[1:2, 1:2])
            expect_equal(linkedMatrix[1:2, , drop = FALSE], genotypes[1:2, , drop = FALSE])
            expect_equal(linkedMatrix[, 1:2, drop = FALSE], genotypes[, 1:2, drop = FALSE])
            expect_equal(linkedMatrix[1:2, 1:2, drop = FALSE], genotypes[1:2, 1:2, drop = FALSE])

            # expect_equal(linkedMatrix[2:1], genotypes[2:1]) Not implemented yet
            expect_equal(linkedMatrix[2:1, ], genotypes[2:1, ])
            expect_equal(linkedMatrix[, 2:1], genotypes[, 2:1])
            expect_equal(linkedMatrix[2:1, 2:1], genotypes[2:1, 2:1])
            expect_equal(linkedMatrix[2:1, , drop = FALSE], genotypes[2:1, , drop = FALSE])
            expect_equal(linkedMatrix[, 2:1, drop = FALSE], genotypes[, 2:1, drop = FALSE])
            expect_equal(linkedMatrix[2:1, 2:1, drop = FALSE], genotypes[2:1, 2:1, drop = FALSE])

            # expect_equal(linkedMatrix[c(3, 1)], genotypes[c(3, 1)]) Not implemented yet
            expect_equal(linkedMatrix[c(3, 1), ], genotypes[c(3, 1), ])
            expect_equal(linkedMatrix[, c(3, 1)], genotypes[, c(3, 1)])
            expect_equal(linkedMatrix[c(3, 1), c(3, 1)], genotypes[c(3, 1), c(3, 1)])
            expect_equal(linkedMatrix[c(3, 1), , drop = FALSE], genotypes[c(3, 1), , drop = FALSE])
            expect_equal(linkedMatrix[, c(3, 1), drop = FALSE], genotypes[, c(3, 1), drop = FALSE])
            expect_equal(linkedMatrix[c(3, 1), c(3, 1), drop = FALSE], genotypes[c(3, 1), c(3, 1), drop = FALSE])

            # expect_equal(linkedMatrix[genotypes > 1], genotypes[genotypes > 1]) Not implemented yet
            logRow <- rep_len(c(TRUE, FALSE), nrow(genotypes))
            logCol <- rep_len(c(TRUE, FALSE), ncol(genotypes))
            expect_equal(linkedMatrix[logRow, ], genotypes[logRow, ])
            expect_equal(linkedMatrix[, logCol], genotypes[, logCol])
            expect_equal(linkedMatrix[logRow, logCol], genotypes[logRow, logCol])
            expect_equal(linkedMatrix[logRow, , drop = FALSE], genotypes[logRow, , drop = FALSE])
            expect_equal(linkedMatrix[, logCol, drop = FALSE], genotypes[, logCol, drop = FALSE])
            expect_equal(linkedMatrix[logRow, logCol, drop = FALSE], genotypes[logRow, logCol, drop = FALSE])

            expect_equal(linkedMatrix["id_1", ], genotypes["id_1", ])
            expect_equal(linkedMatrix[, "mrk_1"], genotypes[, "mrk_1"])
            expect_equal(linkedMatrix["id_1", "mrk_1"], genotypes["id_1", "mrk_1"])
            expect_equal(linkedMatrix["id_1", , drop = FALSE], genotypes["id_1", , drop = FALSE])
            expect_equal(linkedMatrix[, "mrk_1", drop = FALSE], genotypes[, "mrk_1", drop = FALSE])
            expect_equal(linkedMatrix["id_1", "mrk_1", drop = FALSE], genotypes["id_1", "mrk_1", drop = FALSE])

            expect_equal(linkedMatrix[c("id_1", "id_2"), ], genotypes[c("id_1", "id_2"), ])
            expect_equal(linkedMatrix[, c("mrk_1", "mrk_2")], genotypes[, c("mrk_1", "mrk_2")])
            expect_equal(linkedMatrix[c("id_1", "id_2"), c("mrk_1", "mrk_2")], genotypes[c("id_1", "id_2"), c("mrk_1", "mrk_2")])
            expect_equal(linkedMatrix[c("id_1", "id_2"), , drop = FALSE], genotypes[c("id_1", "id_2"), , drop = FALSE])
            expect_equal(linkedMatrix[, c("mrk_1", "mrk_2"), drop = FALSE], genotypes[, c("mrk_1", "mrk_2"), drop = FALSE])
            expect_equal(linkedMatrix[c("id_1", "id_2"), c("mrk_1", "mrk_2"), drop = FALSE], genotypes[c("id_1", "id_2"), c("mrk_1", "mrk_2"), drop = FALSE])

            expect_equal(linkedMatrix[c("id_2", "id_1"), ], genotypes[c("id_2", "id_1"), ])
            expect_equal(linkedMatrix[, c("mrk_2", "mrk_1")], genotypes[, c("mrk_2", "mrk_1")])
            expect_equal(linkedMatrix[c("id_2", "id_1"), c("mrk_2", "mrk_1")], genotypes[c("id_2", "id_1"), c("mrk_2", "mrk_1")])
            expect_equal(linkedMatrix[c("id_2", "id_1"), , drop = FALSE], genotypes[c("id_2", "id_1"), , drop = FALSE])
            expect_equal(linkedMatrix[, c("mrk_2", "mrk_1"), drop = FALSE], genotypes[, c("mrk_2", "mrk_1"), drop = FALSE])
            expect_equal(linkedMatrix[c("id_2", "id_1"), c("mrk_2", "mrk_1"), drop = FALSE], genotypes[c("id_2", "id_1"), c("mrk_2", "mrk_1"), drop = FALSE])

            expect_equal(linkedMatrix[c("id_3", "id_1"), ], genotypes[c("id_3", "id_1"), ])
            expect_equal(linkedMatrix[, c("mrk_3", "mrk_1")], genotypes[, c("mrk_3", "mrk_1")])
            expect_equal(linkedMatrix[c("id_3", "id_1"), c("mrk_3", "mrk_1")], genotypes[c("id_3", "id_1"), c("mrk_3", "mrk_1")])
            expect_equal(linkedMatrix[c("id_3", "id_1"), , drop = FALSE], genotypes[c("id_3", "id_1"), , drop = FALSE])
            expect_equal(linkedMatrix[, c("mrk_3", "mrk_1"), drop = FALSE], genotypes[, c("mrk_3", "mrk_1"), drop = FALSE])
            expect_equal(linkedMatrix[c("id_3", "id_1"), c("mrk_3", "mrk_1"), drop = FALSE], genotypes[c("id_3", "id_1"), c("mrk_3", "mrk_1"), drop = FALSE])

        })

        test_that("replacement", {

            # Generate new genotypes for replacement
            replacement <- matrix(c(3, 1, 3, 2, 4, 3, 1, 1, 2), nrow = 3, ncol = 3)
            colnames(replacement) <- paste0("mrk_", 1:3)
            rownames(replacement) <- paste0("id_", 1:3)
            comparison <- genotypes

            testAndRestore <- function(label) {
                expect_equal(linkedMatrix[], comparison, label = label)
                linkedMatrix <- createLinkedMatrix(class, nNodes)
                assign("linkedMatrix", linkedMatrix, parent.frame())
                assign("comparison", genotypes, parent.frame())
            }

            linkedMatrix[] <- replacement
            comparison[] <- replacement
            testAndRestore("[]")

            linkedMatrix[1, ] <- replacement[1, ]
            comparison[1, ] <- replacement[1, ]
            testAndRestore("[1, ]")
            linkedMatrix[, 1] <- replacement[, 1]
            comparison[, 1] <- replacement[, 1]
            testAndRestore("[, 1]")
            linkedMatrix[1, 1] <- replacement[1, 1]
            comparison[1, 1] <- replacement[1, 1]
            testAndRestore("[1, 1]")

            linkedMatrix[1:2, ] <- replacement[1:2, ]
            comparison[1:2, ] <- replacement[1:2, ]
            testAndRestore("[1:2, ]")
            linkedMatrix[, 1:2] <- replacement[, 1:2]
            comparison[, 1:2] <- replacement[, 1:2]
            testAndRestore("[, 1:2]")
            linkedMatrix[1:2, 1:2] <- replacement[1:2, 1:2]
            comparison[1:2, 1:2] <- replacement[1:2, 1:2]
            testAndRestore("[1:2, 1:2]")

            linkedMatrix[2:1, ] <- replacement[2:1, ]
            comparison[2:1, ] <- replacement[2:1, ]
            testAndRestore("[2:1, ]")
            linkedMatrix[, 2:1] <- replacement[, 2:1]
            comparison[, 2:1] <- replacement[, 2:1]
            testAndRestore("[, 2:1]")
            linkedMatrix[2:1, 2:1] <- replacement[2:1, 2:1]
            comparison[2:1, 2:1] <- replacement[2:1, 2:1]
            testAndRestore("[2:1, 2:1]")

            linkedMatrix[c(3, 1), ] <- replacement[c(3, 1), ]
            comparison[c(3, 1), ] <- replacement[c(3, 1), ]
            testAndRestore("[c(3, 1), ]")
            linkedMatrix[, c(3, 1)] <- replacement[, c(3, 1)]
            comparison[, c(3, 1)] <- replacement[, c(3, 1)]
            testAndRestore("[, c(3, 1)]")
            linkedMatrix[c(3, 1), c(3, 1)] <- replacement[c(3, 1), c(3, 1)]
            comparison[c(3, 1), c(3, 1)] <- replacement[c(3, 1), c(3, 1)]
            testAndRestore("[c(3, 1), c(3, 1)]")

            linkedMatrix[1, ] <- NA
            comparison[1, ] <- NA
            testAndRestore("[1, ] <- NA")
            linkedMatrix[, 1] <- NA
            comparison[, 1] <- NA
            testAndRestore("[, 1] <- NA")
            linkedMatrix[1, 1] <- NA
            comparison[1, 1] <- NA
            testAndRestore("[1, 1] <- NA")

        })

        test_that("dim", {
            expect_equal(dim(linkedMatrix), dim(genotypes))
        })

        test_that("length", {
            expect_equal(length(linkedMatrix), length(genotypes))
        })

        test_that("nNodes", {
            expect_equal(nNodes(linkedMatrix), nNodes)
        })

    }

} 
