library(LinkedMatrix)

# Prepare dummy data
genotypes <- matrix(c(4, 4, 4, 3, 2, 3, 1, 2, 1), nrow = 3, ncol = 3)
colnames(genotypes) <- paste0("mrk_", 1:3)
rownames(genotypes) <- paste0("id_", 1:3)

createLinkedMatrix <- function(class, nNodes) {
    list <- new(class)
    if (nNodes == 2) {
        if (class == "ColumnLinkedMatrix") {
            list[[1]] <- genotypes[, 1:2, drop = FALSE]
            list[[2]] <- genotypes[, 3, drop = FALSE]
        } else {
            list[[1]] <- genotypes[1:2, , drop = FALSE]
            list[[2]] <- genotypes[3, , drop = FALSE]
        }
    } else {
        list[[1]] <- genotypes
    }
    list[] <- genotypes
    colnames(list) <- paste0("mrk_", 1:3)
    rownames(list) <- paste0("id_", 1:3)
    return(list)
}

for (class in c("ColumnLinkedMatrix", "RowLinkedMatrix")) {
    
    for (nNodes in 1:2) {
        
        context(paste0(class, " (nNodes = ", nNodes, ")"))
        
        # Prepare LinkedMatrix object
        list <- createLinkedMatrix(class, nNodes)
        
        test_that("subsetting", {
            
            expect_true(all.equal(list[], genotypes))
            
            # expect_true(all.equal(list[1], genotypes[1])) Not implemented yet
            expect_true(all.equal(list[1, ], genotypes[1, ]))
            expect_true(all.equal(list[, 1], genotypes[, 1]))
            expect_true(all.equal(list[1, 1], genotypes[1, 1]))
            expect_true(all.equal(list[1, , drop = FALSE], genotypes[1, , drop = FALSE]))
            expect_true(all.equal(list[, 1, drop = FALSE], genotypes[, 1, drop = FALSE]))
            expect_true(all.equal(list[1, 1, drop = FALSE], genotypes[1, 1, drop = FALSE]))
            
            # expect_true(all.equal(list[1:2], genotypes[1:2])) Not implemented yet
            expect_true(all.equal(list[1:2, ], genotypes[1:2, ]))
            expect_true(all.equal(list[, 1:2], genotypes[, 1:2]))
            expect_true(all.equal(list[1:2, 1:2], genotypes[1:2, 1:2]))
            expect_true(all.equal(list[1:2, , drop = FALSE], genotypes[1:2, , drop = FALSE]))
            expect_true(all.equal(list[, 1:2, drop = FALSE], genotypes[, 1:2, drop = FALSE]))
            expect_true(all.equal(list[1:2, 1:2, drop = FALSE], genotypes[1:2, 1:2, drop = FALSE]))
            
            # expect_true(all.equal(list[2:1], genotypes[2:1])) Not implemented yet
            expect_true(all.equal(list[2:1, ], genotypes[2:1, ]))
            expect_true(all.equal(list[, 2:1], genotypes[, 2:1]))
            expect_true(all.equal(list[2:1, 2:1], genotypes[2:1, 2:1]))
            expect_true(all.equal(list[2:1, , drop = FALSE], genotypes[2:1, , drop = FALSE]))
            expect_true(all.equal(list[, 2:1, drop = FALSE], genotypes[, 2:1, drop = FALSE]))
            expect_true(all.equal(list[2:1, 2:1, drop = FALSE], genotypes[2:1, 2:1, drop = FALSE]))
            
            # expect_true(all.equal(list[c(3, 1)], genotypes[c(3, 1)])) Not implemented yet
            expect_true(all.equal(list[c(3, 1), ], genotypes[c(3, 1), ]))
            expect_true(all.equal(list[, c(3, 1)], genotypes[, c(3, 1)]))
            expect_true(all.equal(list[c(3, 1), c(3, 1)], genotypes[c(3, 1), c(3, 1)]))
            expect_true(all.equal(list[c(3, 1), , drop = FALSE], genotypes[c(3, 1), , drop = FALSE]))
            expect_true(all.equal(list[, c(3, 1), drop = FALSE], genotypes[, c(3, 1), drop = FALSE]))
            expect_true(all.equal(list[c(3, 1), c(3, 1), drop = FALSE], genotypes[c(3, 1), c(3, 1), drop = FALSE]))
            
            # expect_true(all.equal(list[genotypes > 1], genotypes[genotypes > 1])) Not implemented yet
            expect_true(all.equal(list[c(TRUE, FALSE, TRUE), ], genotypes[c(TRUE, FALSE, TRUE), ]))
            expect_true(all.equal(list[, c(TRUE, FALSE, TRUE)], genotypes[, c(TRUE, FALSE, TRUE)]))
            expect_true(all.equal(list[c(TRUE, FALSE, TRUE), c(TRUE, FALSE, TRUE)], genotypes[c(TRUE, FALSE, TRUE), c(TRUE, FALSE, TRUE)]))
            expect_true(all.equal(list[c(TRUE, FALSE, TRUE), , drop = FALSE], genotypes[c(TRUE, FALSE, TRUE), , drop = FALSE]))
            expect_true(all.equal(list[, c(TRUE, FALSE, TRUE), drop = FALSE], genotypes[, c(TRUE, FALSE, TRUE), drop = FALSE]))
            expect_true(all.equal(list[c(TRUE, FALSE, TRUE), c(TRUE, FALSE, TRUE), drop = FALSE], genotypes[c(TRUE, FALSE, TRUE), c(TRUE, FALSE, TRUE), drop = FALSE]))
            
            expect_true(all.equal(list["id_1", ], genotypes["id_1", ]))
            expect_true(all.equal(list[, "mrk_1"], genotypes[, "mrk_1"]))
            expect_true(all.equal(list["id_1", "mrk_1"], genotypes["id_1", "mrk_1"]))
            expect_true(all.equal(list["id_1", , drop = FALSE], genotypes["id_1", , drop = FALSE]))
            expect_true(all.equal(list[, "mrk_1", drop = FALSE], genotypes[, "mrk_1", drop = FALSE]))
            expect_true(all.equal(list["id_1", "mrk_1", drop = FALSE], genotypes["id_1", "mrk_1", drop = FALSE]))
            
            expect_true(all.equal(list[c("id_1", "id_2"), ], genotypes[c("id_1", "id_2"), ]))
            expect_true(all.equal(list[, c("mrk_1", "mrk_2")], genotypes[, c("mrk_1", "mrk_2")]))
            expect_true(all.equal(list[c("id_1", "id_2"), c("mrk_1", "mrk_2")], genotypes[c("id_1", "id_2"), c("mrk_1", "mrk_2")]))
            expect_true(all.equal(list[c("id_1", "id_2"), , drop = FALSE], genotypes[c("id_1", "id_2"), , drop = FALSE]))
            expect_true(all.equal(list[, c("mrk_1", "mrk_2"), drop = FALSE], genotypes[, c("mrk_1", "mrk_2"), drop = FALSE]))
            expect_true(all.equal(list[c("id_1", "id_2"), c("mrk_1", "mrk_2"), drop = FALSE], genotypes[c("id_1", "id_2"), c("mrk_1", "mrk_2"), drop = FALSE]))
            
            expect_true(all.equal(list[c("id_2", "id_1"), ], genotypes[c("id_2", "id_1"), ]))
            expect_true(all.equal(list[, c("mrk_2", "mrk_1")], genotypes[, c("mrk_2", "mrk_1")]))
            expect_true(all.equal(list[c("id_2", "id_1"), c("mrk_2", "mrk_1")], genotypes[c("id_2", "id_1"), c("mrk_2", "mrk_1")]))
            expect_true(all.equal(list[c("id_2", "id_1"), , drop = FALSE], genotypes[c("id_2", "id_1"), , drop = FALSE]))
            expect_true(all.equal(list[, c("mrk_2", "mrk_1"), drop = FALSE], genotypes[, c("mrk_2", "mrk_1"), drop = FALSE]))
            expect_true(all.equal(list[c("id_2", "id_1"), c("mrk_2", "mrk_1"), drop = FALSE], genotypes[c("id_2", "id_1"), c("mrk_2", "mrk_1"), drop = FALSE]))
            
            expect_true(all.equal(list[c("id_3", "id_1"), ], genotypes[c("id_3", "id_1"), ]))
            expect_true(all.equal(list[, c("mrk_3", "mrk_1")], genotypes[, c("mrk_3", "mrk_1")]))
            expect_true(all.equal(list[c("id_3", "id_1"), c("mrk_3", "mrk_1")], genotypes[c("id_3", "id_1"), c("mrk_3", "mrk_1")]))
            expect_true(all.equal(list[c("id_3", "id_1"), , drop = FALSE], genotypes[c("id_3", "id_1"), , drop = FALSE]))
            expect_true(all.equal(list[, c("mrk_3", "mrk_1"), drop = FALSE], genotypes[, c("mrk_3", "mrk_1"), drop = FALSE]))
            expect_true(all.equal(list[c("id_3", "id_1"), c("mrk_3", "mrk_1"), drop = FALSE], genotypes[c("id_3", "id_1"), c("mrk_3", "mrk_1"), drop = FALSE]))
            
        })
        
        test_that("replacement", {
            
            # Generate new genotypes for replacement
            replacement <- matrix(c(3, 1, 3, 2, 4, 3, 1, 1, 2), nrow = 3, ncol = 3)
            colnames(replacement) <- paste0("mrk_", 1:3)
            rownames(replacement) <- paste0("id_", 1:3)
            comparison <- genotypes
            
            testAndRestore <- function(label) {
                expect_equal(all.equal(list[], comparison), TRUE, label = label)
                list <- createLinkedMatrix(class, nNodes)
                assign("list", list, parent.frame())
                assign("comparison", genotypes, parent.frame())
            }
            
            list[] <- replacement
            comparison[] <- replacement
            testAndRestore("[]")
            
            list[1, ] <- replacement[1, ]
            comparison[1, ] <- replacement[1, ]
            testAndRestore("[1, ]")
            list[, 1] <- replacement[, 1]
            comparison[, 1] <- replacement[, 1]
            testAndRestore("[, 1]")
            list[1, 1] <- replacement[1, 1]
            comparison[1, 1] <- replacement[1, 1]
            testAndRestore("[1, 1]")
            
            list[1:2, ] <- replacement[1:2, ]
            comparison[1:2, ] <- replacement[1:2, ]
            testAndRestore("[1:2, ]")
            list[, 1:2] <- replacement[, 1:2]
            comparison[, 1:2] <- replacement[, 1:2]
            testAndRestore("[, 1:2]")
            list[1:2, 1:2] <- replacement[1:2, 1:2]
            comparison[1:2, 1:2] <- replacement[1:2, 1:2]
            testAndRestore("[1:2, 1:2]")
            
            list[2:1, ] <- replacement[2:1, ]
            comparison[2:1, ] <- replacement[2:1, ]
            testAndRestore("[2:1, ]")
            list[, 2:1] <- replacement[, 2:1]
            comparison[, 2:1] <- replacement[, 2:1]
            testAndRestore("[, 2:1]")
            list[2:1, 2:1] <- replacement[2:1, 2:1]
            comparison[2:1, 2:1] <- replacement[2:1, 2:1]
            testAndRestore("[2:1, 2:1]")
            
            list[c(3, 1), ] <- replacement[c(3, 1), ]
            comparison[c(3, 1), ] <- replacement[c(3, 1), ]
            testAndRestore("[c(3, 1), ]")
            list[, c(3, 1)] <- replacement[, c(3, 1)]
            comparison[, c(3, 1)] <- replacement[, c(3, 1)]
            testAndRestore("[, c(3, 1)]")
            list[c(3, 1), c(3, 1)] <- replacement[c(3, 1), c(3, 1)]
            comparison[c(3, 1), c(3, 1)] <- replacement[c(3, 1), c(3, 1)]
            testAndRestore("[c(3, 1), c(3, 1)]")
            
            list[1, ] <- NA
            comparison[1, ] <- NA
            testAndRestore("[1, ] <- NA")
            list[, 1] <- NA
            comparison[, 1] <- NA
            testAndRestore("[, 1] <- NA")
            list[1, 1] <- NA
            comparison[1, 1] <- NA
            testAndRestore("[1, 1] <- NA")
            
        })
        
        test_that("dim", {
            expect_equal(dim(list), dim(genotypes))
        })
        
        test_that("apply", {
            
            expect_true(all.equal(colMeans(list), colMeans(genotypes)))
            expect_true(all.equal(colSums(list), colSums(genotypes)))
            expect_true(all.equal(rowMeans(list), rowMeans(genotypes)))
            expect_true(all.equal(rowSums(list), rowSums(genotypes)))
            
            # Introduce NA
            genotypes_na <- genotypes
            genotypes_na[1, 1] <- NA
            list[1, 1] <- NA
            
            expect_warning(colMeans(list))
            expect_warning(colSums(list))
            expect_warning(rowMeans(list))
            expect_warning(rowSums(list))
            
            expect_true(all.equal(colMeans(list, na.rm = FALSE), colMeans(genotypes_na, na.rm = FALSE)))
            expect_true(all.equal(colSums(list, na.rm = FALSE), colSums(genotypes_na, na.rm = FALSE)))
            expect_true(all.equal(rowMeans(list, na.rm = FALSE), rowMeans(genotypes_na, na.rm = FALSE)))
            expect_true(all.equal(rowSums(list, na.rm = FALSE), rowSums(genotypes_na, na.rm = FALSE)))
            
            expect_true(all.equal(colMeans(list, na.rm = TRUE), colMeans(genotypes_na, na.rm = TRUE)))
            expect_true(all.equal(colSums(list, na.rm = TRUE), colSums(genotypes_na, na.rm = TRUE)))
            expect_true(all.equal(rowMeans(list, na.rm = TRUE), rowMeans(genotypes_na, na.rm = TRUE)))
            expect_true(all.equal(rowSums(list, na.rm = TRUE), rowSums(genotypes_na, na.rm = TRUE)))
            
            # Revert NA
            list[] <- genotypes
            
        })
        
    }
    
} 
