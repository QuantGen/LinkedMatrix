library(BGData)

context("BGData")

# Create temporary directory
tmpPath <- paste0("/tmp/BGData-", randomString(), "/")
dir.create(tmpPath)

restoreGenotypes <- function () {
    genotypes <- matrix(c(4, 4, 4, 3, 2, 3, 1, 2, 1), nrow = nRows, ncol = nCols)
    colnames(genotypes) <- paste0('mrk_', 1:3)
    rownames(genotypes) <- paste0('id_', 1:3)
    return(genotypes)
}

# Create example PED file
pedPath <- paste0(tmpPath, 'ped-', randomString(), '.txt')
nRows <- 3
nCols <- 3
phenotypes <- data.frame(FID = c('0', '0', '0'),
                         IID = c('id_1', 'id_2', 'id_3'),
                         PAT = c('NA', 'NA', 'NA'),
                         MAT = c('NA', 'NA', 'NA'),
                         SEX = c('NA', 'NA', 'NA'),
                         PHENOTYPE = c('NA', 'NA', 'NA'),
                         stringsAsFactors = FALSE)
phenotypes[] <- lapply(phenotypes, type.convert, as.is=TRUE)
rownames(phenotypes) <- paste0('id_', 1:3)
genotypes <- restoreGenotypes()
ped <- cbind(phenotypes, genotypes)
write.table(ped, file = pedPath, quote = FALSE, row.names = FALSE)


context("readPED")

test_that("it complains if folderOut already exists", {
    dirExistsPath <- paste0(tmpPath, "dirExists")
    dir.create(dirExistsPath, showWarnings = FALSE)
    expect_error(
        readPED(fileIn = pedPath, header = TRUE, dataType = 'integer',
                n = nRows, folderOut = dirExistsPath)
    )
})

test_that("it reads a PED file into a BGData object", {
    
    # With minimum number of parameters (with exception of folderOut)
    BGData <- readPED(fileIn = pedPath, header = TRUE, dataType = integer(),
                      folderOut = paste0(tmpPath, 'BGData-', randomString()))
    expect_true(all.equal(BGData@pheno, phenotypes))
    expect_true(all.equal(BGData@geno[], genotypes))
    
    # With n
    BGData <- readPED(fileIn = pedPath, header = TRUE,
                      dataType = integer(), n = nRows,
                      folderOut = paste0(tmpPath, 'BGData-', randomString()))
    expect_true(all.equal(BGData@pheno, phenotypes))
    expect_true(all.equal(BGData@geno[], genotypes))
    
    # With p
    BGData <- readPED(fileIn = pedPath, header = TRUE,
                      dataType = integer(), p = nCols,
                      folderOut = paste0(tmpPath, 'BGData-', randomString()))
    expect_true(all.equal(BGData@pheno, phenotypes))
    expect_true(all.equal(BGData@geno[], genotypes))
    
    # With both n and p
    BGData <- readPED(fileIn = pedPath, header = TRUE,
                      dataType = integer(), n = nRows, p = nCols,
                      folderOut = paste0(tmpPath, 'BGData-', randomString()))
    expect_true(all.equal(BGData@pheno, phenotypes))
    expect_true(all.equal(BGData@geno[], genotypes))
    
    # As integer
    class(genotypes) <- 'integer'
    BGData <- readPED(fileIn = pedPath, header = TRUE, dataType = integer(),
                      folderOut = paste0(tmpPath, 'BGData-', randomString()))
    expect_true(all.equal(BGData@geno[], genotypes))
    BGData <- readPED(fileIn = pedPath, header = TRUE, dataType = 'integer',
                      folderOut = paste0(tmpPath, 'BGData-', randomString()))
    expect_true(all.equal(BGData@geno[], genotypes))
    genotypes <- restoreGenotypes()
    
    # As double
    class(genotypes) <- 'double'
    BGData <- readPED(fileIn = pedPath, header = TRUE, dataType = double(),
                      folderOut = paste0(tmpPath, 'BGData-', randomString()))
    expect_true(all.equal(BGData@geno[], genotypes))
    BGData <- readPED(fileIn = pedPath, header = TRUE, dataType = 'double',
                      folderOut = paste0(tmpPath, 'BGData-', randomString()))
    expect_true(all.equal(BGData@geno[], genotypes))
    genotypes <- restoreGenotypes()
    
    # As character
    expect_error(readPED(fileIn = pedPath, header = TRUE, dataType = character(),
                         folderOut = paste0(tmpPath, 'BGData-', randomString())))
    expect_error(readPED(fileIn = pedPath, header = TRUE, dataType = 'character',
                         folderOut = paste0(tmpPath, 'BGData-', randomString())))
    
})


context("readPED.matrix")

test_that("it reads a PED file into a matrix object", {
    
    # With minimum number of parameters (with exception of folderOut)
    BGData <- readPED.matrix(fileIn = pedPath, header = TRUE,
                             dataType = integer())
    expect_true(all.equal(BGData@pheno, phenotypes))
    expect_true(all.equal(BGData@geno[], genotypes))
    
    # With n
    BGData <- readPED.matrix(fileIn = pedPath, header = TRUE,
                             dataType = integer(), n = nRows)
    expect_true(all.equal(BGData@pheno, phenotypes))
    expect_true(all.equal(BGData@geno[], genotypes))
    
    # With p
    BGData <- readPED.matrix(fileIn = pedPath, header = TRUE,
                             dataType = integer(), p = nCols)
    expect_true(all.equal(BGData@pheno, phenotypes))
    expect_true(all.equal(BGData@geno[], genotypes))
    
    # With both n and p
    BGData <- readPED.matrix(fileIn = pedPath, header = TRUE,
                             dataType = integer(), n = nRows, p = nCols)
    expect_true(all.equal(BGData@pheno, phenotypes))
    expect_true(all.equal(BGData@geno[], genotypes))
    
    # As integer
    class(genotypes) <- 'integer'
    BGData <- readPED.matrix(fileIn = pedPath, header = TRUE, dataType = integer())
    expect_true(all.equal(BGData@geno[], genotypes))
    BGData <- readPED.matrix(fileIn = pedPath, header = TRUE, dataType = 'integer')
    expect_true(all.equal(BGData@geno[], genotypes))
    genotypes <- restoreGenotypes()
    
    # As double
    class(genotypes) <- 'double'
    BGData <- readPED.matrix(fileIn = pedPath, header = TRUE, dataType = double())
    expect_true(all.equal(BGData@geno[], genotypes))
    BGData <- readPED.matrix(fileIn = pedPath, header = TRUE, dataType = 'double')
    expect_true(all.equal(BGData@geno[], genotypes))
    genotypes <- restoreGenotypes()
    
    # As character
    class(genotypes) <- 'character'
    BGData <- readPED.matrix(fileIn = pedPath, header = TRUE, dataType = character())
    expect_true(all.equal(BGData@geno[], genotypes))
    BGData <- readPED.matrix(fileIn = pedPath, header = TRUE, dataType = 'character')
    expect_true(all.equal(BGData@geno[], genotypes))
    genotypes <- restoreGenotypes()
    
})
