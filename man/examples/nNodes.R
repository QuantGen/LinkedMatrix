# Create an example RowLinkedMatrix from various matrix-like objects that
# correspond in dimensions
if (require("ff")) {
    m1 <- ff::ff(initdata = rnorm(50), dim = c(5, 10))
} else {
    m1 <- matrix(data = rnorm(50), nrow = 5, ncol = 10)
}

if (require("bigmemory")) {
    m2 <- bigmemory::big.matrix(init = rnorm(50), nrow = 5, ncol = 10)
} else {
    m2 <- matrix(data = rnorm(50), nrow = 5, ncol = 10)
}

m3 <- matrix(data = rnorm(50), nrow = 5, ncol = 10)

m <- RowLinkedMatrix(
    ff::ff(initdata = rnorm(50), dim = c(5, 10)),
    bigmemory::big.matrix(init = rnorm(50), nrow = 5, ncol = 10),
    matrix(data = rnorm(50), nrow = 5, ncol = 10)
)

# Get the number of nodes of the RowLinkedMatrix
nNodes(m)
