# Create various matrix-like objects that correspond in dimensions
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

# Create a ColumnLinkedMatrix object
cm1 <- ColumnLinkedMatrix(m1, m2, m3)

# Create a RowLinkedMatrix object
rm1 <- RowLinkedMatrix(m1, m2, m3)

# Alternatively, a LinkedMatrix object can also be created using the `new`
# function
cm2 <- new("ColumnLinkedMatrix", m1, m2, m3)

# To specify the matrix-like objects as a list, use the `do.call` function
rm2 <- do.call("RowLinkedMatrix", list(m1, m2, m3))
