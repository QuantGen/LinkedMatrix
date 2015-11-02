#' LinkedMatrix: Column-Linked and Row-Linked Matrices
#' 
#' Matrices implemented as collections of matrix-like nodes, linked by columns or rows.
#' 
#' @docType package
#' @name LinkedMatrix
#' @import methods
NULL


.onAttach <- function(libname, pkgname) {
    packageStartupMessage("The LinkedMatrix package was supported by the National Institutes of Health (Grant: R01GM101219, R01GM099992).")
}
