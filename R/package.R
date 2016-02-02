#' LinkedMatrix: Column-Linked and Row-Linked Matrices
#'
#' Matrices implemented as lists of matrix-like nodes, linked by columns or
#' rows.
#'
#' @docType package
#' @name LinkedMatrix-package
#' @importFrom methods setClass setClassUnion setMethod
#' @aliases NULL
NULL


.onAttach <- function(libname, pkgname) {
    packageStartupMessage("The LinkedMatrix package was supported by the National Institutes of Health (Grant: R01GM101219, R01GM099992).")
    packageStartupMessage()
}


release_questions <- function() {
    c("Have you updated the NEWS file?")
}
