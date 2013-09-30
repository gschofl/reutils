#' @section Package options:
#'
#' \emph{reutils} uses two \code{\link{options}} to configure behaviour:
#'
#' \itemize{
#'   \item \code{reutils.mail}: NCBI requires that a user of their API provides an
#'      email address with a call to Entrez. If you are going to perform a lot
#'      of queries you should set \code{reutils.mail} to your email address. \emph{reutils}
#'      will annoy you with warnings untill you comply.
#'
#'   \item \code{reutils.verbose.queries}: If you perform many queries non-interactively
#'      you might want to switch off the messages announcing each query. You can do so by setting
#'      the option \code{reutils.verbose.queries} to \code{FALSE}.
#'
#' }
#' 
#' @docType package
#' @name reutils
NULL

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.reutils <- list(
    reutils.mail = "gschofl@yahoo.de",
#     reutils.mail = "Your.Name.Here@example.org",
    reutils.verbose.queries = TRUE
  )
  toset <- !(names(op.reutils) %in% names(op))
  if (any(toset)) {
    options(op.reutils[toset])
  }
  invisible()
}
