#' @section Package options:
#'
#' \emph{reutils} uses three \code{\link{options}} to configure behaviour:
#'
#' \itemize{
#'   \item \code{reutils.email}: NCBI requires that a user of their API provides an
#'      email address with a call to Entrez. If you are going to perform a lot
#'      of queries you should set \code{reutils.email} to your email address in
#'      your .Rprofile file. \emph{reutils} will annoy you with warnings until
#'      you comply.
#'      
#'   \item \code{reutils.show.headlines}: By default \code{\linkS4class{efetch}}
#'      objects containing text data show only the first 12 lines. This is quite handy
#'      if you have downloaded a fairly large genome in Genbank file format. This
#'      can be changed by setting the global option \code{reutils.show.headlines} to
#'      another numeric value or \code{NULL}.
#'
#'   \item \code{reutils.verbose.queries}: If you perform many queries interactively
#'      you might want to get messages announcing  the queris you run. You can do so by setting
#'      the option \code{reutils.verbose.queries} to \code{TRUE}.
#'
#' }
#' 
#' @docType package
#' @name reutils
NULL

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.reutils <- list(
    reutils.email = "Your.Name.Here@example.org",
    reutils.show.headlines = 12,
    reutils.verbose.queries = FALSE,
    reutils.test.remote = FALSE
  )
  toset <- !(names(op.reutils) %in% names(op))
  if (any(toset)) {
    options(op.reutils[toset])
  }
  invisible()
}
