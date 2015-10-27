.onLoad <- function(libname, pkgname) {
  op <- options()
  op.reutils <- list(
    reutils.email = "gerhard.schofl@gmail.com",
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
