#' @include utils.R
NULL


#' Generator object for the \code{\linkS4class{eutil_error}} class
#'
#' @usage eutil_error(...)
#' @section Methods:
#' \describe{
#'    \item{\code{#all_empty()}:}{Are all error fields \code{NULL}}
#'    \item{\code{#check_errors(.Object, verbose = TRUE)}:}{check a \code{linkS4class{eutil}} object
#'    for errors}
#' }
#' @keywords classes
#' @export
eutil_error <- setRefClass(
  Class="eutil_error",
  fields=c("error", "errmsg", "wrnmsg"),
  methods=list(
    initialize=function() {
      .self$error <- NULL
      .self$errmsg <- NULL
      .self$wrnmsg <- NULL
    },
    all_empty=function() {
      is.null(error) && is.null(errmsg) && is.null(wrnmsg)
    },
    check_errors=function(.Object, verbose=TRUE) {
      assert_that(is(.Object, "eutil"))
      x <- .Object$get_content("xml")
      .self$error <- xvalue(x, '//ERROR', default=NULL)
      if (verbose && !is.null(error)) {
        message('Error:\n\t', error)
      }
      errmsg_name  <- xname(x, '//ErrorList/*', default=NULL)
      .self$errmsg <- setNames(xvalue(x, '//ErrorList/*', default=NULL), errmsg_name)
      if (verbose && !is.null(errmsg)) {
        message('Error(s):\n\t', 
                paste(paste(names(errmsg), errmsg, sep="\t"), collapse="\n\t"))
      }
      wrnmsg_name  <- xname(x, '//WarningList/*', default=NULL)
      .self$wrnmsg <- setNames(xvalue(x, '//WarningList/*', default=NULL), wrnmsg_name) 
      if (verbose && !is.null(wrnmsg)) {
        message('Warning(s):\n\t', 
                paste(paste(names(wrnmsg), wrnmsg, sep="\t"), collapse="\n\t"))
      }
    },
    show=function() {
      if (all_empty()) {
        cat("No errors", sep="\n")
      } else {
        error  %&&% methods::show(error)
        errmsg %&&% methods::show(errmsg)
        wrnmsg %&&% methods::show(wrnmsg)
      }
    }
  )
)

#' Class \code{"eutil_error"}
#'
#' A container for handling errors when trying to parse XML files returned
#' by Entrez.
#' @name eutil_error-class
#' @section Fields:
#' \describe{
#'    \item{\code{error}:}{}
#'    \item{\code{errmsg}:}{}
#'    \item{\code{wrnmsg}:}{}
#' }
#' @section Extends: All reference classes extend and inherit methods from
#'     \code{"\linkS4class{envRefClass}"}.
#' @seealso \code{\link{eutil_error}}, \code{\linkS4class{eutil}}.
#' @keywords classes
#' @examples
#' ###
NULL

