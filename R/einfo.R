#' @include eutil.R
NULL

#' @export
.einfo <- setRefClass(
  Class="einfo",
  contains="eutil", 
  methods=list(
    initialize=function(method="GET", ...) {
      callSuper()
      perform_query(method=method, ...)
      if (no_errors()) {
        errors$check_errors(.self)
      }
    },
    show=function() {
      cat("Object of class", sQuote(eutil()), "\n")
      if (no_errors()) {
        if (is.null(database())) {
          cat("List of Entrez databases\n")
          methods::show(xmlValue('/eInfoResult/DbList/DbName'))
        } else {
          cat(sprintf("Overview over the Entrez database %s.\n",
                      sQuote(xmlValue('/eInfoResult/DbInfo/MenuName'))))
          x <- get_content("parsed")
          nm <- names(x)
          fnm <- ellipsize(paste(names(x$Fields), collapse="; "), offset=10)
          lnm <- ellipsize(paste(names(x$Links), collapse="; "), offset=9)
          showme <- sprintf("  %s: %s", nm, 
                            c(vapply(x[1:6], as.character, ""),
                              fnm, lnm))
          cat(showme, sep='\n')
        }
      } else {
        methods::show(get_error())
      }
    }
  )
)


parse_einfo <- function(.obj) {
  x <- .obj$get_content("xml")
  if (is.null(.obj$database())) {
    xvalue(x, '/eInfoResult/DbList/DbName')
  } else {
    list(
      dbName=xvalue(x, '/eInfoResult/DbInfo/DbName'),
      MenuName=xvalue(x, '/eInfoResult/DbInfo/MenuName'),
      Description=xvalue(x, '/eInfoResult/DbInfo/Description'),
      DbBuild=xvalue(x, '/eInfoResult/DbInfo/DbBuild'),
      Count=xvalue(x, '/eInfoResult/DbInfo/Count', 'integer'),
      LastUpdate=xvalue(x, '/eInfoResult/DbInfo/LastUpdate', 'POSIXlt'),
      Fields=extract_df(x, '/eInfoResult/DbInfo/FieldList/Field/'),
      Links=extract_df(x, '/eInfoResult/DbInfo/LinkList/Link/')
    )
  }
}


extract_df <- function(x, path) {
  nm <- unique(xname(x, paste0(path, 'child::node()')))        
  if (!all(is.na(nm))) {
    nodes <- xset(x, paste0(path, '*'))
    list <- split(vapply(nodes, xmlValue, ""), nm)
    data.frame(stringsAsFactors=FALSE, list)[, nm]
  } else {
    data.frame()
  }
}

#' \code{einfo} performs calls to the NCBI EInfo utility to retrieve the names
#' of all valid Entrez databases, or, if \code{db} is provided,
#' to retrieve statistics for a single database, including lists of indexing
#' fields and available link names. 
#' 
#' @details
#' See the official online documentation for NCBI's
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/\#chapter4.EInfo}{EUtilities}
#' for additional information.
#' 
#' @title einfo - getting database statistics and search fields
#' @param db A valid NCBI database name. If \code{NULL}, a list of all current NCBI
#' databases is returned.
#' @return A \code{\linkS4class{einfo}} object.
#' @seealso
#' \code{\link{content}}, \code{\link{getUrl}}, \code{\link{getError}}.
#' @export
#' @examples
#' ## Return a list of all current Entrez database names
#' einfo()
#' 
#' \dontrun{
#' ## Return statistics for the Entrez Gene database and parse
#' ## the returned data into a data.frame
#' x <- einfo("gene")
#' content(x, "parsed")
#' }
einfo <- function(db=NULL) {
  assert_that(is.null(db) || is.string(db))
  .einfo(method="GET", db=db)
}

#' EInfo accessors
#' 
#' Extract parts of a parsed \code{\linkS4class{einfo}} object.
#' 
#' @param x An \code{\linkS4class{einfo}} object.
#' @param i Numeric or character indices specifying the elements to extract.
#' @return A list.
#' @seealso 
#'    \code{\link[base]{Extract}}
#' @rdname einfo-methods
#' @export
#' @examples
#' \dontrun{
#' e <- einfo("pubmed")
#' e[1:5]
#' e["Description"]
#' e[["Links"]]
#' }
setMethod("[", "einfo", function(x, i) {
  content(x, "parsed")[i]
})

#' @rdname einfo-methods
#' @export
setMethod("[[", "einfo", function(x, i) {
  content(x, "parsed")[[i]]
})
