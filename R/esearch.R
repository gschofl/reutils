#' @include eutil.R
NULL


#' @export
.esearch <- setRefClass(
  Class="esearch",
  contains="eutil",
  methods=list(
    initialize=function(method, ...) {
      callSuper()
      perform_query(method=method, ...)
      if (no_errors()) {
        errors$check_errors(.self)
      }
    },
    show=function() {
      cat("Object of class", sQuote(eutil()), "\n")
      if (no_errors()) {
        methods::show(get_content("parsed"))
      } else {
        methods::show(get_error())
      }
    }
  )
)


parse_esearch <- function(.obj) {
  if (.obj$no_errors()) {
    x <- .obj$get_content("xml")
    structure(
      xvalue(x, '/eSearchResult/IdList/Id'),
      ## Attributes
      retmax=xvalue(x, '/eSearchResult/RetMax', as='numeric'),
      retstart=xvalue(x, '/eSearchResult/RetStart', as='numeric'),
      count=xvalue(x, '/eSearchResult/Count', as='numeric'),
      query_translation=xvalue(x, '/eSearchResult/QueryTranslation'),
      querykey=xvalue(x, '/eSearchResult/QueryKey', as='numeric'),
      webenv=xvalue(x, '/eSearchResult/WebEnv'),
      database=.obj$database(),
      class=c("entrez_uid", "character")
    )
  } else {
    structure(NA_character_, database=NA_character_, class=c("entrez_uid", "character"))
  }
}


#' Class \code{"entrez_uid"}
#'
#' A container for UIDs returned by a call to \code{\link{esearch}}.
#' It is essentially a character vector of UIDs supplemented with a number
#' of attributes:
#' \describe{
#'    \item{\code{retmax}:}{Total number of hits retrieved from the Entrez server.}
#'    \item{\code{retstart}:}{Index of the first hit retrieved from the Entrez server.}
#'    \item{\code{count}:}{Total number of hits for a search query.}
#'    \item{\code{query_translation}:}{Details of how Entrez translated the query.}
#'    \item{\code{querykey}:}{If \code{usehistory = TRUE}, the query key,
#'    otherwise \code{NA}.}
#'    \item{\code{webenv}:}{If \code{usehistory = TRUE}, the Web envronment string,
#'    otherwise \code{NA}.}
#'    \item{\code{database}:}{Name of the queried database.}
#' }
#' @keywords classes internal
#' @name entrez_uid-class
#' @examples
#' ###
setOldClass("entrez_uid")


#' @rdname database-methods
#' @export
setMethod("database", "entrez_uid", function(x, ...) attr(x, "database"))


#' @rdname uid-methods
#' @export
setMethod("uid", "entrez_uid", function(x, ...) {
  attributes(x) <- NULL
  x
})


#' @rdname webenv-methods
#' @export
setMethod("webenv", "entrez_uid", function(x, ...) attr(x, "webenv"))


#' @rdname querykey-methods
#' @export
setMethod("querykey", "entrez_uid", function(x, ...) attr(x, "querykey"))


#' @S3method print entrez_uid
print.entrez_uid <- function(x, ...) {
  db <- database(x)
  if (!is.na(webenv(x))) {
    row1 <- sprintf("Web Environment for the %s database.", sQuote(db))
    row2 <- sprintf("Number of UIDs stored on the History server: %s", attr(x, "count"))
    row3 <- sprintf("Query Key: %s\nWebEnv: %s\n", querykey(x), webenv(x))
    cat(row1, row2, row3, sep="\n")
  } else {
    cat(sprintf("List of UIDs from the %s database.\n", sQuote(db)))
    print(format(x))
  }
  invisible()
}


#' @S3method [ entrez_uid
"[.entrez_uid" <- function(x, i, j, ..., drop=TRUE) {
  out <- NextMethod(...)
  attributes(out) <- attributes(x)  
  out    
}


#' \code{esearch} performs searches using the the NCBI ESearch utility to retrieve
#' primary UIDs matching a text query. These UIDs can be used in subsequent calls
#' to \code{\link{esummary}}, \code{\link{efetch}}, or \code{\link{elink}}.
#' 
#' @details
#' See the official online documentation for NCBI's
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/\#chapter4.ESearch}{EUtilities}
#' for additional information on this EUtility.
#' 
#' @title esearch - searching an Entrez database
#' @param term A valid Entrez text query.
#' @param db Database to search (default: nuccore).
#' @param rettype Retrieval type. (default: 'uilist', alternative: 'count'.)
#' @param retstart Numeric index of the first UID in the
#' retrieved set to be shown in the XML output (default: 0).
#' @param retmax Total number of UIDs to be retrieved (default: 100).
#' @param usehistory If \code{TRUE}, search results are posted directly to
#' the Entrez History Server so that they can be used in subsequent 
#' calls to \code{\link{esummary}}, \code{\link{efetch}}, or
#' \code{\link{elink}}. Also, \code{usehistory} must be set to \code{TRUE}
#' for \code{esearch} to interpret query key values included in \code{term}
#' or to accept a \code{webenv} as input.
#' @param webenv Web environment string returned by a previous call to
#' \code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}}.
#' When provided, \code{esearch} will append the results of the search to
#' the pre-existing Web environment. Providing \code{webenv} also allows
#' query keys to be used in \code{term} so that previous search sets can be
#' combined or limited.
#' @param querykey query key returned by a previous call to
#' \code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}}.
#' When provided, \code{esearch} will find the intersection of the set
#' specified by \code{querykey} and the set retrieved by the query in \code{term}
#' (i.e. joins the two with AND).  
#' @param field Optional. Search field used to limit the entire search
#' term.
#' @param datetype Optional. Type of date to limit the search. One of "mdat"
#' (modification date), "pdat" (publication date) or "edat" (Entrez date)
#' @param reldate Optional. Number of days back for which search items are
#' returned.
#' @param mindate Optional. Minimum date of search range. Format
#' YYYY/MM/DD, YYYY/MM, or YYYY.
#' @param maxdate Optional. Maximum date of search range. Format
#' YYYY/MM/DD, YYYY/MM, or YYYY.
#' @return An \code{\linkS4class{esearch}} object.
#' @export
#' @seealso
#' Combine calls to ESearch with other EUtils:
#' \code{\link{esummary}}, \code{\link{efetch}}, \code{\link{elink}}.
#' @seealso
#' Accessor methods:
#' \code{\link{content}}, \code{\link{getUrl}}, \code{\link{getError}},
#' \code{\link{database}}, \code{\link{uid}},
#' \code{\link{webenv}}, \code{\link{querykey}}.
#' @examples
#' \dontrun{
#' ## Search PubMed for articles with the term "Chlamydia psittaci" in the
#' ## title that were published in 2013.
#' pmid <- esearch("Chlamydia psittaci[titl] and 2013[pdat]", "pubmed")
#' pmid
#'
#' ## Extract the query results either as an XML tree or parsed into
#' ## a character vector
#' xml <- content(pmid, "xml")
#' uids <- uid(pmid)
#' 
#' ## Alternatively post the UIDs to the History Server.
#' pmid <- esearch("Chlamydia psittaci[titl] and 2013[pdat]", "pubmed", usehistory=TRUE)
#' pmid
#' 
#' ## Associate new search results with the existing search results.
#' pmid2 <- esearch("Chlamydia psittaci[titl] and 2012[pdat]", "pubmed",
#'                  usehistory=TRUE, webenv=webenv(pmid))
#' pmid2
#' }
esearch <- function(term, db="nuccore", rettype="uilist",
                    retstart=0, retmax=100, usehistory=FALSE,
                    webenv=NULL, querykey=NULL, field=NULL,
                    datetype=NULL, reldate=NULL, mindate=NULL,
                    maxdate=NULL) {
  if (missing(term)) {
    stop("No query term provided", call.=FALSE)
  }
  if (!nzchar(db)) {
    stop("No database provided", call.=FALSE)
  }
  if (length(term) > 1L) {
    term <- paste(term, collapse=" OR ")
  }
  .esearch(method=if (nchar(term) < 100) "GET" else "POST",
           term=.escape(term), db=db, 
           usehistory=if (usehistory) "y" else NULL,
           WebEnv=webenv, query_key=querykey, retstart=retstart,
           retmax=if (usehistory) 0 else retmax, rettype=rettype,
           field=field, datetype=datetype, reldate=reldate,
           mindate=mindate, maxdate=maxdate)
}

#' ESearch Accessors
#' 
#' Extract UIDs from an \code{\link{esearch}} object.
#'
#' @param x An \code{\linkS4class{esearch}} object.
#' @param i Numeric indices.
#' @return A \code{\linkS4class{entrez_uid}} object.
#' @rdname esearch-methods
#' @export
#' @examples
#' \dontrun{
#' e <- esearch("Mus musculus", "protein", retmax=20)
#' e[1:5]
#' ## pass the subset directly on to esummary or efetch
#' content(esummary(e[1:5]), "parsed")
#' }
setMethod("[", c("esearch", "numeric"), function(x, i) {
  res <- content(x, "parsed")
  out <- res[i]
  attributes(out) <- attributes(res)  
  out    
})


#' @rdname uid-methods
#' @export
setMethod("uid", "esearch", function(x, ...) uid(x$get_content("parsed")))


#' @rdname webenv-methods
#' @export
setMethod("webenv", "esearch", function(x, ...) webenv(x$get_content("parsed")))


#' @rdname querykey-methods
#' @export
setMethod("querykey", "esearch", function(x, ...) querykey(x$get_content("parsed")))

