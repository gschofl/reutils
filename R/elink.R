#' @include eutil.R
NULL


#' @export
.elink <- setRefClass(
  Class="elink",
  contains="eutil",
  methods=list(
    initialize=function(method, ...) {
      callSuper()
      perform_query(method=method, ...)
      if (errors$all_empty()) {
        errors$check_errors(.self)
      }
    },
    show=function() {
      cat("Object of class", sQuote(eutil()), "\n")
      if (no_errors()) {
        methods::show(get_content("xml"))
        tail <- sprintf("ELink query from database %s to destinaton DB %s.",
                        sQuote(params$dbFrom), sQuote(database()))
        cat(tail, sep="\n")
      } else {
        methods::show(get_error())
      }
    }
  )
)

#' elink
#'
#' \code{elink} generates a list of UIDs in a specified Entrez database that
#' are linked to a set of input UIDs in either the same or another
#' database. For instance, the ELink utility can find Entrez gene records
#' linked to records in Entrez Protein.
#' 
#' @details
#' See the official online documentation for NCBI's
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ELink}{EUtilities}
#' for additional information.
#' 
#' If \code{dbTo} and \code{dbFrom} are set to the same database, ELink will
#' return neighbors within that database.
#' 
#' Elink commands (cmd) specify the function that elink will perform.
#' Available commands are:
#' \itemize{
#'   \item{"\strong{neighbor}" }{(Default) ELink returns a set of UIDs in dbTo
#'   linked to the input UIDs in dbFrom.}
#'   \item{"\strong{neighbor_score}" }{ELink returns a set of UIDs within the
#'   same database as the input UIDs along with similarity scores.}
#'   \item{"\strong{neighbor_history}" }{ELink posts the output UIDs to the
#'   Entrez History server and returns a query_key and WebEnv parameter.
#'   Alternatively this is achieved by setting \code{usehistory=TRUE}}
#'   \item{"\strong{acheck}" }{ELink lists all links available for a set of UIDs.}
#'   \item{"\strong{ncheck}" }{ELink checks for the existence of links
#'   \emph{within the same database} for a set of UIDs.}
#'   \item{"\strong{lcheck}" }{Elink checks for the existence of external links
#'   (LinkOuts) for a set of UIDs.}
#'   \item{"\strong{llinks}" }{For each input UID, ELink lists the URLs and
#'   attributes for the LinkOut providers that are not libraries.}
#'   \item{"\strong{llinkslib}" }{For each input UID, ELink lists the URLs and
#'   attributes for all LinkOut providers including libraries.}
#'   \item{"\strong{prlinks}" }{ELink lists the primary LinkOut provider for
#'   each input UID.}
#' }
#' 
#' @param uid (Required) A character vector of UIDs.
#' @param dbFrom Initial database containing the UIDs in the input list.
#' @param dbTo Destination database from which to retrieve linked UIDs. If
#' not provided links will be sought in the database containing the input UIDs.
#' @param linkname Name of the Entrez link to retrieve. Every link in
#' Entrez is given a name of the form 'dbFrom_dbTo_subset'.
#' @param usehistory If \code{TRUE} search results are stored directly in
#' the user's Web environment so that they can be used in subsequents 
#' calls to \code{\link{esummary}} or \code{\link{efetch}}.
#' @param cmd ELink command mode (default: 'neighbor'). See Details.
#' @param correspondence if \code{TRUE} correspondence between query UIDs and
#' destination UIDs is preserved.
#' @param querykey Query key.
#' @param webenv Web Environment.
#' @param term Search query to limit the output set of linked UIDs.
#' @param holding Name of LinkOut provider.
#' @param datetype Type of date to limit the search. One of 'mdat'
#' (modification date), 'pdat' (publication date) or 'edat' (Entrez date).
#' @param reldate umber of days back for which search items are
#' returned.
#' @param mindate Minimum date of search range. Format YYYY/MM/DD.
#' @param maxdate Maximum date of search range. Format YYYY/MM/DD.
#' @return An \code{\linkS4class{elink}} object.
#' @export
#' @examples
#' ###
elink <- function(uid, dbFrom=NULL, dbTo=NULL, linkname=NULL,
                  usehistory=FALSE, cmd="neighbor",
                  correspondence=FALSE, querykey=NULL, webenv=NULL,
                  term=NULL, holding=NULL, datetype=NULL,
                  reldate=NULL, mindate=NULL, maxdate=NULL) {
  ## extract query parameters
  params <- parse_params(uid, dbFrom, querykey, webenv)
  
  ## set dbTo=dbFrom if no dbTo is provided
  if (is.null(dbTo) && !grepl(pattern="check$|links", cmd)) {
    dbTo <- params$db
  }
  if (usehistory) {
    cmd <- "neighbor_history"
  }
  if  (correspondence && !is.null(params$uid)) {
    uid  <- paste0(params$uid, collapse="&id=")
  } else {
    uid <- .collapse(params$uid)
  }
  .elink(method=if (length(params$uid) < 100) "GET" else "POST",
         id=uid, db=dbTo, dbFrom=params$db, cmd=cmd, query_key=params$querykey,
         WebEnv=params$webenv, linkname=linkname, term=term, holding=holding,
         datetype=datetype, reldate=reldate, mindate=mindate, maxdate=maxdate)
}

