#' @include eutil.R
NULL


#' @export
.ecitmatch <- setRefClass(
  Class="ecitmatch",
  contains="eutil",
  methods=list(
    initialize = function(method, ...) {
      callSuper()
      perform_query(method=method, ...)
      if (errors$all_empty() && retmode() != "xml") {
        ## Switch off error checking even if retmode is set to "xml"
        ## because weirdly enough with ecitmatch retmode must be set to 'xml'
        ## but at least currently it doesn't return XML results but
        ## bar separated citation strings with the PMID as last entry.
        errors$check_errors(.self)
      }
    },
    show=function() {
      cat("Object of class", sQuote(eutil()), "\n")
      if (no_errors()) {
        cat(get_content("text"))
      } else {
        methods::show(get_error())
      }
    }
  )
)


#' @rdname content-methods
#' @aliases content,ecitmatch,ecitmatch-method
setMethod("content", "ecitmatch",
          function(x, as="text") {
            as <- match.arg(as, "text")
            callNextMethod(x=x, as=as)
          })


#' ecitmatch
#' 
#' \code{ecitmatch} serves as an API to the PubMed
#' \href{http://www.ncbi.nlm.nih.gov/pubmed/batchcitmatch}{batch citation matcher}.
#' It retrieves PubMed IDs (PMIDs) that correspond to a set of input citation strings.
#' 
#' @param bdata Citation strings. Each input citation must be represented
#' by a citation string in the following format:
#' \emph{journal_title|year|volume|first_page|author_name|your_key|}
#' @param db Database to search. The only supported value is \sQuote{pubmed}.
#' @param retmode Retrieval mode The only supported value is \sQuote{xml}.
#' @return An \code{\linkS4class{ecitmatch}} object.
#' @export
#' @examples
#' citstrings <- c("proc natl acad sci u s a|1991|88|3248|mann bj|Art1|",
#'                 "science|1987|235|182|palmenber ac|Art2|")
#' x <- ecitmatch(citstrings)
#' x
#' content(x)
ecitmatch <- function(bdata, db="pubmed", retmode="xml") {
  if (missing(bdata)) {
    stop("No citation string provided", call.=FALSE)
  }
  if (length(bdata) > 1) {
    bdata <- paste0(bdata, collapse='\r')
  }
  db <- match.arg(db, "pubmed")
  retmode <- match.arg(retmode, "xml")
  .ecitmatch('GET', db=db, retmode=retmode, bdata=bdata)
}