#' @include eutil-error.R
#' @importFrom RCurl basicHeaderGatherer
#' @importFrom RCurl basicTextGatherer
#' @importFrom RCurl postForm
#' @importFrom RCurl getURLContent
#' @importFrom RCurl curlEscape
NULL


#' Generator object for the \code{\linkS4class{eutil}} classes
#'
#' The generator object for \code{\linkS4class{eutil}}, \code{\linkS4class{einfo}},
#' \code{\linkS4class{esearch}}, \code{\linkS4class{esummary}}, \code{\linkS4class{efetch}},
#' \code{\linkS4class{elink}}, \code{\linkS4class{epost}}, \code{\linkS4class{egquery}},
#' \code{\linkS4class{espell}}, and \code{\linkS4class{ecitmatch}} reference classes.
#' This object implements the low level request generator for interaction with the NCBI services.
#' it should not be used direcly, but initialized through the respective constructor
#' functions \code{\link{einfo}}, \code{\link{esearch}}, \code{\link{esummary}},
#' \code{\link{efetch}}, \code{\link{elink}}, \code{\link{epost}}, \code{\link{egquery}},
#' \code{\link{espell}}, and \code{\link{ecitmatch}}.
#' 
#' @aliases eutil, einfo, esearch, esummary, efetch, elink, epost, egquery, espell, ecitmatch
#' @section Public methods:
#' \describe{
#'   \item{\code{#xmlValue(xpath, as="character", default=NA_character_)}:}{
#'   Return the text value of a leaf node in the XML data given a valid XPath
#'   expression.}
#'   \item{\code{#xmlAttr(xpath, name, as="character", default=NA_character_)}:}{
#'   Return the value of an XML attribute given a valid XPath expression and an
#'   attribute name.}
#'   \item{\code{#xmlName(xpath, as="character", default=NA_character_)}:}{
#'   Return the tag names of XML nodes given a valid XPath expression.} 
#'   \item{\code{#xmlSet(xpath)}:}{
#'   Return a set of XML nodes given a valid XPath expression.}
#' }
#' 
#' @section Internal methods:
#' \describe{  
#'   \item{\code{#get_url()}:}{Return the URL used for an Entrez query; should not
#'   be used directly, use \code{\link{getUrl}} instead.}
#'   \item{\code{#get_error()}:}{Return \code{\linkS4class{eutil_error}}s; should not
#'   be used directly, use \code{\link{getError}} instead.}
#'   \item{\code{#get_content(as='text')}:}{Return the results of an Entrez query as
#'   text, xml, or a parsed R object; should not be used directly,
#'   use \code{\link{content}} instead.}
#'   \item{\code{#perform_query(method="GET", ...)}:}{Perform an Entrez query using
#'   either http GET or POST requests; should not be used directly.}
#' }
#' 
#' @seealso \code{\linkS4class{eutil}}, \code{\link{einfo}},
#' \code{\link{esearch}}, \code{\link{esummary}}, \code{\link{efetch}},
#' \code{\link{elink}}, \code{\link{epost}}, \code{\link{egquery}},
#' \code{\link{espell}}, and \code{\link{ecitmatch}}.
#' @keywords classes internal
#' @export
eutil <- setRefClass(
    Class="eutil",
    fields=list(params="list", errors="eutil_error", content="character"),
    methods=list(
      initialize=function() {
        .self$params  <- list()
        .self$errors  <- eutil_error()
        .self$content <- NA_character_
      },
      ##
      ## public methods
      ##
      xmlValue=function(xpath, as="character", default=NA_character_) {
        'Extract the text from XML leaf nodes given a valid XPath expression.'
        xvalue(get_content("xml"), xpath, as, default)
      },
      xmlName=function(xpath, as="character", default=NA_character_) {
        'Extract the tag names from XML nodes given a valid XPath expression.'
        xname(get_content("xml"), xpath, as, default)
      },
      xmlAttr=function(xpath, name, as="character", default=NA_character_) {
        'Extract the value of an XML attribute given a valid XPath expression
        and an attribute name'
        xattr(get_content("xml"), xpath, name, as, default)
      },
      xmlSet=function(xpath, ...) {
        'Extract a set of XML nodes given a valid XPath expression.'
        xset(get_content("xml"), xpath, ...)
      },
      ##
      ## internal methods
      ##
      get_url=function() {
        'return the URL used to perform a EUtils query'
        return(query_url('GET'))
      },
      get_error=function() {
        'return errors'
        return(.self$errors)
      },
      get_content=function(as="text") {
        'return the contents of a query as text, xml, or a native R object'
        as <- match.arg(as, c("text", "xml", "parsed"))
        switch(as,
          text=.self$content,
          xml=savely_parse_xml(.self$content),
          parsed=parse_content(.self)
        )
      },
      perform_query=function(method="GET", ...) {
        verbose <- isTRUE(getOption("reutils.verbose.queries"))
        if (verbose) {
          cat("Perfoming an", sQuote(eutil()), "query ...\n")
        }
        method <- match.arg(method, c("GET", "POST"))
        
        ## update an object with new query parameters
        .email <- getOption("reutils.email")
        if (is.null(.email) || grepl("^Your\\.name\\.here.+", .email, ignore.case=TRUE)) {
          warning("NCBI requests that you provide an email address with each query to their API.\n",
                  " Set the global option ", sQuote("reutils.email"), " to your address to make",
                  " this message go away.", call.=FALSE, immediate.=FALSE)
        }
        .params <- list(...)
        .params <- compact(Reduce(merge_list, list(.params, params,  list(email=.email, tool="reutils"))))
        .self$params <- .params
        
        opts <- list()
        hg <- basicHeaderGatherer()
        opts$headerfunction <- hg$update
        tg <- basicTextGatherer()
        opts$writefunction <- tg$update
        
        if (method == "POST") {
          e <- tryCatch(postForm(query_url("POST"), .params=.self$params, .opts=opts),
                        error=function(e) e$message)
        }
        else if (method == "GET") {
          if (verbose) {
            cat(ellipsize(query_url("GET")), "\n")
          }
          e <- tryCatch(getURLContent(query_url("GET"), .opts=opts),
                        error=function(e) e$message)
        }
        .self$content <- as.character(tg$value())
        if (is.null(e) || !nzchar(e)) {
          header <- as.list(hg$value())
          status <- as.numeric(header$status)
          statusmsg <- header$statusMessage
          if (status != 200) {
            .self$errors$error <- paste0("HTTP error: Status ", status, "; ", statusmsg)
            warning(errors$error, call.=FALSE, immediate.=TRUE)
          }
        } else {
          .self$errors$error <- paste0("CurlError: ", e)
          warning(errors$error, call.=FALSE, immediate.=TRUE)
        }
      },
      ##
      ## helpers methods
      ##
      query_url=function(method) {
        host <- switch(eutil(),
                       egquery="http://eutils.ncbi.nlm.nih.gov/gquery",
                       ecitmatch="http://eutils.ncbi.nlm.nih.gov/entrez/eutils/ecitmatch.cgi",
                       paste0('http://eutils.ncbi.nlm.nih.gov/entrez/eutils/', eutil(),'.fcgi')
        )
        if (method == "GET") {
          fields <- paste(curlEscape(names(.self$params)), curlEscape(.self$params),
                          sep="=", collapse="&")
          paste0(host, "?", fields)
        } else {
          host
        }
      },
      eutil=function() {
        class(.self)
      },
      database=function() {
        .self$params$db
      },
      rettype=function() {
        .self$params$rettype
      },
      retmode=function() {
        .self$params$retmode
      },
      no_errors=function() {
        .self$errors$all_empty()
      }
    )
  )
 

#' Reference classes that capture the response from EUtils requests of
#' the same name.
#' 
#' @title \code{"eutil"} classes
#' @name eutil-class
#' @aliases eutil-class einfo-class esearch-class esummary-class efetch-class
#'          elink-class epost-class egquery-class espell-class ecitmatch-class
#' @section Fields:
#' \describe{
#'    \item{\code{params}:}{A named \code{list} of query parameters.}
#'    \item{\code{errors}:}{A \code{\linkS4class{eutil_error}} object.}
#'    \item{\code{content}:}{Results of an Entrez request as a character vector.}
#' }
#' @section Extends: All reference classes extend and inherit methods from
#'     \code{"\linkS4class{envRefClass}"}. Furthermore, \code{"einfo"},
#'     \code{"esearch"}, \code{"esummary"}, \code{"efetch"}, \code{"elink"},
#'     \code{"epost"}, \code{"egquery"}, \code{"espell"}, and \code{"ecitmatch"}
#'     all extend the \code{"eutil"} class.
#' @seealso \code{\link{eutil}}, \code{\link{einfo}}, \code{\link{esearch}},
#' \code{\link{esummary}}, \code{\link{efetch}}, \code{\link{elink}},
#' \code{\link{epost}}, \code{\link{egquery}}, \code{\link{espell}},
#' \code{\link{ecitmatch}}.
#' @keywords classes
#' @examples
#' showClass("eutil")
NULL


#' @importFrom XML xmlTreeParse
#' @importFrom XML xmlParseString
savely_parse_xml <- function(x, ...) {
  tryCatch(xmlTreeParse(x, asText=TRUE, useInternalNodes=TRUE,
                        error=NULL, ...),
           "XMLError"=function(e) {
             errmsg <- paste("XML parse error:", e$message)
             xmlParseString(paste0("<ERROR>", errmsg, "</ERROR>"))
           },
           "error"=function(e) {
             errmsg <- paste("Simple error:", e$message)
             xmlParseString(paste0("<ERROR>", errmsg, "</ERROR>"))
           })
}


parse_content <- function(.object) {
  switch(.object$eutil(),
    einfo=parse_einfo(.object),
    esearch=parse_esearch(.object),
    epost=parse_epost(.object),
    esummary=parse_esummary(.object),
    elink=parse_linkset(.object),
    "Not yet implemented"
  )
}


#' Extract the data content from an Entrez request
#' 
#' There are three ways to access data returned by an Entrez request: as a character string
#' \code{(as = "text")}, as a parsed XML tree \code{(as = "xml")}, or, if supported,
#' parsed into some native R object, e.g. a \code{list} or a \code{data.frame}
#' \code{(as = "parsed")}.
#' 
#' @title Extract the data content from an Entrez request
#' @usage content(x, as = "xml")
#' @param x An \code{\linkS4class{eutil}} object.
#' @param as Type of output: \code{\dQuote{xml}}, \code{\dQuote{text}}, or
#' \code{\dQuote{parsed}}.
#' @seealso
#' \code{\link{einfo}}, \code{\link{esearch}}, \code{\link{esummary}},
#' \code{\link{efetch}}, \code{\link{elink}}, \code{\link{epost}},
#' \code{\link{egquery}}, \code{\link{espell}}, \code{\link{ecitmatch}}.
#' @export
#' @docType methods
#' @rdname content-methods
#' @examples
#' e <- einfo()
#' 
#' ## return XML as an 'XMLInternalDocument'.
#' content(e, "xml")
#' 
#' ## return XML as character string.
#' cat(content(e, "text"))
#' 
#' ## return DbNames parsed into a character vector.
#' content(e, "parsed")
setGeneric("content", function(x, ...) standardGeneric("content"))

#' @rdname content-methods
#' @aliases content,eutil,eutil-method
setMethod("content", "eutil", function(x, as="xml") {
  x$get_content(as)
})


#' Retrieve a http or XML parsing error from an \code{\linkS4class{eutil}} object.
#' 
#' @title getError
#' @usage getError(x, ...)
#' @param x An \code{\linkS4class{eutil}} object.
#' @param ... Further arguments passed on to methods.
#' @seealso
#' \code{\link{einfo}}, \code{\link{esearch}}, \code{\link{esummary}},
#' \code{\link{efetch}}, \code{\link{elink}}, \code{\link{epost}},
#' \code{\link{egquery}}, \code{\link{espell}}, \code{\link{ecitmatch}}.
#' @export
#' @docType methods
#' @rdname getError-methods
#' @examples
#' e <- efetch("Nonsensical_accession_nr", "protein", rettype="fasta")
#' getError(e)
setGeneric("getError", function(x, ...) standardGeneric("getError"))

#' @rdname getError-methods
#' @aliases getError,eutil,eutil-method
setMethod("getError", "eutil", function(x) {
  x$get_error()
})


#' Retrieve the URL used to perform an Entrez E-Utilities query.
#' 
#' @title getUrl
#' @usage getUrl(x, ...)
#' @param x An \code{\linkS4class{eutil}} object.
#' @param ... Further arguments passed on to methods.
#' @seealso
#' \code{\link{einfo}}, \code{\link{esearch}}, \code{\link{esummary}},
#' \code{\link{efetch}}, \code{\link{elink}}, \code{\link{epost}},
#' \code{\link{egquery}}, \code{\link{espell}}, \code{\link{ecitmatch}}.
#' @export
#' @docType methods
#' @rdname getUrl-methods
#' @examples
#' e <- efetch("AV333213.1", "protein", rettype="fasta")
#' getUrl(e)
setGeneric("getUrl", function(x, ...) standardGeneric("getUrl"))

#' @rdname getUrl-methods
#' @aliases getUrl,eutil,eutil-method
setMethod("getUrl", "eutil", function(x) {
  x$get_url()
})


#' @keywords internal
#' @export
setGeneric("performQuery", function(x, method="GET", ...) standardGeneric("performQuery"))
setMethod("performQuery", "eutil", function(x, method="GET", ...) {
  method <- match.arg(method, c("GET", "POST"))
  x$perform_query(method=method, ...)
  return(invisible(x))
})


#' database
#' 
#' Target database of an \code{eutil} object.
#' 
#' @usage database(x, ...)
#' @param x An \code{\linkS4class{eutil}} object.
#' @param ... Further arguments passed on to methods.
#' @seealso
#' \code{\link{einfo}}, \code{\link{esearch}}, \code{\link{esummary}},
#' \code{\link{efetch}}, \code{\link{elink}}, \code{\link{epost}},
#' \code{\link{egquery}}, \code{\link{espell}}, \code{\link{ecitmatch}}.
#' @export
#' @docType methods
#' @rdname database-methods
#' @examples
#' e <- esearch("Mus musculus", "taxonomy")
#' database(e)
setGeneric("database", function(x, ...) standardGeneric("database"))

#' @rdname database-methods
#' @aliases database,eutil,eutil-method
setMethod("database", "eutil", function(x) x$database())


#' retmode
#' 
#' Retrieval mode of an \code{eutil} object. One of \code{xml} \code{text},
#' \code{asn.1} or \code{NULL} if not supported by an E-Utility.
#' 
#' @usage retmode(x, ...)
#' @param x An \code{\linkS4class{eutil}} object.
#' @param ... Further arguments passed on to methods.
#' @seealso
#' \code{\link{einfo}}, \code{\link{esearch}}, \code{\link{esummary}},
#' \code{\link{efetch}}, \code{\link{elink}}, \code{\link{epost}},
#' \code{\link{egquery}}, \code{\link{espell}}, \code{\link{ecitmatch}}.
#' @export
#' @docType methods
#' @rdname retmode-methods
#' @examples
#' e <- efetch("10090", "taxonomy")
#' retmode(e)
setGeneric("retmode", function(x, ...) standardGeneric("retmode"))

#' @rdname retmode-methods
#' @aliases retmode,eutil,eutil-method
setMethod("retmode", "eutil", function(x) x$retmode())

#' rettype
#' 
#' Retrieval type of an \code{eutil} object. See 
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
#' for the available retrieval types for different NCBI databases.
#' 
#' @usage rettype(x, ...)
#' @param x An \code{\linkS4class{eutil}} object.
#' @param ... Further arguments passed on to methods.
#' @seealso
#' \code{\link{einfo}}, \code{\link{esearch}}, \code{\link{esummary}},
#' \code{\link{efetch}}, \code{\link{elink}}, \code{\link{epost}},
#' \code{\link{egquery}}, \code{\link{espell}}, \code{\link{ecitmatch}}.
#' @export
#' @docType methods
#' @rdname rettype-methods
#' @examples
#' e <- esearch("Mus musculus", "taxonomy")
#' rettype(e)
setGeneric("rettype", function(x, ...) standardGeneric("rettype"))

#' @rdname rettype-methods
#' @aliases rettype,eutil,eutil-method
setMethod("rettype", "eutil", function(x) x$rettype())


#' Retrieve the list of UIDs returned by a call to ESearch or ELink.
#' 
#' @title uid
#' @usage uid(x, ...)
#' @param x An \code{\linkS4class{eutil}} object.
#' @param ... Further arguments passed on to methods.
#' @seealso
#' \code{\link{esearch}}, \code{\link{elink}}.
#' @export
#' @docType methods
#' @rdname uid-methods
#' @examples
#' e <- esearch("Mus musculus", "taxonomy")
#' uid(e)
setGeneric("uid", function(x, ...) standardGeneric("uid"))


#' The Web environment string returned from an ESearch, EPost or ELink call.
#' \code{NA} if the History server was not used.
#' 
#' @title webenv
#' @usage webenv(x, ...)
#' @param x An \code{\linkS4class{eutil}} object.
#' @param ... Further arguments passed on to methods.
#' @seealso
#' \code{\link{einfo}}, \code{\link{esearch}}, \code{\link{esummary}},
#' \code{\link{efetch}}, \code{\link{elink}}, \code{\link{epost}},
#' \code{\link{egquery}}, \code{\link{espell}}, \code{\link{ecitmatch}}.
#' @export
#' @docType methods
#' @rdname webenv-methods
#' @examples
#' e <- esearch("Mus musculus", "taxonomy", usehistory=TRUE)
#' webenv(e)
setGeneric("webenv", function(x, ...) standardGeneric("webenv"))


#' An integer query key returned by an ESearch, EPost or ELink call if
#' the History server was used. Otherwise \code{NA}.
#' 
#' @title querykey
#' @usage querykey(x, ...)
#' @param x An \code{\linkS4class{eutil}} object.
#' @param ... Further arguments passed on to methods.
#' @seealso
#' \code{\link{einfo}}, \code{\link{esearch}}, \code{\link{esummary}},
#' \code{\link{efetch}}, \code{\link{elink}}, \code{\link{epost}},
#' \code{\link{egquery}}, \code{\link{espell}}, \code{\link{ecitmatch}}.
#' @export
#' @docType methods
#' @rdname querykey-methods
#' @examples
#' e <- esearch("Mus musculus", "taxonomy", usehistory=TRUE)
#' querykey(e)
setGeneric("querykey", function(x, ...) standardGeneric("querykey"))

