#' @include eutil.R
#' @include parse-params.R
NULL

#' @export
.efetch <- setRefClass(
  Class="efetch",
  contains="eutil",
  methods=list(
    initialize=function(method, ...) {
      callSuper()
      perform_query("efetch", method=method, ...)
      if (errors$all_empty() && retmode() == "xml") {
        errors$check_errors(.self)
      }
    },
    show=function() {
      cat("Object of class", sQuote(eutil()), "\n")
      nhead <- getOption("reutils.show.headlines")
      if (no_errors()) {
        if (retmode() == "xml") {
          methods::show(get_content("xml"))
## This does not work well with huge XML objects          
#           out <- capture.output(get_content("xml"))
#           if (!is.null(nhead)) {
#             out <- out[1:nhead]
#           }
#           cat(substring(out, first=1, last=getOption("width") - 2), "...", sep="\n")
        } else {
          con <- get_content("textConnection")
          on.exit(close(con))
          headlines <- readLines(con, n=nhead %||% -1L)
          cat(headlines, "...", sep="\n")
        }
      } else {
        methods::show(get_error())
      }
      tail <- sprintf("EFetch query using the %s database.\nQuery url: %s\nRetrieval type: %s, retrieval mode: %s\n",
                      sQuote(database()), sQuote(ellipsize(get_url(), offset=15)),
                      sQuote(rettype()), sQuote(retmode()))
      cat(tail, sep="\n")
    } 
  )
)

#' @rdname content
#' @export
setMethod("content", "efetch", function(x, as=NULL, ...) {
  as <- as %||% retmode(x)
  if (as == "asn.1") {
    as <- "text"
  }
  if (as == "parsed") {
    as <- retmode(x)
  }
  if (as == "xml" && retmode(x) != "xml") {
    stop("This document does not contain XML data", call.=FALSE)
  }
  as <- match.arg(as, c("text", "textConnection", "xml"))
  callNextMethod(x=x, as=as)
})

#' \code{efetch} performs calls to the NCBI EFetch utility to retrieve data records
#' in the requested format for an NCBI Accession Number, one or more primary UIDs,
#' or for a set of UIDs stored in the user's web environment.
#' 
#' @details
#' See the official online documentation for NCBI's
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/\#chapter4.EFetch}{EUtilities}
#' for additional information.
#' 
#' See
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
#' for the default values for \code{rettype} and r\code{retmode}, as well as a list of the available
#' databases for the  EFetch utility.
#' 
#' @title efetch - downloading full records
#' @param uid (Required) A list of UIDs provided either as a character vector, as an
#' \code{esearch} object, or by reference to a web environment
#' and a query key obtained directly from previous calls to \code{\link{esearch}}
#' (if \code{usehistory=TRUE}), \code{\link{epost}} or \code{\link{elink}}.
#' If UIDs are provided as a plain character vector, \code{db} must be
#' specified explicitly, and all of the UIDs must be from the database
#' specified by \code{db}.
#' @param db (Required if \code{uid} is a character vector of UIDs)
#' Database from which to retrieve records. See
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25497/table/chapter2.chapter2_table1/?report=objectonly}{here}
#' for the supported databases.
#' @param rettype A character string specifying the retrieval type, such as 'abstract' or
#' 'medline' for PubMed, 'gp' or 'fasta' for Protein, or 'gb', or 'fasta' for Nuccore. See 
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
#' for the available values for each database.
#' @param retmode A character string specifying the data mode of the records returned,
#' such as 'text' or 'xml'. See 
#' \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
#' for the available values for each database.
#' @param retstart Numeric index of the first record to be retrieved.
#' @param retmax Total number of records from the input set to be retrieved.
#' @param querykey An integer specifying which of the UID lists attached
#' to a user's Web Environment will be used as input to \code{efetch}.
#' (Usually obtained drectely from objects returned by a previous call to
#' \code{\link{esearch}}, \code{\link{epost}} or \code{\link{elink}}.)
#' @param webenv A character string specifying the Web Environment that
#' contains the UID list. (Usually obtained directely from objects returned
#' by a previous call to \code{\link{esearch}}, \code{\link{epost}} or
#' \code{\link{elink}}.)
#' @param strand Strand of DNA to retrieve. (1: plus strand, 2: minus strand)
#' @param seqstart First sequence base to retrieve.
#' @param seqstop Last sequence base to retrieve.
#' @param complexity Data content to return. (0: entire data structure,
#' 1: bioseq, 2: minimal bioseq-set, 3: minimal nuc-prot, 4: minimal pub-set)
#' @return An \code{\linkS4class{efetch}} object.
#' @export
#' @seealso
#' \code{\link{content}}, \code{\link{getUrl}}, \code{\link{getError}},
#' \code{\link{database}}, \code{\link{retmode}}, \code{\link{rettype}}.
#' @examples
#' \dontrun{
#' ## From Protein, retrieve a raw GenPept record and write it to a file.
#' p <- efetch("195055", "protein", "gp")
#' p
#' 
#' write(content(p, "text"), file = "~/AAD15290.gp")
#' 
#' ## Get accessions for a list of GenBank IDs (GIs)
#' acc <- efetch(c("1621261", "89318838", "68536103", "20807972", "730439"),
#'               "protein", rettype = "acc")
#' acc
#' acc <- strsplit(content(acc), "\n")[[1]]
#' acc
#' 
#' ## Get GIs from a list of accession numbers
#' gi <- efetch(c("CAB02640.1", "EAS10332.1", "YP_250808.1", "NP_623143.1", "P41007.1"),
#'              "protein", "uilist")
#' gi
#' 
#' ## we can conveniently extract the UIDs using the eutil method #xmlValue(xpath)
#' gi$xmlValue("/IdList/Id")
#' 
#' ## or we can extract the contents of the efetch query using the fuction content()
#' ## and use the XML package to retrieve the UIDs
#' doc <- content(gi)
#' XML::xpathSApply(doc, "/IdList/Id", XML::xmlValue)
#'
#' ## Get the scientific name for an organism starting with the NCBI taxon id.
#' tx <- efetch("527031", "taxonomy")
#' tx
#'  
#' ## Convenience accessor for XML nodes of interest using XPath
#' ## Extract the TaxIds of the Lineage
#' tx["//LineageEx/Taxon/TaxId"]
#' 
#' ## Use an XPath expession to extract the scientific name.
#' tx$xmlValue("/TaxaSet/Taxon/ScientificName")
#' }
efetch <- function(uid, db=NULL, rettype=NULL, retmode=NULL,
                   retstart=NULL, retmax=NULL, querykey=NULL,
                   webenv=NULL, strand=NULL, seqstart=NULL,
                   seqstop=NULL, complexity=NULL) {
  ## extract query parameters
  params <- parse_params(uid, db, querykey, webenv)

  # set default rettype and retmode for a given db
  r <- ncbi_retrieval_type(params$db, rettype, retmode)
  
  if (is.null(retmax)) {
    retmax <- Inf
  }
  if (retmax > 500 && (is.finite(params$count) && (params$count > 500))) {
    # if record_count exceeds 500 issue a warning and recommend
    # efetch.batch()
    message(gettextf("You are attempting to download %s records.\nOnly the first 500 are downloaded. Use efetch.batch() instead.",
                     min(c(params$count, retmax))))
    retmax <- 500
    params$uid <- params$uid[seq_len(500)]
  } else if (is.na(params$count)) {
    # this takes care of the cases where we don't actually know how many UIDs
    # are stored on the history server
    # message("A single download request is restricted to 500 records.\nUse efetch.batch() to download more records.")
    retmax <- 500
  }
  .efetch(method=if (length(params$uid) < 100) "GET" else "POST",
         db=params$db, id=.collapse(params$uid),
         query_key=params$querykey, WebEnv=params$webenv, 
         retmode=r$retmode, rettype=r$rettype, retstart=retstart,
         retmax=retmax, strand=strand, seq_start=seqstart,
         seq_stop=seqstop, complexity=complexity)
}

#' EFetch accessors
#' 
#' Extract XML nodes from an \code{\linkS4class{efetch}} object.
#' 
#' @param x An \code{\linkS4class{efetch}} object containing XML data.
#' @param i An XPath expression specifying the XML nodes to extract.
#' @return An XML node set.
#' @rdname efetch-methods
#' @export
#' @examples
#' \dontrun{
#' p <- efetch("195055", "protein", "gp", "xml")
#' p['//GBFeature[GBFeature_key="mat_peptide"]//GBQualifier_value']
#' }
setMethod("[", c("efetch", "character"), function(x, i) {
  if (retmode(x) != "xml") {
    stop("This document does not contain XML data", call.=FALSE)
  }
  x$xmlSet(i)  
})

#' @rdname efetch-methods
#' @export
setMethod("[[", c("efetch", "character"), function(x, i) {
  ans <- x[i]
  if (length(ans) > 1) {
    warning(length(ans), " elements in node set. Returning just the first!")
  }
  ans[[1]]
})

