#' @details 
#' 
#' With nine \href{https://www.ncbi.nlm.nih.gov/books/NBK25501/}{Entrez Progamming Utilities},
#' NCBI provides a programmatical interface to the Entrez query and database
#' system for searching and retrieving requested data
#' 
#' Each of these tools corresponds to an \code{R} function in the \code{reutils}
#' package described below.
#' 
#' The output returned by the EUtils is typically in XML format. To gain access to
#' this output you have several options:
#' 
#' \enumerate{
#'  \item Use the \code{content(as = "xml")} method to extract the output as an
#'    \code{XMLInternalDocument} object and process it further using the
#'    facilities provided by the \code{XML} package.
#' 
#' \item Use the \code{content(as = "parsed")} method to extract the output
#'    into \code{data.frame}s. Note that this is currently only implemented for
#'    \emph{docsum}s returned by \code{\link{esummary}}, \emph{uilist}s returned
#'    by \code{\link{esearch}}, and the output returned by \code{\link{einfo}}.
#' 
#' \item Access specific nodes in the XML tree using XPath expressions with the
#'    reference class methods \code{#xmlValue}, \code{#xmlAttr}, or \code{#xmlName}
#'    built into \code{\linkS4class{eutil}} objects.
#' }
#' 
#' The Entrez Programming Utilities can also generate output in other formats,
#' such as plain-text Fasta or GenBank files for sequence databases,
#' or the MedLine format for the literature database. The type of output is
#' generally controlled by setting the \code{retmode} and \code{rettype} arguments
#' when calling a EUtil.
#' Please check the relevant
#' \href{https://www.ncbi.nlm.nih.gov/books/NBK25497/#chapter2.Usage_Guidelines_and_Requiremen}{usage guidelines}
#' when using these services. Note that Entrez server requests are subject to
#' frequency limits.
#' 
#' @section Main functions:
#' \itemize{
#'    \item \code{\link{esearch}}: Search and retrieve primary UIDs for use
#'    with \code{esummary}, \code{elink}, or \code{efetch}.
#'    \code{esearch} additionally returns term translations and optionally
#'    stores results for future use in the user's Web Environment.
#'     
#'    \item \code{\link{esummary}}: Retrieve document summaries from
#'    a list of primary UIDs (Provided as a character vector or as an
#'    \code{esearch} object).
#'     
#'    \item \code{\link{egquery}}: Provides Entrez database counts in XML
#'    for a single search term using a Global Query.
#'   
#'    \item \code{\link{einfo}}: Retrieve field names, term counts, last
#'    update, and available updates for each database.
#'         
#'    \item \code{\link{efetch}}: Retrieve data records in a specified
#'    format corresponding to a list of primary UIDs or from the user's Web
#'    Environment in the Entrez History server.
#'   
#'    \item \code{\link{elink}}: Returns a list of UIDs (and relevancy
#'    scores) from a target database that are related to a list of UIDs in
#'    the same database or in another Entrez database.
#'     
#'    \item \code{\link{epost}}: Uploads primary UIDs to the users's Web 
#'    Environment on the Entrez history server for subsequent use with
#'    \code{esummary}, \code{elink}, or \code{efetch}.
#'     
#'    \item \code{\link{espell}}: Provide spelling suggestions.
#'     
#'    \item \code{\link{ecitmatch}}: Retrieves PubMed IDs (PMIDs) that
#'    correspond to a set of input citation strings
#'     
#'    \item \code{\link{content}}: Extract the content of a request from the
#'    \code{\linkS4class{eutil}} object returned by any of the above functions.
#' }
#'
#' @section Package options:
#' \pkg{reutils} uses five \code{\link{options}} to configure behaviour:
#' \itemize{
#'   \item \code{reutils.api.key}: Starting May 2018, NCBI limits eutils queries
#'      to 3 per second. This limit is increased to 10 per second with an API key.
#'      If you are going to perform a lot of queries consider registering an NCBI
#'      \href{https://www.ncbi.nlm.nih.gov/account/}{account} to obtain a key.
#'      You can then set the \code{reutils.api.key} option to your API key in
#'      your .Rprofile file.
#'      
#'   \item \code{reutils.show.headlines}: By default \code{\linkS4class{efetch}}
#'      objects containing text data show only the first 12 lines. This is quite handy
#'      if you have downloaded a fairly large genome in Genbank file format. This
#'      can be changed by setting the global option \code{reutils.show.headlines} to
#'      another numeric value or \code{NULL}.
#'
#'   \item \code{reutils.verbose.queries}: If you perform many queries interactively
#'      you might want to get messages announcing the queries you run. You can do so by setting
#'      the option \code{reutils.verbose.queries} to \code{TRUE}.
#'      
#'   \item \code{reutils.test.remote}: Unit tests that require online access to NCBI
#'      services are disabled by default, as they cannot be garanteed to be
#'      available/working under all circumstances. Set the option
#'      code{reutils.test.remote} to \code{TRUE} to run the full suite of tests.
#'      
#'   \item \code{reutils.rcurl.connecttimeout}: Set the connection timeout (Default: 10).
#' }
#'    
#' @importFrom methods callNextMethod is new
#' @importFrom stats setNames
#' @example inst/examples/reutils.R
#' @author Gerhard Schöfl \email{gerhard.schofl@gmail.com}
"_PACKAGE"
