% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/eutil.R
\docType{methods}
\name{getUrl}
\alias{getUrl}
\alias{getUrl,eutil-method}
\title{getUrl}
\usage{
getUrl(x, ...)

\S4method{getUrl}{eutil}(x, ...)
}
\arguments{
\item{x}{An \code{\linkS4class{eutil}} object.}

\item{...}{Further arguments passed on to methods.}
}
\value{
A character string.
}
\description{
Retrieve the URL used to perform an Entrez E-Utilities query.
}
\section{Methods (by class)}{
\itemize{
\item \code{eutil}: 
}}
\examples{
\dontrun{
e <- efetch("AV333213.1", "protein", rettype = "fasta")
getUrl(e)
}
}
\seealso{
\code{\link{einfo}}, \code{\link{esearch}}, \code{\link{esummary}},
   \code{\link{efetch}}, \code{\link{elink}}, \code{\link{epost}},
   \code{\link{egquery}}, \code{\link{espell}}, \code{\link{ecitmatch}}.
}
