% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eutil.R
\name{retmode}
\alias{retmode}
\alias{retmode,eutil-method}
\title{retmode}
\usage{
retmode(x, ...)

\S4method{retmode}{eutil}(x, ...)
}
\arguments{
\item{x}{An \code{\linkS4class{eutil}} object.}

\item{...}{Further arguments passed on to methods.}
}
\value{
A character string or \code{NULL}.
}
\description{
Get the \dQuote{retrieval mode} of an \code{\linkS4class{eutil}} object.
It is usually one of \code{xml}, \code{json}, \code{text}, or \code{asn.1}. 
It is set to \code{NULL} if \dQuote{retrieval mode} is not supported by an
E-Utility.
}
\section{Methods (by class)}{
\itemize{
\item \code{eutil}: Access the \dQuote{retrieval mode} of an \code{eutil}
object.
}}

\examples{
\dontrun{
e <- efetch("10090", "taxonomy")
retmode(e)
}
}
\seealso{
\code{\link{einfo}}, \code{\link{esearch}}, \code{\link{esummary}},
   \code{\link{efetch}}, \code{\link{elink}}, \code{\link{epost}},
   \code{\link{egquery}}, \code{\link{espell}}, \code{\link{ecitmatch}}.
}
