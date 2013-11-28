\docType{methods}
\name{retmode}
\alias{retmode}
\alias{retmode,eutil-method}
\title{retmode}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} object.}

  \item{...}{Further arguments passed on to methods.}
}
\description{
  Get the \dQuote{retrieval mode} of an
  \code{\linkS4class{eutil}} object It is usually one of
  \code{xml} \code{text}, or \code{asn.1}. It is set to
  \code{NULL} if \dQuote{retrieval mode} is not supported
  by an E-Utility.
}
\examples{
e <- efetch("10090", "taxonomy")
retmode(e)
}
\seealso{
  \code{\link{einfo}}, \code{\link{esearch}},
  \code{\link{esummary}}, \code{\link{efetch}},
  \code{\link{elink}}, \code{\link{epost}},
  \code{\link{egquery}}, \code{\link{espell}},
  \code{\link{ecitmatch}}.
}

