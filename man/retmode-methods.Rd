\docType{methods}
\name{retmode}
\alias{retmode}
\alias{retmode,eutil,eutil-method}
\title{retmode}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} object.}

  \item{...}{Further arguments passed on to methods.}
}
\description{
  Retrieval mode of an \code{eutil} object. One of
  \code{xml} \code{text}, \code{asn.1} or \code{NULL} if
  not supported by an E-Utility.
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

