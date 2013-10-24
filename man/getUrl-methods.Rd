\name{getUrl}
\alias{getUrl}
\alias{getUrl,eutil-method}
\alias{getUrl-methods}
\title{getUrl}
\usage{
  \S4method{getUrl}{eutil}(x, ...)
}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} object.}

  \item{...}{Further arguments passed on to methods.}
}
\description{
  Retrieve the URL used to perform an Entrez E-Utilities
  query.
}
\examples{
e <- efetch("AV333213.1", "protein", rettype="fasta")
getUrl(e)
}
\seealso{
  \code{\link{einfo}}, \code{\link{esearch}},
  \code{\link{esummary}}, \code{\link{efetch}},
  \code{\link{elink}}, \code{\link{epost}},
  \code{\link{egquery}}, \code{\link{espell}},
  \code{\link{ecitmatch}}.
}

