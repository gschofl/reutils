\name{querykey}
\alias{querykey}
\alias{querykey,entrez_uid-method}
\alias{querykey,epost-method}
\alias{querykey,esearch-method}
\alias{querykey-methods}
\title{querykey}
\usage{
  \S4method{querykey}{entrez_uid}(x, ...)

  \S4method{querykey}{esearch}(x, ...)

  \S4method{querykey}{epost}(x, ...)
}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} object.}

  \item{...}{Further arguments passed on to methods.}
}
\description{
  An integer query key returned by an ESearch, EPost or
  ELink call if the History server was used. Otherwise
  \code{NA}.
}
\examples{
e <- esearch("Mus musculus", "taxonomy", usehistory=TRUE)
querykey(e)
}
\seealso{
  \code{\link{einfo}}, \code{\link{esearch}},
  \code{\link{esummary}}, \code{\link{efetch}},
  \code{\link{elink}}, \code{\link{epost}},
  \code{\link{egquery}}, \code{\link{espell}},
  \code{\link{ecitmatch}}.
}

