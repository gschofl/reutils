\docType{methods}
\name{querykey}
\alias{querykey}
\alias{querykey,entrez_uid,entrez_uid-method}
\alias{querykey,epost,epost-method}
\alias{querykey,esearch,esearch-method}
\title{querykey}
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

