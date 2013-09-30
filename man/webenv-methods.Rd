\docType{methods}
\name{webenv}
\alias{webenv}
\alias{webenv,entrez_uid,entrez_uid-method}
\alias{webenv,epost,epost-method}
\alias{webenv,esearch,esearch-method}
\title{webenv}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} object.}

  \item{...}{Further arguments passed on to methods.}
}
\description{
  The Web environment string returned from an ESearch,
  EPost or ELink call. \code{NA} if the History server was
  not used.
}
\examples{
e <- esearch("Mus musculus", "taxonomy", usehistory=TRUE)
webenv(e)
}
\seealso{
  \code{\link{einfo}}, \code{\link{esearch}},
  \code{\link{esummary}}, \code{\link{efetch}},
  \code{\link{elink}}, \code{\link{epost}},
  \code{\link{egquery}}, \code{\link{espell}},
  \code{\link{ecitmatch}}.
}

