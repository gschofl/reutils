\docType{methods}
\name{uid}
\alias{uid}
\alias{uid,entrez_uid,entrez_uid-method}
\alias{uid,esearch,esearch-method}
\title{uid}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} object.}

  \item{...}{Further arguments passed on to methods.}
}
\description{
  Retrieve the list of UIDs returned by a call to ESearch
  or ELink.
}
\examples{
e <- esearch("Mus musculus", "taxonomy")
uid(e)
}
\seealso{
  \code{\link{esearch}}, \code{\link{elink}}.
}

