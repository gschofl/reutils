\name{uid}
\alias{uid}
\alias{uid,entrez_linkset-method}
\alias{uid,entrez_uid-method}
\alias{uid,esearch-method}
\alias{uid-methods}
\title{uid}
\usage{
  \S4method{uid}{entrez_uid}(x, ...)

  \S4method{uid}{esearch}(x, ...)

  \S4method{uid}{entrez_linkset}(x, ...)
}
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

