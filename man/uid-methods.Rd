\docType{methods}
\name{uid}
\alias{uid}
\alias{uid,elink-method}
\alias{uid,entrez_linkset-method}
\alias{uid,entrez_uid-method}
\alias{uid,esearch-method}
\title{uid}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} object.}

  \item{...}{Further arguments passed on to methods.}
}
\value{
  A character vector.
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

