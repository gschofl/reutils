\docType{methods}
\name{[.esearch}
\alias{[,esearch,numeric-method}
\alias{[.esearch}
\title{ESearch Accessors}
\arguments{
  \item{x}{An \code{\linkS4class{einfo}} object.}

  \item{i}{Integer indices.}
}
\value{
  A \code{entrez_uid} object.
}
\description{
  Extract UIDs from an \code{\link{esearch}} object.
}
\examples{
e <- esearch("Mus musculus", "protein", retmax=20)
e[1:5]
## pass the subset directly on to esummary or efetch
content(esummary(e[1:5]), "parsed")
}

