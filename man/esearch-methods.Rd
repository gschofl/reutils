\docType{methods}
\name{[-method}
\alias{[,esearch,numeric,missing,ANY,esearch-method}
\alias{[-method}
\title{Esearch Accessors}
\description{
  Extract UIDs from an \code{\link{esearch}} object.
}
\examples{
e <- esearch("Mus musculus", "protein", retmax=20)
e[1:5]
## pass the subset directly on to esummary or efetch
content(esummary(e[1:5]), "parsed")
}

