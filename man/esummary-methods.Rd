\docType{methods}
\name{[.esummary}
\alias{[,esummary,character-method}
\alias{[.esummary}
\alias{[[,esummary,character-method}
\alias{[[.esummary}
\title{ESummary accessors}
\arguments{
  \item{x}{An \code{\linkS4class{esummary}} object.}

  \item{...}{An XPath expression}
}
\value{
  An XML node set.
}
\description{
  Extract XML nodes from an \code{\linkS4class{esummary}}
  object.
}
\examples{
ds <- esummary("470338", "protein")
ds["//Slen/node()"]

require("XML")
as.numeric(xmlValue(ds[["//Slen"]]))
}

