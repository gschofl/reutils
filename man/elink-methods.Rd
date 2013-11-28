\docType{methods}
\name{[.elink}
\alias{[}
\alias{[,elink,character-method}
\alias{[,elink,numeric-method}
\alias{[.elink}
\title{ELink Accessors}
\arguments{
  \item{x}{An \code{\linkS4class{elink}} object.}

  \item{i}{Integer or character indices.}
}
\value{
  A \code{\linkS4class{entrez_linkset}} object.
}
\description{
  Extract UIDs from an \code{\link{elink}} object.
}
\examples{
e <- elink(c("34577062", "24475906"), dbFrom="nuccore")
e[1]
}

