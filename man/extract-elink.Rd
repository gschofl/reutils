% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elink.R
\name{[,elink,ANY,missing-method}
\alias{[,elink,ANY,missing-method}
\alias{[,elink,character,ANY-method}
\title{ELink Accessors}
\usage{
\S4method{[}{elink,ANY,missing}(x, i, j)

\S4method{[}{elink,character,ANY}(x, i)
}
\arguments{
\item{x}{An \code{\linkS4class{elink}} object.}

\item{i}{Numeric or character indices.}

\item{j}{Ignored.}
}
\value{
A \code{\linkS4class{entrez_linkset}} object.
}
\description{
Extract UIDs from an \code{\link{elink}} object.
}
\examples{
\dontrun{
e <- elink(c("34577062", "24475906"), dbFrom = "nuccore")
e[1]
}
}
