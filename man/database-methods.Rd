\name{database}
\alias{database}
\alias{database,entrez_linkset-method}
\alias{database,entrez_uid-method}
\alias{database,eutil-method}
\alias{database-methods}
\title{database}
\usage{
  \S4method{database}{eutil}(x, ...)

  \S4method{database}{entrez_uid}(x, ...)

  \S4method{database}{entrez_linkset}(x, ...)
}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} object.}

  \item{...}{Further arguments passed on to methods.}
}
\description{
  Target database of an \code{eutil} object.
}
\examples{
e <- esearch("Mus musculus", "taxonomy")
database(e)
}
\seealso{
  \code{\link{einfo}}, \code{\link{esearch}},
  \code{\link{esummary}}, \code{\link{efetch}},
  \code{\link{elink}}, \code{\link{epost}},
  \code{\link{egquery}}, \code{\link{espell}},
  \code{\link{ecitmatch}}.
}

