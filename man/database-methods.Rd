\docType{methods}
\name{database}
\alias{database}
\alias{database,entrez_linkset-method}
\alias{database,entrez_uid-method}
\alias{database,eutil-method}
\title{database}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} object.}

  \item{...}{Further arguments passed on to methods.}
}
\value{
  A character string.
}
\description{
  Retrieve the target database name from an
  \code{\linkS4class{eutil}} object.
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

