\name{rettype}
\alias{rettype}
\alias{rettype,eutil-method}
\alias{rettype-methods}
\title{rettype}
\usage{
  \S4method{rettype}{eutil}(x, ...)
}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} object.}

  \item{...}{Further arguments passed on to methods.}
}
\description{
  Retrieval type of an \code{eutil} object. See
  \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1/?report=objectonly}{here}
  for the available retrieval types for different NCBI
  databases.
}
\examples{
e <- esearch("Mus musculus", "taxonomy")
rettype(e)
}
\seealso{
  \code{\link{einfo}}, \code{\link{esearch}},
  \code{\link{esummary}}, \code{\link{efetch}},
  \code{\link{elink}}, \code{\link{epost}},
  \code{\link{egquery}}, \code{\link{espell}},
  \code{\link{ecitmatch}}.
}

