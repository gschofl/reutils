\docType{methods}
\name{getError}
\alias{getError}
\alias{getError,eutil-method}
\title{getError}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} object.}

  \item{...}{Further arguments passed on to methods.}
}
\value{
  An \code{\linkS4class{eutil_error}} object.
}
\description{
  Retrieve a http or XML parsing error from an
  \code{\linkS4class{eutil}} object.
}
\examples{
e <- efetch("Nonsensical_accession_nr", "protein", rettype="fasta")
getError(e)
}
\seealso{
  \code{\link{einfo}}, \code{\link{esearch}},
  \code{\link{esummary}}, \code{\link{efetch}},
  \code{\link{elink}}, \code{\link{epost}},
  \code{\link{egquery}}, \code{\link{espell}},
  \code{\link{ecitmatch}}.
}

