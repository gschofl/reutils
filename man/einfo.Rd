\name{einfo}
\alias{einfo}
\title{einfo - getting database statistics and search fields}
\usage{
  einfo(db = NULL)
}
\arguments{
  \item{db}{A valid NCBI database name. If \code{NULL}, a
  list of all current NCBI databases is returned.}
}
\value{
  A \code{\linkS4class{einfo}} object.
}
\description{
  \code{einfo} performs calls to the NCBI EInfo utility to
  retrieve the names of all valid Entrez databases, or, if
  \code{db} is provided, to retrieve statistics for a
  single database, including lists of indexing fields and
  available link names.
}
\details{
  See the official online documentation for NCBI's
  \href{http://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.EInfo}{EUtilities}
  for additional information.
}
\examples{
## Return a list of all current Entrez database names
einfo()
## Return statistics for the Entrez Gene database and parse
## the returned data into a data.frame
x <- einfo("gene")
content(x, "parsed")
}
\seealso{
  \code{\link{content}}, \code{\link{getUrl}},
  \code{\link{getError}}.
}
