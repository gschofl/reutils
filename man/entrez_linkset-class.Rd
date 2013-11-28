\name{entrez_linkset-class}
\alias{entrez_linkset-class}
\title{Class \code{"entrez_linkset"}}
\description{
  A list containing a set of links as returned by a call to
  \code{\link{elink}}. Each element of the list is a
  character vector of UIDs of class \code{"entrez_link"}
  with three attributes: \describe{
  \item{\code{score}:}{Similarity scores between query UIDs
  and the linked UIDs} \item{\code{database}:}{The
  destination database of the ELink query.}
  \item{\code{linkName}:}{Name of the retrieved Entrez link
  of the form \emph{dbFrom_dbTo_subset}} }
}
\details{
  An \code{"entrez_linkset"} has two global attributes:
  \describe{ \item{\code{uid}:}{The input UIDs.}
  \item{\code{database}:}{The database containing the input
  UIDs.} }
}
\examples{
###
}
\keyword{classes}
\keyword{internal}

