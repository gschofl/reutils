\docType{methods}
\name{linkset}
\alias{linkset}
\alias{linkset,elink-method}
\alias{linkset,entrez_linkset-method}
\title{linkset}
\arguments{
  \item{x}{An \code{\linkS4class{elink}} object.}

  \item{linkname}{(optional) Name of the Entrez link to
  retrieve. Every link in Entrez is given a name of the
  form \emph{dbFrom_dbTo_subset}. If \code{NULL}, all
  available links are retrieved from the object.}

  \item{...}{Further arguments passed on to methods.}
}
\value{
  A list.
}
\description{
  Retrieve a linkset from an \code{\linkS4class{elink}}
  object.
}
\examples{
## Find related articles to PMID 20210808 and xtract linked UIDs from the
## "pubmed" to "pubmed_reviews" link
x <- elink("20210808", dbFrom="pubmed", dbTo="pubmed", cmd="neighbor_score")
linkset(x, "pubmed_pubmed_reviews")
}

