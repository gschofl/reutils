\docType{methods}
\name{[.einfo}
\alias{[,einfo,ANY-method}
\alias{[.einfo}
\alias{[[,einfo,ANY-method}
\alias{[[.einfo}
\title{EInfo accessors}
\arguments{
  \item{x}{An \code{\linkS4class{einfo}} object.}

  \item{i}{Indices specifying elements to extract.}
}
\value{
  A list.
}
\description{
  Extract parts of a parsed \code{\linkS4class{einfo}}
  object.
}
\examples{
e <- einfo("pubmed")
e[1:5]
e["Description"]
e[["Links"]]
}
\seealso{
  \code{\link[base]{Extract}}
}

