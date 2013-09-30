\docType{methods}
\name{[-method}
\alias{[,einfo,numeric,missing,ANY,einfo-method}
\alias{[-method}
\alias{[[,einfo,character,missing,einfo-method}
\alias{[[,einfo,numeric,missing,einfo-method}
\alias{[[-method}
\title{EInfo accessors}
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

