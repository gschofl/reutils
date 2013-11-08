\name{xname}
\alias{xname}
\title{Extract the tag name of XML nodes}
\usage{
  xname(doc, path, as = "character",
    default = NA_character_, ...)
}
\arguments{
  \item{doc}{An object of class
  \code{XMLInternalDocument}.}

  \item{path}{An XPath expression.}

  \item{as}{Mode of return value.}

  \item{default}{Default return value.}

  \item{...}{Arguments passed to
  \code{\link[XML]{xpathApply}}.}
}
\description{
  Extract the tag name of XML nodes
}
\keyword{internal}

