\name{content}
\alias{content}
\alias{content,ecitmatch-method}
\alias{content,efetch-method}
\alias{content,eutil-method}
\alias{content-methods}
\title{Extract the data content from an Entrez request}
\usage{
  \S4method{content}{eutil}(x, as = "xml", ...)

  \S4method{content}{efetch}(x, as = NULL, ...)

  \S4method{content}{ecitmatch}(x, as = "text", ...)
}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} object.}

  \item{as}{Type of output: \code{"xml"}, \code{"text"}, or
  \code{"parsed"}.}

  \item{...}{Further arguments passed on to methods.}
}
\description{
  There are three ways to access data returned by an Entrez
  request: as a character string \code{(as = "text")}, as a
  parsed XML tree \code{(as = "xml")}, or, if supported,
  parsed into some native R object, e.g. a \code{list} or a
  \code{data.frame} \code{(as = "parsed")}.
}
\examples{
e <- einfo()

## return XML as an 'XMLInternalDocument'.
content(e, "xml")

## return XML as character string.
cat(content(e, "text"))

## return DbNames parsed into a character vector.
content(e, "parsed")
}
\seealso{
  \code{\link{einfo}}, \code{\link{esearch}},
  \code{\link{esummary}}, \code{\link{efetch}},
  \code{\link{elink}}, \code{\link{epost}},
  \code{\link{egquery}}, \code{\link{espell}},
  \code{\link{ecitmatch}}.
}

