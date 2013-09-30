\docType{methods}
\name{content}
\alias{content}
\alias{content,ecitmatch,ecitmatch-method}
\alias{content,efetch,efetch-method}
\alias{content,eutil,eutil-method}
\title{Extract the data content from an Entrez request}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} object.}

  \item{as}{Type of output: \code{\dQuote{xml}},
  \code{\dQuote{text}}, or \code{\dQuote{parsed}}.}
}
\description{
  Extract the data content from an Entrez request
}
\details{
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

