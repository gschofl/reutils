\docType{methods}
\name{content}
\alias{content}
\alias{content,ecitmatch-method}
\alias{content,efetch-method}
\alias{content,eutil-method}
\title{Extract the data content from an Entrez request}
\arguments{
  \item{x}{An \code{\linkS4class{eutil}} object.}

  \item{as}{Type of output: \code{"xml"}, \code{"text"},
  \code{"textConnection"}, or \code{"parsed"}.}

  \item{...}{Further arguments passed on to methods.}
}
\description{
  There are four ways to access data returned by an Entrez
  request: as a character string \code{(as = "text")}, as a
  \code{\link{textConnection}} \code{(as =
  "textConnection")}, as a parsed XML tree \code{(as =
  "xml")}, or, if supported, parsed into a native R object,
  e.g. a \code{list} or a \code{data.frame} \code{(as =
  "parsed")}.
}
\examples{
e <- einfo()

## return XML as an 'XMLInternalDocument'.
content(e, "xml")

## return XML as character string.
cat(content(e, "text"))

## return DbNames parsed into a character vector.
content(e, "parsed")


\dontrun{
## return a textConnection to allow linewise read of the data.
x <- efetch("CP000828", "nuccore", rettype="gbwithparts", retmode="text")
con <- content(x, "textConnection")
readLines(con, 2)
close(con)
}
}
\seealso{
  \code{\link{einfo}}, \code{\link{esearch}},
  \code{\link{esummary}}, \code{\link{efetch}},
  \code{\link{elink}}, \code{\link{epost}},
  \code{\link{egquery}}, \code{\link{espell}},
  \code{\link{ecitmatch}}.
}

