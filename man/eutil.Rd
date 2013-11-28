\name{eutil}
\alias{eutil}
\title{Generator object for the \code{\linkS4class{eutil}} classes}
\usage{
  eutil(...)
}
\description{
  The generator object for \code{\linkS4class{eutil}},
  \code{\linkS4class{einfo}}, \code{\linkS4class{esearch}},
  \code{\linkS4class{esummary}},
  \code{\linkS4class{efetch}}, \code{\linkS4class{elink}},
  \code{\linkS4class{epost}}, \code{\linkS4class{egquery}},
  \code{\linkS4class{espell}}, and
  \code{\linkS4class{ecitmatch}} reference classes. This
  object implements the low level request generator for
  interaction with the NCBI services. It should not be used
  direcly, but initialized through the respective
  constructor functions \code{\link{einfo}},
  \code{\link{esearch}}, \code{\link{esummary}},
  \code{\link{efetch}}, \code{\link{elink}},
  \code{\link{epost}}, \code{\link{egquery}},
  \code{\link{espell}}, and \code{\link{ecitmatch}}.
}
\section{Public methods}{
  \describe{ \item{\code{#xmlValue(xpath, as="character",
  default=NA_character_)}:}{ Return the text value of a
  leaf node in the XML data given a valid XPath
  expression.} \item{\code{#xmlAttr(xpath, name,
  as="character", default=NA_character_)}:}{ Return the
  value of an XML attribute given a valid XPath expression
  and an attribute name.} \item{\code{#xmlName(xpath,
  as="character", default=NA_character_)}:}{ Return the tag
  names of XML nodes given a valid XPath expression.}
  \item{\code{#xmlSet(xpath)}:}{ Return a set of XML nodes
  given a valid XPath expression.} }
}

\section{Internal methods}{
  \describe{ \item{\code{#get_url()}:}{Return the URL used
  for an Entrez query; should not be used directly, use
  \code{\link{getUrl}} instead.}
  \item{\code{#get_error()}:}{Return
  \code{\linkS4class{eutil_error}}s; should not be used
  directly, use \code{\link{getError}} instead.}
  \item{\code{#get_content(as='text')}:}{Return the results
  of an Entrez query as text, xml, or a parsed R object;
  should not be used directly, use \code{\link{content}}
  instead.} \item{\code{#perform_query(method="GET",
  ...)}:}{Perform an Entrez query using either http GET or
  POST requests; should not be used directly.} }
}
\seealso{
  \code{\linkS4class{eutil}}, \code{\link{einfo}},
  \code{\link{esearch}}, \code{\link{esummary}},
  \code{\link{efetch}}, \code{\link{elink}},
  \code{\link{epost}}, \code{\link{egquery}},
  \code{\link{espell}}, and \code{\link{ecitmatch}}.
}
\keyword{classes}
\keyword{internal}

