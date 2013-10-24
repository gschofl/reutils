\name{eutil-class}
\alias{ecitmatch-class}
\alias{efetch-class}
\alias{egquery-class}
\alias{einfo-class}
\alias{elink-class}
\alias{epost-class}
\alias{esearch-class}
\alias{espell-class}
\alias{esummary-class}
\alias{eutil-class}
\title{\code{"eutil"} classes}
\description{
  Reference classes that capture the response from EUtils
  requests of the same name.
}
\section{Fields}{
  \describe{ \item{\code{params}:}{A named \code{list} of
  query parameters.} \item{\code{errors}:}{A
  \code{\linkS4class{eutil_error}} object.}
  \item{\code{content}:}{Results of an Entrez request as a
  character vector.} }
}

\section{Extends}{
  All reference classes extend and inherit methods from
  \code{"\linkS4class{envRefClass}"}. Furthermore,
  \code{"einfo"}, \code{"esearch"}, \code{"esummary"},
  \code{"efetch"}, \code{"elink"}, \code{"epost"},
  \code{"egquery"}, \code{"espell"}, and \code{"ecitmatch"}
  all extend the \code{"eutil"} class.
}
\examples{
showClass("eutil")
}
\seealso{
  \code{\link{eutil}}, \code{\link{einfo}},
  \code{\link{esearch}}, \code{\link{esummary}},
  \code{\link{efetch}}, \code{\link{elink}},
  \code{\link{epost}}, \code{\link{egquery}},
  \code{\link{espell}}, \code{\link{ecitmatch}}.
}
\keyword{classes}
\keyword{internal}

