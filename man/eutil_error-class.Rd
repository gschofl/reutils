\name{eutil_error-class}
\alias{eutil_error-class}
\title{Class \code{"eutil_error"}}
\description{
  A container for handling errors when trying to parse XML
  files returned by Entrez.
}
\section{Fields}{
  \describe{ \item{\code{error}:}{Error messages returned
  by the Entrez server when no search could be performed
  like, e.g., "Invalid db name specified".}
  \item{\code{errmsg}:}{Error messages pertaining to the
  search like e.g., "PhraseNotFound".}
  \item{\code{wrnmsg}:}{Warnings like, e.g., "No items
  found."} }
}

\section{Extends}{
  All reference classes extend and inherit methods from
  \code{"\linkS4class{envRefClass}"}.
}
\examples{
###
}
\seealso{
  \code{\link{eutil_error}}, \code{\linkS4class{eutil}}.
}
\keyword{classes}
\keyword{internal}

