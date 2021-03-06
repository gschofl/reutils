% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eutil.R, R/elink.R, R/esearch.R
\name{database}
\alias{database}
\alias{database,eutil-method}
\alias{database,entrez_linkset-method}
\alias{database,entrez_uid-method}
\title{database}
\usage{
database(x, ...)

\S4method{database}{eutil}(x, ...)

\S4method{database}{entrez_linkset}(x, ...)

\S4method{database}{entrez_uid}(x, ...)
}
\arguments{
\item{x}{An \code{\linkS4class{eutil}} object.}

\item{...}{Further arguments passed on to methods.}
}
\value{
A character string.
}
\description{
Retrieve the target database name from an \code{\linkS4class{eutil}} object.
}
\section{Methods (by class)}{
\itemize{
\item \code{eutil}: Retrieve the target database name from an \code{eutil}
object.

\item \code{entrez_linkset}: Retrieve the target database name from an \code{entrez_linkset}
object.

\item \code{entrez_uid}: Retrieve the target database name from an \code{entrez_uid}
object.
}}

\examples{
\dontrun{
e <- esearch("Mus musculus", "taxonomy")
database(e)
}
}
\seealso{
\code{\link{einfo}}, \code{\link{esearch}}, \code{\link{esummary}},
   \code{\link{efetch}}, \code{\link{elink}}, \code{\link{epost}},
   \code{\link{egquery}}, \code{\link{espell}}, \code{\link{ecitmatch}}.
}
