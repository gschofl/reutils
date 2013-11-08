\name{[.efetch}
\alias{[,efetch,character-method}
\alias{[.efetch}
\alias{[[,efetch,character-method}
\alias{[[.efetch}
\title{EFetch accessors}
\usage{
  x[...]

  x[[...]]
}
\arguments{
  \item{x}{An \code{\linkS4class{efetch}} object containing
  XML data.}

  \item{...}{An XPath expression}
}
\value{
  An XML node set.
}
\description{
  Extract XML nodes from an \code{\linkS4class{efetch}}
  object.
}
\examples{
p <- efetch("195055", "protein", "gp", "xml")
p['//GBFeature[GBFeature_key="mat_peptide"]//GBQualifier_value']
}

