## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
hook_output <- knitr::knit_hooks$get("output")
knitr::knit_hooks$set(output = function(x, options) {
  lines <- options$output.lines
  if (is.null(lines)) {
    return(hook_output(x, options))
  }
  x <- unlist(strsplit(x, "\n"))
  more <- "..."
  if (length(lines) == 1) {
    if (length(x) > lines) {
      x <- c(head(x, lines), more)
    }
  } else {
    x <- c(if (abs(lines[1]) > 1 | lines[1] < 0) more else NULL,
           x[lines],
           if (length(x) > lines[abs(length(lines))]) more else NULL
    )
  }
  x <- paste(c(x, ""), collapse = "\n")
  hook_output(x, options)
}) 
options(reutils.api.key = NULL)
options(reutils.rcurl.connecttimeout = 50)
library(reutils)

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
pmid <- esearch("Chlamydia psittaci[titl] and 2020[pdat]", "pubmed")
pmid

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
pmid2 <- esearch("Chlamydia psittaci[titl] and 2020[pdat]", "pubmed", usehistory = TRUE)
pmid2

## ---- eval=TRUE, echo=TRUE, output.lines=20-----------------------------------
cpaf <- esearch("Chlamydiaceae[orgn] and PMP[gene]", "nucleotide")
cpaf

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
getUrl(cpaf)

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
getError(cpaf)

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
database(cpaf)

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
uid(cpaf)

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
querykey(pmid2)


## ---- eval=TRUE, echo=TRUE----------------------------------------------------
webenv(pmid2)

## ---- eval=TRUE, echo=TRUE, output.lines=20-----------------------------------
content(cpaf, "xml")

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
cpaf$xmlValue("//Id")

## ---- eval=TRUE, echo=TRUE, output.lines=20-----------------------------------
esum <- esummary(cpaf[1])
esum

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
esum <- esummary(cpaf[1:4])
content(esum, "parsed")

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
cpaf <- esearch("Chlamydia[orgn] and CPAF", "protein")
cpaf

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
cpaff <- efetch(cpaf[1], rettype = "fasta", retmode = "text")
cpaff

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
write(content(cpaff), file = "~/cpaf.fna")

## ---- eval=TRUE, echo=TRUE, output.lines=20-----------------------------------
cpafx <- efetch(cpaf, rettype = "fasta", retmode = "xml")
cpafx

## ---- eval=TRUE, echo=TRUE, output.lines=20-----------------------------------
aa <- cpafx$xmlValue("//TSeq_sequence")
aa
defline <- cpafx$xmlValue("//TSeq_defline")
defline

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
einfo()

## ---- eval=TRUE, echo=TRUE----------------------------------------------------
einfo("taxonomy")

