
# Test efetch() -----------------------------------------------------------

context("Testing 'efetch()'")

test_that("efetch works with UIDs provided as character or numeric vectors", {
  a <- efetch(c(28800982, 28628843), "protein", "fasta")
  ## sometimes NCBI returns an empty TSeqSet for whatever reason. This completely
  ## breaks the test so we better check for that scenario
  if (!all(is.na(a$xmlName("//TSeqSet/*")))) {
    expect_equal(a$xmlName("//TSeqSet/*"), c("TSeq", "TSeq"))
  }
})

test_that("efetch works with an esearch object as input", {
  x <- esearch("erythroid associated factor AND Homo sapiens", 'protein', retmax=2)
  a <- efetch(x, rettype="fasta")
  if (!all(is.na(a$xmlValue("//TSeq_taxid")))) {
    expect_equal(a$xmlValue("//TSeq_taxid"), c("9606", "9606"))
  }
})
