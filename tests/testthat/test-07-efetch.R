
# Test efetch() -----------------------------------------------------------

context("Testing 'efetch()'")

test_that("efetch works with UIDs provided as character or numeric vectors", {
  a <- efetch(c(28800982, 28628843), "protein", "fasta")
  expect_equal(a$xmlName("//TSeqSet/*"), c("TSeq", "TSeq"))
})

test_that("efetch works with an esearch object as input", {
  x <- esearch("erythroid associated factor AND Homo sapiens", 'protein', retmax=2)
  a <- efetch(x, rettype="fasta")
  expect_equal(a$xmlValue("//TSeq_taxid"), c("9606", "9606"))
})
