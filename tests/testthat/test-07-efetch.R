
# Test efetch() -----------------------------------------------------------

context("Testing 'efetch()'")

test_that("efetch works with UIDs provided as character or numeric vectors", {
  if (getOption('reutils.test.remote')) {
    a <- efetch(c(28800982, 28628843), "protein", "fasta")
    save(a, file = "data/test-07-a.rda")
  } else {
    load("data/test-07-a.rda")
  }
  ## sometimes NCBI returns an empty TSeqSet for whatever reason. This completely
  ## breaks the test so we better check for that scenario
  if (!all(is.na(a$xmlName("//TSeqSet/*")))) {
    expect_equal(a$xmlName("//TSeqSet/*"), c("TSeq", "TSeq"))
  }
})

test_that("efetch works with an esearch object as input", {
  if (getOption('reutils.test.remote')) {
    x <- esearch("erythroid associated factor AND Homo sapiens", 'protein', retmax=2)
    a <- efetch(x, rettype="fasta")
    save(a, file = "data/test-07-a2.rda")
  } else {
    load("data/test-07-a2.rda")
  }
  if (!all(is.na(a$xmlValue("//TSeq_taxid")))) {
    expect_equal(a$xmlValue("//TSeq_taxid"), c("9606", "9606"))
  }
})
