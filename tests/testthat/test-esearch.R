
# Test esearch() ---------------------------------------------------------

context("Testing 'esearch()'")

a <- esearch(term="cancer", db="pubmed", reldate=60, datetype="edat",
             retmax=6, usehistory=TRUE)
b <- esearch(term="cancer", db="pubmed", reldate=60, datetype="edat",
             retmax=6, usehistory=FALSE)

test_that("esearch() returns an 'esearch' object", {
  expect_is(a, "esearch")
  expect_is(b, "esearch")
})

test_that("'content()' returns a character vector or an XMLInternalDocument", {
  expect_that(content(a, "text"), is_a("character"))
  expect_that(content(b, "text"), is_a("character"))
  expect_that(content(a, "xml"), is_a("XMLInternalDocument"))
  expect_that(content(b, "xml"), is_a("XMLInternalDocument"))
  expect_that(content(a, 'parsed'), is_a("entrez_uid"))
  expect_that(content(b, 'parsed'), is_a("entrez_uid"))
})

test_that("Subsetting an 'esearch' returns an 'esearch' object", {
  expect_that(a[1:2], is_a("entrez_uid"))
  expect_that(b[1:2], is_a("entrez_uid"))
  expect_that(length(b[1:2]), equals(2))
})

test_that("'querykey', 'webenv', and 'database' return the appropriate results", {
  expect_equal(querykey(a), 1)
  expect_match(webenv(a), "NCID_+")
  expect_equal(database(a), "pubmed")
  
  expect_equal(querykey(b), NA_integer_)
  expect_equal(webenv(b), NA_character_)
  expect_equal(database(b), 'pubmed')
})

