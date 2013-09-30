
# Test einfo() -----------------------------------------------------------

context("Testing 'einfo()'")

a <- einfo()
b <- einfo("gene")

test_that("einfo() returns an 'einfo' object", {
  expect_is(a, "einfo")
  expect_is(b, "einfo")
})

test_that("Subsetting an 'einfo' instance returns a character vector or a list", {
  expect_is(a[1:10], "character")
  expect_equal(length(a[1:10]), 10)
  expect_is(b[1:5], 'list')
  expect_equal(length(a[1:5]), 5)
})

test_that("'content()' returns a character vector or an XMLInternalDocument", {
  expect_that(content(a, "text"), is_a("character"))
  expect_that(content(b, "text"), is_a("character"))
  expect_that(content(a, "xml"), is_a("XMLInternalDocument"))
  expect_that(content(b, "xml"), is_a("XMLInternalDocument"))
  expect_that(content(a, 'parsed'), is_a("character"))
  expect_that(content(b, 'parsed'), is_a("list"))
})

test_that("'database()' returns a NULL or 'gene'", {
  expect_that(database(a), equals(NULL))
  expect_that(database(b), equals("gene"))
})

test_that("'getUrl()' returns the query url", {
  expect_that(getUrl(a), 
              matches("^http://eutils\\.ncbi\\.nlm\\.nih\\.gov/entrez/eutils/einfo.+"))
  expect_that(getUrl(b), 
              matches("^http://eutils\\.ncbi\\.nlm\\.nih\\.gov/entrez/eutils/einfo\\.fcgi\\?db=gene.+"))
})

test_that("The accessor 'getError()' returns an 'eutil_error' object", {
  expect_that(getError(a), is_a("eutil_error"))
  expect_that(getError(b), is_a("eutil_error"))
})
