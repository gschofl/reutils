
# Test einfo() -----------------------------------------------------------

context("Testing 'einfo()'")

if (getOption('reutils.test.remote')) {
  a <- einfo()
  b <- einfo(retmode = "json")
  c <- einfo(db = 'gene')
  
  test_that("einfo() returns an 'einfo' object", {
    expect_is(a, "einfo")
    expect_is(b, "einfo")
  })
  
  test_that("Subsetting an 'einfo' instance returns a character vector or a list", {
    expect_is(a[1:10], "character")
    expect_equal(length(a[1:10]), 10)
    expect_is(b[1], 'list')
    expect_equal(length(a[1]), 1)
  })
  
  test_that("'content()' returns character vector, XMLInternalDocument, or json", {
    expect_that(content(a, "text"), is_a("character"))
    expect_that(content(b, "text"), is_a("character"))
    expect_that(content(a, "xml"), is_a("XMLInternalDocument"))
    expect_that(content(b, "xml"), throws_error("Cannot return data of retmode.+"))
    expect_that(content(a, "json"), throws_error("Cannot return data of retmode.+"))
    expect_that(content(b, "json"), is_a("json"))
    expect_that(content(a, 'parsed'), is_a("character"))
    expect_that(content(b, 'parsed'), is_a("list"))
  })
  
  test_that("'content(x, \"parsed\")' returns a 'tbl_df'", {
    expect_that(content(c, "parsed")[["Fields"]], is_a("tbl_df"))
    expect_that(content(c, "parsed")[["Links"]], is_a("tbl_df"))
  })
  
  test_that("'database()' returns NULL or 'gene'", {
    expect_that(database(a), equals(NULL))
    expect_that(database(c), equals("gene"))
  })
  
  test_that("'getUrl()' returns the query url", {
    expect_match(getUrl(a), "^https://eutils\\.ncbi\\.nlm\\.nih\\.gov/entrez/eutils/einfo.+")
    expect_match(getUrl(b), "^https://eutils\\.ncbi\\.nlm\\.nih\\.gov/entrez/eutils/einfo\\.fcgi\\?retmode=json.+")
    expect_match(getUrl(c),  "^https://eutils\\.ncbi\\.nlm\\.nih\\.gov/entrez/eutils/einfo\\.fcgi\\?db=gene.+")
  })
  
  test_that("The accessor 'getError()' returns an 'eutil_error' object", {
    expect_that(getError(a), is_a("eutil_error"))
    expect_that(getError(b), is_a("eutil_error"))
  })
  
}
