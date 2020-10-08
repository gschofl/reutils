
# Test esummary() ---------------------------------------------------------

context("Testing 'esummary()'")

if (getOption('reutils.test.remote')) {

  x <- esearch(term = "Chlamydia psittaci", db = "nuccore", retmax = 2)
  a <- esummary(x)
  
  test_that("esummary() returns an 'esummary' object", {
    expect_is(a, "esummary")
  })
  
  test_that("'retmode' returns 'xml'", {
    expect_that(retmode(a), equals("xml"))
  })
  
  test_that("'content()' returns an XMLInternalDocument or data.fame", {
    expect_that(content(a), is_a("XMLInternalDocument"))
    expect_that(content(a, 'json'), throws_error("Cannot return data of retmode.+"))
    expect_that(content(a, 'parsed'), is_a("tbl_df"))
  })
  
  test_that("'summary(..)' returns the correct number of elements", {
    expect_equal(NROW(content(esummary("929249993", "nuccore"), "parsed")), 1)
    expect_equal(NROW(content(esummary(c("929249993", "929249993"), "nuccore"), "parsed")), 2)
  })
  
  test_that("'summary(post(...))' returns the correct number of elements", {
    expect_equal(NROW(content(esummary(epost("929249993", "nuccore")), "parsed")), 1)
    expect_equal(NROW(content(esummary(epost(c("929249993", "929249993"), "nuccore")), "parsed")), 2)
    expect_equal(NROW(content(esummary(epost(c("929249993", "929249993"), "nuccore"), retstart = 2), "parsed")), 1)
  })
}
