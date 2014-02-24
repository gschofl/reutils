
# Test esummary() ---------------------------------------------------------

context("Testing 'esummary()'")

if (getOption('reutils.test.remote')) {
  x <- esearch(term="Chlamydia psittaci", db="nuccore", retmax=2)
  a <- esummary(x)
  
  test_that("esummary() returns an 'esummary' object", {
    expect_is(a, "esummary")
  })
  
  test_that("'content()' returns a data.fame", {
    expect_that(content(a, 'parsed'), is_a("data.frame"))
  })
}
