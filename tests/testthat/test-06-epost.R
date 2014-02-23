
# Test epost() -----------------------------------------------------------

context("Testing 'epost()'")

if (getOption('reutils.test.remote')) {
  uids <- esearch("Chlamydia", "bioproject", retmax=20)
  save(uids, file = "data/test-06-uids.rda")
} else {
  load("data/test-06-uids.rda")
}

test_that("epost() works with 'esearch' objects", {
  if (getOption('reutils.test.remote')) {
    p1 <- epost(uids)
    save(p1, file = "data/test-06-p1.rda")
  } else {
    load("data/test-06-p1.rda")
  }
  expect_is(p1, 'epost')
  expect_match(webenv(p1), "NCID_.+")
  expect_equal(querykey(p1), 1)
})

test_that("epost() works with 'entrez_uid' objects", {
  if (getOption('reutils.test.remote')) {
    p2 <- epost(content(uids, 'parsed'))
    save(p2, file = "data/test-06-p2.rda")
  } else {
    load("data/test-06-p2.rda")
  }
  expect_is(p2, 'epost')
  expect_match(webenv(p2), "NCID_.+")
  expect_equal(querykey(p2), 1)
})

test_that("epost() works with character/numeric vectors", {
  if (getOption('reutils.test.remote')) {
    p3 <- epost(c("194680922","50978626","28558982","9507199","6678417"), "protein")
    save(p3, file = "data/test-06-p3.rda")
  } else {
    load("data/test-06-p3.rda")
  }
  expect_is(p3, 'epost')
  expect_match(webenv(p3), "NCID_.+")
  expect_equal(querykey(p3), 1)
})

