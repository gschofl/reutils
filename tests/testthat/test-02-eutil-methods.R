context("Test eutil methods")

test_that("The methods we want to test are all present in an eutil class", {
  expect_true(all(c("database", "eutil", "get_content", "get_error", "get_url", "perform_query", "no_errors", 
                    "retmode", "rettype",  "xmlAttr", "xmlName", "xmlSet", "xmlValue") %in% eutil$methods()))
})

## generate a test esearch instance.
## if not on CRAN go to NCBI, if on CRAN use locally stored instance.
if (getOption('reutils.test.remote')) {
  a <- .esearch('GET', db='pubmed', term='Chlamydia psittaci', retstart=6, retmax=2)
  save(a, file = "data/test-02-a.rda")
} else {
  load("data/test-02-a.rda")  
}

test_that("#database works", {
  expect_equal(a$database(), "pubmed")
})

test_that("#eutil works", {
  expect_equivalent(a$eutil(), "esearch")
})

test_that("#get_content works", {
  expect_is(a$get_content("text"), "character")
  expect_is(a$get_content("xml"), "XMLInternalDocument")
  expect_is(a$get_content("parsed"), "entrez_uid")
  expect_is(a$get_content("parsed"), "character")
  expect_is(a$get_content("textConnection"), "textConnection")
  expect_error(a$get_content("bla"))
})

test_that("#get_error returns an 'eutil_error' object", {
  expect_that(a$get_error(), is_a("eutil_error"))
})

test_that("#get_url returns the query URL", {
  expect_match(a$get_url(), "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi\\?db=pubmed&term=Chlamydia%20psittaci&retstart=6&retmax=2.+")
})

test_that("#no_errors works as expected", {
  expect_true(a$no_errors())
})

test_that("'#xmlName() works", {
  tagnames <- a$xmlName('/eSearchResult/*')
  expect_is(tagnames, 'character')
  expect_equal(tagnames[1], 'Count')
})

test_that("'#xmlSet() works", {
  nodeset <- a$xmlSet('/eSearchResult/IdList')
  expect_is(nodeset, 'XMLNodeSet')
})

test_that("'#xmlValue works", {
  character.uids <- a$xmlValue('/eSearchResult/IdList/*')
  expect_is(character.uids, 'character')
  expect_equal(length(character.uids), 2)
  integer.uids <- a$xmlValue('/eSearchResult/IdList/*', as='integer')
  expect_is(integer.uids, 'integer')
  expect_equal(length(integer.uids), 2)
  expect_equal(a$xmlValue('/eSearchResult/Bla'), NA_character_)
  expect_equal(a$xmlValue('/eSearchResult/Bla', as='integer'), NA_integer_)
  expect_equal(a$xmlValue('/eSearchResult/Bla', default=NULL), NULL)
  expect_equal(a$xmlValue('/eSearchResult/Bla', default=NULL), NULL)
  expect_equal(a$xmlValue('/eSearchResult/Bla', default=''), '')
})

test_that("#perform_query updates an existing query", {
  if (getOption('reutils.test.remote')) {
    a$perform_query(retmax=10, usehistory="y")
    save(a, file = "data/test-02-a2.rda")
  } else {
    load("data/test-02-a2.rda")
  }  
  expect_match(webenv(a), "^NCID_\\d_\\d+_.+")
  expect_equal(querykey(a), 1)
})

test_that("#rettype and #retmode work", {
  if (getOption('reutils.test.remote')) {
    b <- efetch(uid="8655742", db="protein", rettype="fasta", retmode="xml")
    save(b, file = "data/test-02-b.rda")
  } else {
    load("data/test-02-b.rda")
  }
  expect_equal(b$rettype(), "fasta")
  expect_equal(b$retmode(), "xml")
})


