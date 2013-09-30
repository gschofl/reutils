library(testthat)
opts <- options("reutils.verbose.queries"=FALSE)
test_check("reutils")
options(opts)