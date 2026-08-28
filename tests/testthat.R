# R CMD check entry point for the testthat suite.
#
# This file did not exist, so `R CMD check` ran no tests at all -- the cotest suite alone is
# 175 assertions that were only ever executed by hand via devtools::test() or test_file().
library(testthat)
library(ClinicoPath)

test_check("ClinicoPath")
