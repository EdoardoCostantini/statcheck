# Test Suite
library(testthat)
# Check basics (extractions, )
test_file("PDFimport-test.R")
# 16 more extractions than in reference
# -1 # of errors: statcheck misses 1 error
# -1 # of decision errors: statcheck misses 1 error
auto_test("/Users/Edoardo/DriveUni/gh-statcheck/R","/Users/Edoardo/DriveUni/gh-statcheck/test")