### Project:     statcheck traineeship
### Object:      checkPDF function test
### Description: This script tests whether the extractions done using the stable version of checkPDF (on CRAN) and
###              the version on which you are working on are giving the same results when applied to a dummy article
###              containing a series of test of interest.
###              The criteria of comparison are: number of extractions, number of errors, number of decision errors
### Requirements: (1) having a dummy article; (2) specify a path to it (object x below)
### How to use:  - set path to local statcheck and PDFimport scripts (containing the versions of the function you are working on)
###              - set path to dummy article
###              - run the suite.R script which is contained in the same folder as this script
### Date:        2019-03-20

context('PDF IMPORT: checkPDF (test w/ CRAN ref)')

#setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")
  # because of the way autotest work, I assume the folder is
  # test, where the auto_test script is located (can be deleted,
  # helpful only for writing the script)

# Set up (after context)
  #library(ddpcr)
  #library(stringi) # need for the getPDF function
  #library(plyr)
  source("../R/statcheck.R")
  source("../R/PDFimport.R")

# Define test texts -------------------------------------------------------
  # Define path to example pdf
    x <- "../extra/articles/PDF-AAA-Tri.pdf"
  # Get output from working and stable function
    quiet(working_out <- checkPDF(x))           # output from working version
    quiet(stable_out <- statcheck::checkPDF(x)) # output from stable version

# Define tests of interest ------------------------------------------------
  #> Number of extractions ####
  tocheck   <- nrow(working_out)
  benchmark <- nrow(stable_out)
  
  #tocheck <- 2
  test_that('TEST: # of extractions', {
    expect_equal(tocheck, benchmark)
  })
  
  #> Number of Errors ####
  tocheck   <- sum(na.omit(working_out$Error))
  benchmark <- sum(na.omit(stable_out$Error))

  test_that('TEST: # of errors', {
    expect_equal(tocheck, benchmark)
  })
  
  #> Number of Decision Errors ####
  tocheck   <- sum(na.omit(working_out$DecisionError))
  benchmark <- sum(na.omit(stable_out$DecisionError))
  
  test_that('TEST: # of decision errors', {
    expect_equal(tocheck, benchmark)
  })
