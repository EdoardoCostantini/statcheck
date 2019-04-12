### Project:     statcheck traineeship
### Object:      checkPDFdir function test
### Description: This script contains some working scripts to test whether the extraction perfomrances
###              of different getPDF functions when applying statcheck function to their output.
###              The criteria of comparison are: number of extractions, number of errors, number of decision errors.
### Requirements: (1) having a folder full of articles; (2) specifying a path to it (object 'x' below)
### (3) having statcheck, PDFimport, and pdf_columns
### How to use:  - set path to local statcheck and htmlImport scripts (containing the versions of the function you are working on)
###              - set path to folder containing all reals test articles
###              - run the suite.R script which is contained in the same folder as this script
### Date:        2019-03-20

context('PDF IMPORT: checkPDFdir (test w/ CRAN ref)')

#setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")
  # because of the way autotest work, I assume the folder is
  # test, where the auto_test script is located (can be deleted,
  # helpful only for writing the script)

# Set up (after context)
  #library(ddpcr)
  library(plyr)
  library(stringi) # need for the getPDF function
  source("../R/statcheck.R")
  source("../R/PDFimport.R")

# Define test texts -------------------------------------------------------
  # Define path to example pdf
    x <- "../extra/articles/"
  # Get output from working and stable function
    quiet(working_out <- checkPDFdir(x))            # output from working version
    quiet(stable_out  <- statcheck::checkPDFdir(x)) # output from stable version
    
# Define tests of interest ------------------------------------------------
  #> Number of extractions ####
  tocheck   <- nrow(working_out)
  benchmark <- nrow(stable_out)
  
  #tocheck <- 2
  test_that('TEST: Number of extractions', {
    expect_equal(tocheck, benchmark)
  })
  
  #> Number of Errors ####
  tocheck   <- sum(na.omit(working_out$Error))
  benchmark <- sum(na.omit(stable_out$Error))

  test_that('TEST: Number of errors', {
    expect_equal(tocheck, benchmark)
  })
  
  #> Number of Decision Errors ####
  tocheck   <- sum(na.omit(working_out$DecisionError))
  benchmark <- sum(na.omit(stable_out$DecisionError))
  
  test_that('TEST: Number of decision errors', {
    expect_equal(tocheck, benchmark)
  })
