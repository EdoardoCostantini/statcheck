# Testing PDFimport function

context('PDF IMPORT: checkPDF (test w/ CRAN ref)')

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
    x <- "../extra/articles/TrialArt.pdf"
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
