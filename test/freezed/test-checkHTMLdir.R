### Project:     statcheck traineeship
### Object:      Testing local checkHTLMdir function against stable (CRAN) version
### Description: This script tests whether the extractions done using the stable 
###              version of checkHTMLdir (on CRAN) and the version on which you are
###              working on (local) are giving the same results when applied to all
###              the html articles contained in a defined folder.
###              The criteria of comparison are: 1) number of extractions, 
###                                              2) number of errors, 
###                                              3) number of decision errors
### Requirements: (1) having a folder full of articles
### How to use:  - set path to local statcheck and htmlImport scripts (containing 
###                the versions of the function you are working on)
###              - set path to folder containing all reals test articles
###              - run the suite.R script which is contained in the same folder as 
###                this script
### Output: If the number of extractions are the same the output reports: ok
###         if they do not, the difference between them is reported; positive difference
###         means that the tested version (i.e. local) reports more than benchmark does

context('HTML IMPORT: checkHTLMdir output local = CRAN')

# Set Up ------------------------------------------------------------------

  # # Use the following if you want to run this test independently
  #   setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")
  # # packages
  #   library(testthat)
  #   library(plyr) # for function ddply (need for the auto_test feature)
  # # Source Functions to be tested (local versions, not from package)
     source("../R/statcheck.R")
     source("../R/htmlImport.R")

# Locations: specify the test article location

  x <- "./articles/articles_HTML_PDF/"

# Tests Specification -----------------------------------------------------

# Get output from working and stable function
  quiet(working_out <- checkHTMLdir(x))            # output from working version
  quiet(stable_out  <- statcheck::checkHTMLdir(x)) # output from stable version

#> Number of extractions     ####
  
  tocheck   <- nrow(working_out)
  benchmark <- nrow(stable_out)
  
  test_that('TEST: Number of extractions', {
    expect_equal(tocheck, benchmark)
  })
  
#> Number of Errors          ####
  
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
