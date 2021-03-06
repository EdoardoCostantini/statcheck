### Project:     statcheck traineeship
### Object:      Testing local checkPDF function against stable (CRAN) version
### Description: This script tests whether the extractions done using the stable 
###              version of checkPDF (on CRAN) and the version on which you are
###              working on (local) are giving the same results when applied to 
###              a dummy PDF article containing a series of test of interest.
###              The criteria of comparison are: 1) number of extractions, 
###                                              2) number of errors, 
###                                              3) number of decision errors
### Requirements: (1) having a dummy PDF article;
### How to use:  - set path to local statcheck and PDFimport scripts (containing 
###                the versions of the functions you are working on)
###              - set path to dummy article
###              - run the suite.R script which is contained in the same folder as this script
### Output: If the number of extractions are the same the output reports: ok
###         if they do not, the difference between them is reported; positive difference
###         means that the tested version (i.e. local) reports more than benchmark does

context('PDF IMPORT: checkPDF output local = CRAN')

# Set Up ------------------------------------------------------------------

  # # Use the following if you want to run this test independently
  #   setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")
  # # packages
  #   library(testthat)
  #   library(stringi) # need for the getPDF function
  #   library(plyr) # for function ddply (need for the auto_test feature)
  # # Source Functions to be tested (local versions, not from package)
     source("../R/statcheck.R")
     source("../R/PDFimport.R")
     source("../R/pdf_columns.R") # used by getPDF inside PDFimport script

# Locations: specify the test article location

  x <- "./articles/articles_PDF_trial/TrialArticle.pdf"

# Get output from working and stable function
  quiet(working_out <- checkPDF(x))           # output from working version
  quiet(stable_out <- statcheck::checkPDF(x)) # output from stable version
  
# Tests Specification -----------------------------------------------------
  
#> Number of extractions     ####
  
  nextra <- nrow(working_out)        # number of extractions
    if( is.null( nextra ) == TRUE ){ # if statcheck does not find any statistic, returns a 0
      tocheck <- 0
    } else {
      tocheck <- nextra
    }

  benchmark <- nrow(stable_out)
  
  test_that('TEST: Number of extractions', {
    expect_equal(tocheck, benchmark)
  })
  
#> Number of Errors          ####
  
  nerror <- sum(na.omit(working_out$Error))        # number of extractions
    if( is.null( nerror ) == TRUE ){               # if statcheck does not find any statistic, returns a 0
      tocheck <- 0
    } else {
      tocheck <- nerror
    }
  benchmark <- sum(na.omit(stable_out$Error))

  test_that('TEST: Number of errors', {
    expect_equal(tocheck, benchmark)
  })
  
#> Number of Decision Errors ####

  nerror <- sum(na.omit(working_out$DecisionError))# number of extractions
    if( is.null( nerror ) == TRUE ){               # if statcheck does not find any statistic, returns a 0
      tocheck <- 0
    } else {
      tocheck <- nerror
    } 
  benchmark <- sum(na.omit(stable_out$DecisionError))
  
  test_that('TEST: Number of decision errors', {
    expect_equal(tocheck, benchmark)
  })
