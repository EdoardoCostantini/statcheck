### Project:     statcheck traineeship
### Object:      Testing local checkPDFdir function against checkPDFdir CRAN
###              and checkHTMLdir CRAN
### Description: This script tests whether the extractions done using the 
###              checkPDFdir (local and CRAN) function are giving the same
###              results when applied to all the articles (in appropriate 
###              file format) contained in a defined folder.
###              For both the local and CRAN checkPDFdir differences are
###              computed comparing to the extractions obtained with the 
###              checkHTMLdir function (generally "perfect" performance).
###              I call these differences delta_update and delta_CRAN 
###              respectively and these are the values I use for the tests.
###              The criteria of comparison are: 1) number of extractions, 
###                                              2) number of errors, 
###                                              3) number of decision errors
### Requirements: (1) folder with same articles in pdf and html format;
### How to use:  - set path to local statcheck and htmlImport scripts (locals)
###              - set path to folder containing all reals test articles
###              - run the suite.R script which is contained in the same folder as this script
### Output: HTML will probably always report more statistics. Test will never give ok
###         The output therefore reports how many statistics/errors/decision errors 
###         the local and CRAN versions of checkPDFdir miss compared to the checkHTML
###         The difference between the missed statistics by the checkPDF functions is reported
###         A negative difference means that the local version misses LESS statistics
###         than the CRAN one.

context('PERFORMANCE: checkPDFdir local vs checkPDFdir CRAN vs checkHTMLdir CRAN')

# Set Up ------------------------------------------------------------------

  # # Use the following if you want to run this test independently
  #   setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")
  # # packages
  #   library(testthat)
  #   library(stringi) # need for the getPDF function
  #   library(plyr) # for function ddply (need for the auto_test feature)
  # # Source Functions to be tested (local versions, not from package)
  #   source("../R/statcheck.R")
  #   source("../R/PDFimport.R")
  #   source("../R/pdf_columns.R") # used by getPDF inside PDFimport script

# Locations: specify the test article location

  x <- "./articles/articles_HTML_PDF/"
  
# Get output from working and stable function
  quiet(working_out <- checkPDFdir(x))            # output from working version
  quiet(stable_out  <- statcheck::checkPDFdir(x)) # output from stable version
    
# Performance Checks ------------------------------------------------------

# Get output from working and stable function
  quiet(working_out_up <- checkPDFdir(x))             # output from working version
  quiet(working_out_CR <- statcheck::checkPDFdir(x))  # output from working version
  quiet(stable_out     <- statcheck::checkHTMLdir(x)) # output from stable version

#> Number of extractions    ####
  
  tocheck_update <- nrow(working_out_up)
  tocheck_CRAN   <- nrow(working_out_CR)
  benchmark      <- nrow(stable_out)
  delta_update <- benchmark - tocheck_update
  delta_CRAN   <- benchmark - tocheck_CRAN
  
  test_that('TEST: # of extractions by checkHTMLdir CRAN minus checkPDFdir local or CRAN', {
    expect_equal(delta_update, delta_CRAN)
  })
  
#> Number of Errors          ####
  
  tocheck_update <- sum(na.omit(working_out_up$Error))
  tocheck_CRAN   <- sum(na.omit(working_out_CR$Error))
  benchmark      <- sum(na.omit(stable_out$Error))
  delta_update <- benchmark - tocheck_update
  delta_CRAN   <- benchmark - tocheck_CRAN
  
  test_that('TEST: # of errors by checkHTMLdir CRAN minus checkPDFdir local or CRAN', {
    expect_equal(delta_update, delta_CRAN)
  })
  
#> Number of Decision Errors ####
  
  tocheck_update <- sum(na.omit(working_out_up$DecisionError))
  tocheck_CRAN   <- sum(na.omit(working_out_CR$DecisionError))
  benchmark      <- sum(na.omit(stable_out$DecisionError))
    delta_update <- benchmark - tocheck_update
  delta_CRAN   <- benchmark - tocheck_CRAN
  
  test_that('TEST: # of decision errors by checkHTMLdir CRAN minus checkPDFdir local or CRAN', {
    expect_equal(delta_update, delta_CRAN)
  })
  