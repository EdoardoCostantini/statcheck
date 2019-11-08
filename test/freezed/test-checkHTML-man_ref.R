### Project:     statcheck traineeship
### Object:      Testing local checkHTLM function against manually coded reference
### Description: This script tests whether the extractions obtained using the local 
###              version of checkHTML on a trial articleand what is reported by a 
###              manual check file (.csv) correspond
###              The criteria of comparison are: 1) number of extractions, 
###                                              2) number of errors, 
###                                              3) number of decision errors
### Requirements: (1) having a dummy HTML article
###               (2) .csv file listing its test statistics in the same format as a
###                 statcheck output;
### How to use:  - set path to local statcheck and htmlImport scripts (containing the 
###                versions of the function you are working on)
###              - set path to dummy article
###              - run the suite.R script which is contained in the same folder as this script
### Date:        2019-03-20

context('HTML IMPORT: checkHTLM output local = manual reference') # MUST BE first line of code

# Set Up ------------------------------------------------------------------

  # # Use the following if you want to run this test independently
  #   setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")
  # # packages
  #   library(testthat)
  #   library(plyr) # for function ddply (need for the auto_test feature)
  # # Source Functions to be tested (local versions, not from package)
  #   source("../R/statcheck.R")
  #   source("../R/htmlImport.R")

# Locations: specify the test article and test_article_manually_coded_coded.csv file location

  locationObj <- list(reffile_csv  = "./articles/articles_HTML_csv/TrialArt.csv",   # file containing manually coded pvalues for 1 trial article
                      reffile_html = "./articles/articles_HTML_csv/TrialArt.html",  # trial article in HTML format
                      erros_indx   = c("Error", "DecisionError"))                 # specify the column names used in the csv file for error 
                                                                                  # and decision error (can be numeric for column numbers)
  attach(locationObj)

# Statcheck options: specify the statcheck options you want to use

  statOpt <- list(stat           = c("t", "F", "cor", "chisq", "Z", "Q"),
                  OneTailedTests = FALSE,
                  alpha          = 0.05,
                  pEqualAlphaSig = TRUE,
                  pZeroError     = TRUE,
                  OneTailedTxt   = FALSE, 
                  AllPValues     = FALSE)
  attach(statOpt)
  
# Import reference data (e.g. manually coded plos)

  if(grepl(".csv", reffile_csv) == TRUE){
    ref_input <- read.csv(reffile_csv, header = TRUE)
  } else {
    ref_input <- read.table(reffile_csv, header = TRUE)
  }
  
  ref_input <- ref_input[, c(erros_indx[1], erros_indx[2])]
    # allows to specify the columns containing error and decision erros
    # both as character (names of columns) and as number (e.g. 3rd and 4th columns)
    # based on what specified in the locationObj

# Tests Specification -----------------------------------------------------

# Get statcheck output for the selected articles
  sttchckOutput <- checkHTML(reffile_html, 
                             stat = stat,
                             OneTailedTests = OneTailedTests, 
                             alpha = alpha, 
                             pEqualAlphaSig = pEqualAlphaSig, 
                             pZeroError = pZeroError,
                             OneTailedTxt = OneTailedTxt, 
                             AllPValues = AllPValues)
    # The functio used here is the one you are working on
    # in the local htmlImport.R script, not the one used
    # by statcheck package.
    # Arguments will be set at the beginning of the script
    # Such arguments will define the way the user wants to
    # use statcheck for a specific testing purpose

#> Number of extractions     ####
  
  # benchmark and tocheck (aka actual extractions)
  benchmark <- nrow(ref_input)              # reference number of extractions     
  tocheck   <- nrow(sttchckOutput)
  
  # test
  test_that('TEST: Number of extractions', {
    expect_equal(tocheck, benchmark)
  })

#> Number of Errors          ####

  # benchmark and tocheck (aka actual extractions)
  benchmark <- sum(na.omit(ref_input[, 1])) # reference number of erros
  tocheck   <- sum(sttchckOutput$Error)
  
  # test
  test_that('TEST: Number of errors', {
    expect_equal(tocheck, benchmark)
  })

#> Number of Decision Errors ####
  
  # benchmark and tocheck
  benchmark <- sum(na.omit(ref_input[, 2])) # reference number of decision errors
  tocheck   <- sum(sttchckOutput$DecisionError)
  
  # test
  test_that('TEST: Number of decision errors', {
    expect_equal(tocheck, benchmark)
  })
    
  