### Project:     statcheck traineeship
### Object:      Testing local checkHTLMdir function against manually coded reference
### Description: This script tests whether the extractions obtained using the local 
###              version of checkHTMLdir on a trial article and what is reported by
###              a manual check file (.csv) correspond
###              The criteria of comparison are: 1) number of extractions, 
###                                              2) number of errors, 
###                                              3) number of decision errors
### Requirements: (1) having a folder filled with HTML articles
###               (2) .csv file listing its test statistics in the same format as a
###                 statcheck output;
### How to use:  - set path to local statcheck and htmlImport scripts (containing the 
###                versions of the function you are working on)
###              - set path to html article folder
###              - run the suite.R script which is contained in the same folder as this script
### Output: If the number of extractions are the same the output reports: ok
###         if they do not, the difference between them is reported; positive difference
###         means that the tested version (i.e. local) reports more than benchmark does

context('HTML IMPORT: checkHTLMdir output local = manual reference')

# Set Up ------------------------------------------------------------------

  # # Use the following if you want to run this test independently
  #   setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")
  # # packages
  #   library(testthat)
  #   library(plyr) # for function ddply (need for the auto_test feature)
  # # Source Functions to be tested (local versions, not from package)
  #   source("../R/statcheck.R")
  #   source("../R/htmlImport.R")

# Locations: specify the articles folder and articles_manually_coded_coded.csv file location

  locationObj <- list(artdir = "./articles/articles_HTMLdir_csv",
                      # The user specifies the directory containing all the articles of 
                      # interest in HTML format.
                      # RELATIVE PATH: running the autotest will set the wd to the directory
                      # where the script containing the function auto_test is. To specify relative
                      # path keep this in mind.
                      
                      reffile = "./articles/articles_HTMLdir_csv/manuallycoded.csv",
                      # The user specifies the path to the file (txt for now) that 
                      # reports the manually coded results.
                      # Can give both txt or csv (not csv2)
                      
                      erros_indx = c("plos.Error", "plos.DecisionError"))
                      # The user specifies the names (or numbers) of two columns:
                      # - one containing the logical variable identifying the the error status
                      # - one the decision error status. 
                      # Format for column number specification: erros_indx = c(10, 11)
  attach(locationObj)

# Statcheck options
  
  statOpt <- list(stat           = c("t", "F", "cor", "chisq", "Z", "Q"),
                  OneTailedTests = FALSE,
                  alpha          = 0.05,
                  pEqualAlphaSig = TRUE,
                  pZeroError     = TRUE,
                  OneTailedTxt   = FALSE, 
                  AllPValues     = FALSE)
    # I want the user to be concious at first glance of what is the set up in terms
    # of options tested with a specific test-script
  attach(statOpt)
  

# Import reference data (e.g. manually coded plos)
  if(grepl(".csv", reffile) == TRUE){
    ref_input <- read.csv(reffile, header = TRUE)
  } else {
    ref_input <- read.table(reffile, header = TRUE)
  }
  
  ref_input <- ref_input[, c(erros_indx[1], erros_indx[2])]
    # allows to specify the columns containing error and decision erros
    # both as character (names of columns) and as number (e.g. 3rd and 4th columns)
  
# Get statcheck output for the selected articles
  sttchckOutput <- checkHTMLdir(artdir, stat = stat,
                                OneTailedTests = OneTailedTests, 
                                alpha = alpha, 
                                pEqualAlphaSig = pEqualAlphaSig, 
                                pZeroError = pZeroError,
                                OneTailedTxt = OneTailedTxt, 
                                AllPValues = AllPValues)
    # Arguments will be set at the beginning of the script
    # Such arguments will define the way the user wants to
    # use statcheck for a specific testing purpose
  
# Tests Specification -----------------------------------------------------

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

#> Number of decision errors ####
  
  # benchmark and tocheck
  benchmark <- sum(na.omit(ref_input[, 2])) # reference number of decision errors
  tocheck   <- sum(sttchckOutput$DecisionError)
  
  # test
  test_that('TEST: Number of decision errors', {
    expect_equal(tocheck, benchmark)
  })
    