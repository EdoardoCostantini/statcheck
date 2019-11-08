### Project:     statcheck traineeship
### Object:      Testing local statcheck DOES NOT reads what it SHOULD NOT
### Description: This script tests whether the local version of statcheck does NOT
###              read statistics reported in ways we do not want to read.
###              The unwanted results controlled: 1) invalid pvalues, 
###                                               2) semi-colon instead of comma, 
###                                               3) squared brakets instead of round
###              any other result of interest can easly be added with another test
### Requirements: (1) local statcheck function (the one you are working on);
### How to use:  - set path to local statcheck
###              - run the suite.R script which is contained in the same folder as this script
### Output: If the updated function works as expected reports: ok
###         if not, reports the number of things that are done wrong

context('STATCHEK: do NOT extract')

# Set Up ------------------------------------------------------------------

  # # Use the following if you want to run this test independently
  #   setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")
  # # packages
  #   library(testthat)
  #   library(plyr) # for function ddply (need for the auto_test feature)
  # # Source Functions to be tested (local versions, not from package)
     source("../R/statcheck.R")

# Tests Specification -----------------------------------------------------

# benchmark
  benchmark <- 0                               # same for every test in this context (will not repeat definition)
    
#> Invalid p-values                            ####
        
  # Text Strings
    txt_invP <- c("A test that should not be read: t(48) = 1.02, p = 1",
                  "A test that should not be read: t(48) = 1.02, p = 1.01",
                  "A test that should not be read: t(48) = 1.02, p > 1",
                  "A test that should not be read: t(48) = 1.02, p < 0",
                  #"Correct way of reporting t test: t(48) = 1.02, p = .05", #use to check what happens if a "mistake" is found
                  "A test that should not be read: t(48) = 1.02, p = -.01"
    )
    # you may simply add any other invalid pvalue you want to keep track of
    
  # tocheck
    quiet( nextra <- nrow(statcheck(txt_invP)) ) # number of extractions
    if( is.null( nextra ) == TRUE ){             # if statcheck does not find any statistic, returns a 0
      tocheck <- 0
    } else {
      tocheck <- nrow(statcheck(txt_invP))
    }

    test_that("TEST: Invalid p-values", {
      expect_equal(tocheck, benchmark)
    })
    
#> Non-APA: semi-colon instead of comma        ####
    
  # Text Strings
    txt_semico <- c("Correct way of reporting t test: t(48) = 1.02; p = .3128421")
    
  # tocheck
    quiet( nextra <- nrow(statcheck(txt_semico)) ) # number of extractions
    if( is.null( nextra ) == TRUE ){               # if statcheck does not find any statistic, returns a 0
      tocheck <- 0
    } else {
      tocheck <- nrow(statcheck(txt_semico))
    }
    
  # Test
    test_that('TEST: Non-APA: semi-colon instead of comma', {
      expect_equal(tocheck, benchmark)
    })
  
#> Non-APA: brakets (squraed instead of round) ####

  # Text Strings
    txt_brakets <- c("Correct way of reporting t test: t[48] = 1.02; p = .3128421")

  # tocheck
    quiet( nextra <- nrow(statcheck(txt_brakets)) ) # number of extractions
    if( is.null( nextra ) == TRUE ){                # if statcheck does not find any statistic, returns a 0
      tocheck <- 0
    } else {
      tocheck <- nrow(statcheck(txt_brakets))
    }
    
  # Test
    test_that('TEST: Non-APA: brakets (squraed instead of round)', {
      expect_equal(tocheck, benchmark)
    })

    
