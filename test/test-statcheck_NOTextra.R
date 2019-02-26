# Testing statcheck function

context('STATCHEK: should NOT extract')

#setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")
  # because of the way autotest work, I assume the folder is
  # test, where the auto_test script is located (can be deleted,
  # helpful only for writing the script)

# Set up (after context)
  library(plyr)
  source("../R/statcheck.R")

# Define test texts ####
  #> Invalid p-values ####
    txt_invP <- paste("Correct way of reporting t test: t(48) = 1.02, p = 1",
                      "Correct way of reporting t test: t(48) = 1.02, p = 1.01",
                      "Correct way of reporting t test: t(48) = 1.02, p > 1",
                      "Correct way of reporting t test: t(48) = 1.02, p < 0",
                      "Correct way of reporting t test: t(48) = 1.02, p = -.01"
    )
    # you may simply add any other invalid pvalue you want to keep track of
    #statcheck(txt_invP)
    
  #> Non-apa style: semi-colon instead of comma ####
    txt_semico <- paste("Correct way of reporting t test: t(48) = 1.02; p = .3128421")
    #statcheck(txt_testsign)
    
  #> Non-apa style: brakets (squraed instead of round) ####
    txt_brakets <- paste("Correct way of reporting t test: t[48] = 1.02; p = .3128421")
    #statcheck(txt_brakets)
    
# Define tests of interest ####
  #> Invalid p-values ####
    quiet( tocheck   <- is.null(statcheck(txt_invP)) ) # num of tests statcheck able to extract
    quiet( benchmark <- is.null(statcheck::statcheck(txt_invP)) ) # define benchmark based on how many tests whould be read 
                     # (defined number of recognized tests by stable version of
                     # statcheck applied to txt_spaces object)
    #tocheck <- FALSE
    test_that("TEST: Invalid p-values", {
      expect_equal(tocheck, benchmark)
    })
    
  #> Non-APA: semi-colon instead of comma ####
    quiet( tocheck   <- is.null(statcheck(txt_semico)) ) # num of tests statcheck able to extract
    quiet( benchmark <- is.null(statcheck::statcheck(txt_semico)) ) # define benchmark based on how many tests whould be read 
    test_that('TEST: Non-APA: semi-colon instead of comma', {
      expect_equal(tocheck, benchmark)
    })
  
  #> Non-APA: brakets (squraed instead of round) ####
    quiet( tocheck   <- is.null(statcheck(txt_brakets)) ) # num of tests statcheck able to extract
    quiet( benchmark <- is.null(statcheck::statcheck(txt_brakets)) ) # define benchmark based on how many tests whould be read 
    test_that('TEST: Non-APA: brakets (squraed instead of round)', {
      expect_equal(tocheck, benchmark)
    })