# Testing statcheck function

context('STATCHEK: should extract')

#setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")
  # because of the way autotest work, I assume the folder is
  # test, where the auto_test script is located (can be deleted,
  # helpful only for writing the script)

# Set up (after context)
  library(plyr)
  source("../R/statcheck.R")


# Define test texts -------------------------------------------------------
  #> Comparison sings ####
    txt_compsign <- paste("Correct way of reporting t test: t(48) = 1.02, p = .05",
                          "Correct way of reporting t test: t(48) = 1.02, p > .05.",
                          "Correct way of reporting t test: t(48) = 1.02, p < .05.")
    #statcheck(txt_compsign)
    
  #> Test sign ####
    txt_testsign <- paste("Correct way of reporting t test: t(48) = 1.02, p = .3128421",
                          "Correct way of reporting t test: t(48) = -1.02, p = .3128421")
    #statcheck(txt_testsign)
    
  #> Different caps ####
    # I could automate a bit the creation of the texts with some combining structure
    txt_caps <- paste("Correct way of reporting t test: t(48) = 1.02, p = .3128421",
                      "Correct way of reporting t test: t(48) = 1.02, P = .3128421",
                      "Correct way of reporting t test: T(48) = 1.02, p = .3128421",
                      "Correct way of reporting t test: T(48) = 1.02, P = .3128421",
                      "Correct way of reporting F test: F(3, 27) = 7.77, p = .000675864",
                      "Correct way of reporting F test: F(3, 27) = 7.77, P = .000675864",
                      "Correct way of reporting F test: f(3, 27) = 7.77, p = .000675864",
                      "Correct way of reporting F test: f(3, 27) = 7.77, P = .000675864",
                      "Correct way of reporting chi-sq test: X2 (2, N = 170) = 14.14, p = .0008502331",
                      "Correct way of reporting chi-sq test: X2 (2, N = 170) = 14.14, P = .0008502331",
                      "Correct way of reporting chi-sq test: x2 (2, N = 170) = 14.14, p = .0008502331",
                      "Correct way of reporting chi-sq test: x2 (2, N = 170) = 14.14, P = .0008502331")
    #statcheck(txt_caps)


# Define tests of interest ------------------------------------------------
  #> Different comparison sings (<, >, =) ####
  quiet( tocheck   <- nrow(statcheck(txt_compsign)) ) # num of tests statcheck able to extract
  quiet( benchmark <- nrow(statcheck::statcheck(txt_compsign)) ) # define benchmark based on how many tests whould be read 
                   # (defined number of recognized tests by stable version of
                   # statcheck applied to txt_spaces object)
  #tocheck <- 2
  test_that('TEST: Different Comparison Sings (<, >, =)', {
    expect_equal(tocheck, benchmark)
  })
  
  #> Positive/Negative Statistics ####
  quiet( tocheck   <- nrow(statcheck(txt_testsign)) ) # num of tests statcheck able to extract
  quiet( benchmark <- nrow(statcheck::statcheck(txt_testsign)) ) # define benchmark based on how many tests whould be read 
  test_that('TEST: Positive/Negative Statistics', {
    expect_equal(tocheck, benchmark)
  })
  
  #> Different Caps ####
  quiet( tocheck   <- nrow(statcheck(txt_caps)) ) # num of tests statcheck able to extract
  quiet( benchmark <- nrow(statcheck::statcheck(txt_caps)) ) # define benchmark based on how many tests whould be read 
  test_that('TEST: Different Caps', {
    expect_equal(tocheck, benchmark)
  })
  
  # Exactly reported p-values (?)
  # test_that('TEST: Exactly reported p-values', {
  #   expect_true(nrow(statcheck(txt_testsign)) == 2)
  # })
  