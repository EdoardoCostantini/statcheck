# Testing statcheck function
context('STATCHEK: different spacings')
#setwd("/Users/Edoardo/DriveUni/gh-statcheck/test")
  # because of the way autotest work, I assume the folder is
  # test, where the auto_test script is located (can be deleted,
  # helpful only for writing the script)
# Set up (after context)
  library(plyr)
  library(ddpcr)
  source("../R/statcheck.R")

# Define test texts ####
# Different spacings conditon definition
# 1. Generate a spacecomb dataframe containing 3 possible "space" condtions: 0, 1, and 2
#    spaces for each "space location"
  nplaces <- 7 # how many space location could go wrong?
  space_loc <- vector("list", nplaces)
  for (s in 1:nplaces) {
    space_loc[[s]] <- c(" ", "", "  ")
  }
  spacecomb <- expand.grid(space_loc)
    colnames(spacecomb) <- paste0("space_loc", seq(1, nplaces))

# 2. Generate tests strings with all possible combiantions for the space conditions in the 6
#    different locations
  txt_spaces <- data.frame(String = rep(NA, nrow(spacecomb)),
                           Extc_wrkng = factor(rep(NA, nrow(spacecomb)), levels = c(0, 1), labels = c("N", "Y")),
                           Extc_stbl = factor(rep(NA, nrow(spacecomb)), levels = c(0, 1), labels = c("N", "Y"))
                           )
  # This object contains the test string (first column),
  head(txt_spaces)
  begin <- Sys.time()
  for (comb in 1:nrow(spacecomb)) {
    # correct: t(48) = 1.02, p = .3128421
    # format:  [space1]t(48)[space2]=[space3]1.02[space4],[space5]p[space6]=[space7].3128421
    # the paste function puts toghether the text according to the 
    # format specified.
    txt_spaces[comb, 1] <- paste0("Correct spacing test:", 
                                  spacecomb[comb, 1], "t(48)",
                                  spacecomb[comb, 2], "=",
                                  spacecomb[comb, 3], "1.02",
                                  spacecomb[comb, 4], ",",
                                  spacecomb[comb, 5], "p",
                                  spacecomb[comb, 6], "=",
                                  spacecomb[comb, 7], ".3128421")
    # Check extraction
    # > Using work-in-progress statcheck function (from source location)
    quiet(
      if(is.null( nrow(statcheck(txt_spaces[comb, 1])) ) == FALSE) {
        txt_spaces[comb, 2] <- "Y"    # did extract a test
      } else {
        txt_spaces[comb, 2] <- "N"    # did NOT extract a test
      })
    # > Using stable statcheck function (from statcheck package)
    quiet(
      if(is.null( nrow(statcheck::statcheck(txt_spaces[comb, 1])) ) == FALSE) {
        txt_spaces[comb, 3] <- "Y"
      } else {
        txt_spaces[comb, 3] <- "N"
      }
    )
  }
  end <- Sys.time()
  
  end-begin 
   # for 7 spaces option: 30 secs
   # for full 10 spaces option: 11 minutes (including 3 more spaces for "t(48)")
  
# Define tests of interest ####
  tocheck   <- sum(txt_spaces[, 2] == "Y") # num of tests statcheck working was able to extract
  benchmark <- sum(txt_spaces[, 3] == "Y") # num of tests statcheck stable was able to extract
  #tocheck <- 287 # to test discordant reading
  
  test_that('TEST: different spacing', {
    expect_equal(tocheck, benchmark)
  })
  # It counts the number of recognized tests.
  # When negative, you changed something and less things are recognized
  # When postive, you imporved your code, more combinations of spaces are recognized than in benchmark
  
# Diagnostic ####
  tocheck <- txt_spaces[, 2]
  benchmark <- txt_spaces[, 3]
  # This code shows the cases for which there differences (if any)
  txt_spaces[which(tocheck != benchmark), 1] # if there are mismathces, this will show which strings
                                             # are causing problems
  # This code shows what is read and what's not
  txt_spaces[which(tocheck=="Y"), 1] #readable strings
  txt_spaces[which(tocheck=="N"), 1] #unreadable strings
