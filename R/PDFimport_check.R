### Project:     statcheck traineeship
### Object:      checkperformance of new function on many papers
### Description: This script tests whether the extractions done using the new version are in line with "reliability" studies.
###              Expect 10 % of extractions to be errors, expect 1.5% extractions to be gross errors (decision errors)
### Requirements: modified version of checkDIR with pdf_text function instead of call to pdftotext external app
### Date:        2019-04-10

# functions
  source("./R/statcheck.R")
  source("./R/PDFimport.R")
  library(pdftools) # for pdf_text function in the new getPDF function
  library(stringi)  # for encoding function in the new getPDF function
  library(plyr)     # for ddply function

# Location of articles to test (89 right now, across different pubblications and publishers)
  x <- "./extra/articles_consistency"

# Perform statcheck
  out_pdftools  <- checkPDFdir(x)
  out_tabulizer <- checkPDFdir_tabu(x)
  out_baseline  <- statcheck::checkPDFdir(x)
  
  out_tabulizer[which(is.na(out_tabulizer$Error)), ]
  
  paste0(round(mean(out_pdftools$Error)*100, 1), "% of the extractions are mistakenly reported")
  paste0(round(mean(na.omit(out_tabulizer$Error))*100, 1), "% of the extractions are mistakenly reported")
  #out_tabulizer[which(is.na(out_tabulizer$Error)), ] # ths guy is a missing, why? not sure
  paste0(round(mean(out_baseline$Error)*100, 1), "% of the extractions are mistakenly reported")
  
  round(mean(out_pdftools$DecisionError)*100, 1)
  round(mean(na.omit(out_tabulizer$DecisionError))*100, 1)
  round(mean(out_baseline$DecisionError)*100, 1)
  
# Performaces
  percErrors <- c(round(mean(out_pdftools$Error)*100, 1),
                  round(mean(na.omit(out_tabulizer$Error))*100, 1),
                  round(mean(out_baseline$Error)*100, 1))
  percDecErr <- c(round(mean(out_pdftools$DecisionError)*100, 1),
                  round(mean(na.omit(out_tabulizer$DecisionError))*100, 1),
                  round(mean(out_baseline$DecisionError)*100, 1))
  nextract <- c(nrow(out_pdftools),
                nrow(out_tabulizer),
                nrow(out_baseline))
  percComp <- data.frame(percErrors = percErrors,
                         percDecErr   = percDecErr,
                         nextract     = nextract)
  
  colnames(percComp) <- c("Errs_%", "Dec_Errs_%", "n_extra")
  row.names(percComp) <- c("pdftools", "tabulizer", "CRAN")
  percComp
  
  extraComp
  
  # Read by pdftool version, not read by CRAN
  length(out_pdftools$Value %in% out_baseline$Value)
  out_pdftools[out_pdftools$Value %in% out_baseline$Value == FALSE, ]
  sum(out_pdftools$Value %in% out_baseline$Value == FALSE)
    
  # Read by CRAN, not read by pdftools
  length(out_baseline$Value %in% out_pdftools$Value)
  out_baseline[out_baseline$Value %in% out_pdftools$Value == FALSE, ]
  sum(out_baseline$Value %in% out_pdftools$Value == FALSE)
   # When test is on two lines, does statcheck misses the test
   # What happens
    # Check out this result
    out_baseline[out_baseline$Value %in% out_pdftools$Value == FALSE, ][1, ]
    # is in article
    art <- "./extra/articles_consistency/APA-JEP-2019.pdf"
    # read it with new fucntion
    out <- getPDF(art)[10]
    # Observe how in the middle of the output there is this bit:
    # "t(78.0) = 1.01,\n                                                                 p = 0.32),"
    # in the paper itself (page 10), you can see that the paper presents has a return between "," and "p"
    # could be fixed somehow because it's very consisntent across papers
    
    # Check out this other result:
    out_baseline[out_baseline$Value %in% out_pdftools$Value == FALSE, ][11, ]
    art <- "./extra/articles_consistency/ELS-JESP-2009.pdf"
    getPDF(art)[2]
    # towards the end you can see this is read:
    # "t(65) = 19.72,\nlowed us to attach Black and White heads (matched in level of fa-                     p < .001"
    # unfrotunately words are place in between.
    # This is the disadvantage of this method. However, to me it seems that the overall imporved performance
    # and the easier installation method make it a better option. Especially because there is no particular link
    # between how ”well" a statistic is reported and the fact that it's reported on two different lines.
