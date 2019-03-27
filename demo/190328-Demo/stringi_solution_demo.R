### Project:     Statcheck traineeship
### Object:      Demo for statcheck meeting 28/03/2019
### Description: In this demo you implemented a rough PDF processing system using:
###              - pdftools:pdf_text function to convert pdf files in R character strings
###              - stringi:stri_enc_toutf32 and stringi:stri_encfromut32 to convert unreadable stuff
### Date:        2019-03-26

# Install Packages
  install.packages("pdftools")
  install.packages("stringi")
  install.packages("Rcpp") # for the uni computers (alwyas need to update this package)

# Load Packages and functions
  library(pdftools)
  library(stringi)
  library(plyr)
  source("./statcheck.R")
  source("./htmlImport.R")
  source("./PDFimport.R")
  
# Load Articles (two articles for this demo)
  x <- "./Ames2004.pdf"   # page 4 for tests; page 10 for a chi-square
  x <- "./Beaman2004.pdf" # page 3 for tests
  x <- "./PDF-PB-2009-Den.pdf" # page 15 chi square at the end
  page_of_int <- 15
  
# One page exposé ---------------------------------------------------------
    
  # 1. Show output of pdf_text(x) in UTF32 encoding
    text_output_nostrin <- pdftools::pdf_text(x)[[page_of_int]]
    text_output <- stri_enc_toutf32(pdftools::pdf_text(x)[[page_of_int]])
  # 2. Peak at the last 30ish values (there should be an equal sing there)
    tail(text_output[[1]], 300)
  # 3. Substitute codes
    text_output[[1]][which(text_output[[1]] == 11005)] <- 61
      # double solidous is 11005 in UTF-32 decimal http://www.fileformat.info/info/unicode/char/2afd/index.htm
    text_output[[1]][which(text_output[[1]] == 11021)] <- 60
    text_output[[1]][which(text_output[[1]] == 11002)] <- 45
    text_output[[1]][which(text_output[[1]] == 9273)] <- 967
    text_output[[1]][which(text_output[[1]] == 9253)] <- 947
  # 4. Get things back in UTF8
    text_output_nostrin[[1]]
    stri_enc_fromutf32(tail(text_output[[1]], 7500))
  # 5. Apply statcheck (happly and produly)
    statcheck(stri_enc_fromutf32(text_output[[1]]))

# Full article test -------------------------------------------------------
  
  # 1. Show output of pdf_text(x) in UTF32 encoding
    text_output <- stri_enc_toutf32(pdftools::pdf_text(x))
    str(text_output) # an object per page
    
  # 2. Peak at the last 30ish values (there should be an equal sing there)
    tail(text_output[[page_of_int]], 300)
    
  # 3. Substitute codes
    # Replace known wierd characters
    txtfiles <- lapply(text_output, gsub, pattern = "11005", replacement = "61", fixed = TRUE) # substitute double solidous (UTF-32 Decimal 11005) with equal sign (UTF-32 Decimal 61)
    txtfiles <- lapply(txtfiles, gsub, pattern = "11021", replacement = "60", fixed = TRUE) # substitute up down black arrow (UTF-32 Decimal \u2b0d) with equal less than sign (UTF-32 Decimal 60)
    txtfiles <- lapply(txtfiles, gsub, pattern = "11002", replacement = "45", fixed = TRUE) # substitute U+2AFA (UTF-32 Decimal 11002) with HYPHEN-MINUS sign (UTF-32 Decimal 45) [issue in JPSP]
    txtfiles <- lapply(txtfiles, gsub, pattern = "9273", replacement = "967", fixed = TRUE) # substitute \u2439 (UTF-32 Decimal 9273) with small greek chi (UTF-32 Decimal 967)
    txtfiles <- lapply(txtfiles, gsub, pattern = "9253", replacement = "947", fixed = TRUE) # substitute \u03B3 (UTF-32 Decimal 9253) with small greek chi (UTF-32 Decimal 947)
 
  # 4. Get things back in UTF8
    txtfiles <- stri_enc_fromutf32(txtfiles)
    
  # 5. Apply statcheck (happly and produly)
    statcheck(txtfiles)
    nrow(statcheck(txtfiles))
    
  # 6. CFR w/ other methods
    # XPDF (current version)
    statcheck(getPDF(x))
    statcheck::statcheck(x)
    # Manual
    manual_coded <- read.table("ManuallyCodedComparisonSample.txt", header = TRUE)
    manual_coded[manual_coded$authors == "Ames,-Daniel-R", ]
    sum(manual_coded$authors == "Ames,-Daniel-R")
    # HTML
    html_out <- checkHTML("./Ames2004.htm")
    nrow(html_out)