# Inner function to read pdf:
getPDF <- function(x)
{
    
  txtfiles <- sapply(x, pdf_text) # the squared brakets are just for working with page 4 in the first pdf I provide

  # encode everything in UTF-32 (same result accross multiple operating systems)
  txtfiles <- stri_enc_toutf32(txtfiles) # check if the result of this is exactly the same across systems
  
  # Replace known wierd characters
  txtfiles <- lapply(txtfiles, gsub, pattern = "11005", replacement = "61", fixed = TRUE) # substitute double solidous (UTF-32 Decimal 11005) with equal sign (UTF-32 Decimal 61) [issue in JPSP, JEP]
  txtfiles <- lapply(txtfiles, gsub, pattern = "11021", replacement = "60", fixed = TRUE) # substitute U+2B0D (C++ \u2b0d; UTF-32 Decimal 11021) with equal less than sign (UTF-32 Decimal 60) [issue in JPSP, JEP]
  txtfiles <- lapply(txtfiles, gsub, pattern = "11002", replacement = "45", fixed = TRUE) # substitute U+2AFA (UTF-32 Decimal 11002) with HYPHEN-MINUS sign (UTF-32 Decimal 45) [issue in JPSP]
  txtfiles <- lapply(txtfiles, gsub, pattern = "9273", replacement = "967", fixed = TRUE) # substitute U+2439 (C++ \u2439; UTF-32 Decimal 9273) with small greek chi (UTF-32 Decimal 967)

  # Revert to UTF-8
  txtfiles <- stri_enc_fromutf32(txtfiles)
  
  return(txtfiles)

}

## Function to check directory of PDFs:
checkPDFdir <-
  function(dir,
           subdir = TRUE,
           ...) {
    if (missing(dir))
      dir <- tk_choose.dir()
    
    all.files <-
      list.files(dir,
                 pattern = "\\.pdf",
                 full.names = TRUE,
                 recursive = subdir)
    files <- all.files[grepl("\\.pdf$", all.files)]
    
    if (length(files) == 0)
      stop("No PDF found")
    
    txts <- vector("list", length(files)) #original: character(length(files))
    message("Importing PDF files...")
    pb <- txtProgressBar(max = length(files), style = 3)
    for (i in 1:length(files))
    {
      txts[[i]] <-  getPDF(files[i]) # original has "getPDF" instead of "pdf_text"; txts[i] (but gets only first page?)
      setTxtProgressBar(pb, i)
    }
    close(pb)
    names(txts) <- gsub("\\.pdf$", "", basename(files))
    return(statcheck(txts, ...))
  }

## Function to given PDFs:
checkPDF <-
  function(files, ...) {
    if (missing(files))
      files <- tk_choose.files()
    
    txts <-  sapply(files, getPDF) # original has "getPDF" instead of "pdf_text"
    names(txts) <-
      gsub("\\.pdf$", "", basename(files), perl = TRUE)
    return(statcheck(txts, ...))
  }
