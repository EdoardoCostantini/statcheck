# Inner function to read pdf:
getPDF <- function(x)
{
  txtfiles <- character(length(x))
  for (i in 1:length(x))
  {
    system(paste('pdftotext -q -enc "ASCII7" "', x[i], '"', sep = ""))
    if (file.exists(gsub("\\.pdf$", "\\.txt", x[i]))) {
      fileName <- gsub("\\.pdf$", "\\.txt", x[i])
      txtfiles[i] <- readChar(fileName, file.info(fileName)$size)
    } else{
      warning(paste("Failure in file", x[i]))
      txtfiles[i] <- ""
    }
  }
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
      txts[[i]] <-  pdf_text(files[i]) # original has "getPDF" instead of "pdf_text"; txts[i] (but gets only first page?)
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
    
    txts <-  sapply(files, pdf_text) # original has "getPDF" instead of "pdf_text"
    names(txts) <-
      gsub("\\.pdf$", "", basename(files), perl = TRUE)
    return(statcheck(txts, ...))
  }
