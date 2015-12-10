library(dplyr)

lasreader <- function(lasdir, slb = FALSE){
  
  ##Schlumberger .las files have a different format that places the column names above
  ##the line beginning with "~A" and the data below.  If file is from Schlumberger then
  ##set slb = TRUE to offset reader correctly.
  if(slb == TRUE){
    offset <- -2
  } else {
    offset <- 0
  }
  
  ##Calculate number of lines down to line beginning with "~A"
  rawlas <- readChar(lasdir, file.info(lasdir)$size)
  lasheader <- substr(rawlas, 1, regexpr("~A", rawlas)[1])
  n <- (nchar(lasheader)-nchar(gsub("\r\n", " ", lasheader)))+1
  
  ##Read data from .las file without column names
  lastable <- read.table(lasdir, skip = n, header = TRUE, na.strings = "-999.25")
  
  ##Read in header including line beginning with "~A"
  columns <- readLines(lasdir, n = (n+offset))
  
  ##Isolate last line read in aka the line beginning with "~A"
  columns <- columns[length(columns)]
  
  ##Split up character vector into a list
  columns <- as.vector(unlist(strsplit(columns, " ")), mode = "list")
  
  ##Previous line introduces many empty values
  ##Convert those values to NA
  for (i in 1:length(columns)) {
    if(columns[[i]] == ""){
      columns[[i]] <- NA
    }
  }
  
  ##Eliminate NA to be left with a list of column names plus a first value of "~A"
  columns <- columns[!is.na(columns)]
  
  ##Eliminate "~A value
  columns <- columns[2:length(columns)]
  
  ##Some las files have gamma ray stored as GRR instead of GR
  ##Determine if that is the case here and if so replace "GRR" with "GR"
  if("GRR" %in% columns){
    grrnum <- which(columns == "GRR")
    columns[grrnum] <- "GR"
  }
  
  ##Assign extracted column names to data from las file
  names(lastable) <- columns
  
  ##Store las data to global environment
  lasdata <<- lastable
}

csvlasmerge <- function(csvfile, lasfile, outputfile){
  ##This function will read in a csv file (such as those containing drilling parameters that are downloaded from Pason)
  ##it will then utilize the lasreader function to read an las file.  It will then merge the files on the basis of depth and output
  ##the merged data to a csv file.
  csvdata <- read.csv(csvfile, na.strings = -999.25, header = TRUE)
  lastable <- lasreader(lasfile)
  merged_data <- merge(x = csvdata, y = lastable, by.x = "Hole.Depth", by.y = "DEPT")
  write.csv(merged_data, file = outputfile)
}
