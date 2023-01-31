###climate file reader functions
library(readr)
library(dplyr)

tmp.parser <- function(fname){
  ### get the column names first
  firstRow <- read.table(fname, header = F, nrows = 1, stringsAsFactors = FALSE)
  myNames <- firstRow$V2
  splitNames <- strsplit(myNames, ",")[[1]]
  splitSub <- substr(splitNames, 1, 7)
  ### then duplicate for min and max
  doubledName <- rep(splitSub, each = 2)
  minMax <- rep(c("max","min"), length(doubledName)/2)
  pasted <- paste0(doubledName, minMax)
  first.cols <- c("year", "day")
  col.names <- c(first.cols, pasted)
  
  widthDayYear <- c(4,3)
  rest <- rep(5, length(doubledName))
  colwidths <- c(widthDayYear, rest)
  
  d <- readr::read_fwf(fname, fwf_widths(colwidths, col_names = col.names), skip = 4, na = '')
  
  timings <- select(d, year, day)
  station.data <- select(d, -year, -day)
  
  timings[] <- lapply(d, as.integer)
  station.data[] <- lapply(station.data, as.numeric)
  
  mydata <- data.frame(timings, station.data) %>% tbl_df
  
  toReturn <- addDateToClimateSeries(mydata)

  return(toReturn)
}

pcp.parser <- function(fname){
  ### get the column names first
  firstRow <- read.table(fname, header = F, nrows = 1, stringsAsFactors = FALSE)
  myNames <- firstRow$V2
  splitNames <- strsplit(myNames, ",")[[1]]
  splitSub <- substr(splitNames, 1, 7)

  first.cols <- c("year", "day")
  col.names <- c(first.cols, splitSub)
  
  widthDayYear <- c(4,3)
  rest <- rep(5, length(splitSub))
  colwidths <- c(widthDayYear, rest)
  
  d <- readr::read_fwf(fname, fwf_widths(colwidths, col_names = col.names), skip = 4, na = '')
  
  timings <- select(d, year, day)
  station.data <- select(d, -year, -day)
  
  timings[] <- lapply(d, as.integer)
  station.data[] <- lapply(station.data, as.numeric)
  
  mydata <- data.frame(timings, station.data) %>% tbl_df
  
  toReturn <- addDateToClimateSeries(mydata)
  
  return(toReturn)
}


tmp.yearRepeater <- function(yearToRepeat, fromDataFrame, toDataFrame){

  library(dplyr, quietly = TRUE)
  ###empty df to paste into
  builder.df <- data.frame()

  ###the data for the year we want to repeat
  toRepeat <- dplyr::filter(fromDataFrame, year == yearToRepeat)

  ###all of the years in the dataset
  yearsInFile <- dplyr::distinct(toDataFrame, year)
  
  ###kludgey but works
  for(i in rep(min(yearsInFile) : max(yearsInFile))){
    tempCopy <- data.frame(toRepeat)
    tempCopy$year <- i
    builder.df <- dplyr::bind_rows(builder.df, tempCopy)
  }

  return(builder.df)
}

pcp.yearRepeater <- function(yearToRepeat, fromDataFrame, toDataFrame){
  
  library(dplyr, quietly = TRUE)
  ###empty df to paste into
  builder.df <- data.frame()
  
  ###the data for the year we want to repeat
  toRepeat <- dplyr::filter(fromDataFrame, year == yearToRepeat)
  
  ###all of the years in the dataset
  yearsInFile <- dplyr::distinct(toDataFrame, year)
  
  ###kludgey but works
  for(i in rep(min(yearsInFile) : max(yearsInFile))){
    tempCopy <- data.frame(toRepeat)
    tempCopy$year <- i
    builder.df <- dplyr::bind_rows(builder.df, tempCopy)
  }
  
  return(builder.df)
}

addDateToClimateSeries <- function(swatClimateFile){
  library(dplyr, quietly = T)
  toReturn <- data.frame()
  years <- unique(swatClimateFile$year)
  
  for(yr in years){
    thisyear.df <- swatClimateFile %>% filter(year == yr)
    toAppend <- thisyear.df %>% mutate(newdate = as.Date(day, paste0(year, "-1-1")))
    toReturn <- bind_rows(toReturn, toAppend)
    
  }
  return(toReturn)
  
}

