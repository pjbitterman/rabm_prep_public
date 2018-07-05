# append

source("climateReaderFunctions.R")

library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggthemes)
library(RColorBrewer)

out.data.path <- "/Volumes/pbittermSSD/climateInputsRDS/"

wi.station.code <- 10
wi.station.name <- "C130929"

ic.station.code <- 34
ic.station.name <- "C121319"

cc.station.code <- 37
cc.station.name <- "C138062"



allTmp <- readRDS(paste0(out.data.path, "/all_tmp.RDS")) %>% filter(climateModel != "MM5Iccsm")
allPcp <- readRDS(paste0(out.data.path, "/all_pcp.RDS")) %>% filter(climateModel != "MM5Iccsm")


crcmccsm.t.past <- allTmp %>% filter(climateModel == "CRCMccsm" & timePeriod == "Past")

crcmccsm.t.future <- allTmp %>% filter(climateModel == "CRCMccsm" & timePeriod == "Future")
c.f.t.toappend <- crcmccsm.t.future %>% mutate(year = year - 39)

crcm.t.app <- bind_rows(crcmccsm.t.past, crcmccsm.t.future) %>% select(-(newdate:timePeriod))



crcmccsm.p.past <- allPcp %>% filter(climateModel == "CRCMccsm" & timePeriod == "Past")

crcmccsm.p.future <- allPcp %>% filter(climateModel == "CRCMccsm" & timePeriod == "Future")
c.f.p.toappend <- crcmccsm.p.future %>% mutate(year = year - 39)

crcm.p.app <- bind_rows(crcmccsm.p.past, crcmccsm.p.future) %>% select(-(newdate:timePeriod))



template.pcp <- "/Volumes/pbittermSSD/climateInputs/CRCMccsm/Past/pcp1.pcp"
template.tmp <- "/Volumes/pbittermSSD/climateInputs/CRCMccsm/Past/tmp1.tmp"

###write out
library(stringr)

###get the first 4 rows as strings
topLines.pcp <- readLines(template.pcp, n = 4)

###format the year column
yearFormatted.p <- sprintf("%s", crcm.p.app$year)

###format the day column
dayFormatted.p <- sprintf("%s", crcm.p.app$day) %>% str_pad(3, pad = "0")


###format a column of the temperature series
pcpFormat <- function(pcpSeries){
  toReturn <- sprintf("%.1f", pcpSeries) %>% str_pad(5, pad = "0") %>% gsub("0-", "-0", .)
  return(toReturn)
}

#only the climate series 
seriesOnly.p <- select(crcm.p.app, -year, -day)

seriesFormatted.p <- sapply(seriesOnly.p, function(x) pcpFormat(x))
formatted.df.p <- data.frame(yearFormatted.p, dayFormatted.p, seriesFormatted.p)
pasted.p <- do.call("paste", formatted.df.p) %>% gsub(" ", "", .)
completed.p <- c(topLines.pcp, pasted.p)
writeLines(completed.p, "/Volumes/pbittermSSD/climateInputs/CRCMccsm_appended/pcp1.pcp", sep = "\n")



###TEMPERATURE
###get the first 4 rows as strings
topLines.tmp <- readLines(template.tmp, n = 4)

###format the year column
yearFormatted.t <- sprintf("%s", crcm.t.app$year)

###format the day column
dayFormatted.t <- sprintf("%s", crcm.t.app$day) %>% str_pad(3, pad = "0")


###format a column of the temperature series
tmpFormat <- function(tempSeries){
  toReturn <- sprintf("%.1f", tempSeries) %>% str_pad(5, pad = "0") %>% gsub("0-", "-0", .)
  return(toReturn)
}

#only the climate series 
seriesOnly.t <- select(crcm.t.app, -year, -day)

seriesFormatted.t <- sapply(seriesOnly.t, function(x) tmpFormat(x))
formatted.df.t <- data.frame(yearFormatted.t, dayFormatted.t, seriesFormatted.t)
pasted.t <- do.call("paste", formatted.df.t) %>% gsub(" ", "", .)
completed.t <- c(topLines.tmp, pasted.t)
writeLines(completed.t, "/Volumes/pbittermSSD/climateInputs/CRCMccsm_appended/tmp1.tmp", sep = "\n")
