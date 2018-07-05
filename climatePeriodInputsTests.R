### comparing WRGFccsm models for three profiles - contemporary, future, 2012 all

###climate input comparisions
setwd(file.path("~", "GitHub", "diss_prep"))

source("climateReaderFunctions.R")

library(ggplot2)
library(Rmisc)
library(dplyr)
library(ggplot2)

ex.data.path <- "/Volumes/pbittermSSD/climateComparisonWRFGccsm/"

scenarios <- c("WRFGccsm", "WRFGccsm_contemporary", "WRFGccsm_2012climate", "WRFGccsm_2012climateFuture", "historicalObserved")

cc.station.code <- 37
cc.station.name <- "C138062"


###read the scenario files
for(sc in scenarios){
  path.pcp <- paste0(ex.data.path, sc, "/iter0/pcp1.pcp")
  path.tmp <- paste0(ex.data.path, sc, "/iter0/tmp1.tmp")

  ##parse the data
  pcp <- pcp.parser(path.pcp)
  tmp <- tmp.parser(path.tmp)
  
  ##add a column for which climate model was used (helpful for melting)
  pcp.mutated <- mutate(pcp, climateModel = sc)
  tmp.mutated <- mutate(tmp, climateModel = sc)

  ##assign to global variables
  assign(paste0(sc, ".pcp"), pcp.mutated)
  assign(paste0(sc, ".tmp"), tmp.mutated)

  ##get rid of the temp values because R doesn't clean up namespace
  rm(path.pcp, path.tmp, pcp, tmp, pcp.mutated, tmp.mutated)
}

###setup dataframes
pcp.big <- bind_rows(WRFGccsm.pcp, WRFGccsm_contemporary.pcp, WRFGccsm_2012climate.pcp, WRFGccsm_2012climateFuture.pcp, historicalObserved.pcp)
tmp.big <- bind_rows(WRFGccsm.tmp, WRFGccsm_contemporary.tmp, WRFGccsm_2012climate.tmp, WRFGccsm_2012climateFuture.tmp, historicalObserved.tmp)


cc.pcp <- select(pcp.big, year, day, climateModel, contains(cc.station.name))
cc.tmp <- select(tmp.big, year, day, climateModel, contains(cc.station.name))

jan.pcp <- filter(cc.pcp, day %in% 1:31) %>% mutate(MONTH = 1)
feb.pcp <- filter(cc.pcp, day %in% 32:59) %>% mutate(MONTH = 2)
mar.pcp <- filter(cc.pcp, day %in% 60:90) %>% mutate(MONTH = 3)
apr.pcp <- filter(cc.pcp, day %in% 91:120) %>% mutate(MONTH = 4)
may.pcp <- filter(cc.pcp, day %in% 121:151) %>% mutate(MONTH = 5)
jun.pcp <- filter(cc.pcp, day %in% 152:181) %>% mutate(MONTH = 6)
jul.pcp <- filter(cc.pcp, day %in% 182:212) %>% mutate(MONTH = 7)
aug.pcp <- filter(cc.pcp, day %in% 213:243) %>% mutate(MONTH = 8)
sep.pcp <- filter(cc.pcp, day %in% 244:273) %>% mutate(MONTH = 9)
oct.pcp <- filter(cc.pcp, day %in% 274:304) %>% mutate(MONTH = 10)
nov.pcp <- filter(cc.pcp, day %in% 305:335) %>% mutate(MONTH = 11)
dec.pcp <- filter(cc.pcp, day %in% 336:366) %>% mutate(MONTH = 12)

combined.pcp <- bind_rows(jan.pcp, feb.pcp, mar.pcp, apr.pcp, may.pcp, jun.pcp, jul.pcp, aug.pcp, sep.pcp, oct.pcp, nov.pcp, dec.pcp)

jan.tmp <- filter(cc.tmp, day %in% 1:31) %>% mutate(MONTH = 1)
feb.tmp <- filter(cc.tmp, day %in% 32:59) %>% mutate(MONTH = 2)
mar.tmp <- filter(cc.tmp, day %in% 60:90) %>% mutate(MONTH = 3)
apr.tmp <- filter(cc.tmp, day %in% 91:120) %>% mutate(MONTH = 4)
may.tmp <- filter(cc.tmp, day %in% 121:151) %>% mutate(MONTH = 5)
jun.tmp <- filter(cc.tmp, day %in% 152:181) %>% mutate(MONTH = 6)
jul.tmp <- filter(cc.tmp, day %in% 182:212) %>% mutate(MONTH = 7)
aug.tmp <- filter(cc.tmp, day %in% 213:243) %>% mutate(MONTH = 8)
sep.tmp <- filter(cc.tmp, day %in% 244:273) %>% mutate(MONTH = 9)
oct.tmp <- filter(cc.tmp, day %in% 274:304) %>% mutate(MONTH = 10)
nov.tmp <- filter(cc.tmp, day %in% 305:335) %>% mutate(MONTH = 11)
dec.tmp <- filter(cc.tmp, day %in% 336:366) %>% mutate(MONTH = 12)

combined.tmp <- bind_rows(jan.tmp, feb.tmp, mar.tmp, apr.tmp, may.tmp, jun.tmp, jul.tmp, aug.tmp, sep.tmp, oct.tmp, nov.tmp, dec.tmp)
combined.tmp <- mutate(combined.tmp, C138062mean = (C138062max + C138062min) / 2)

pcp.agg <- combined.pcp %>% select(year, C138062, climateModel) %>% group_by(year, climateModel) %>% summarise_all(funs(sum(C138062)))
tmp.agg <- combined.tmp %>% select(year, C138062mean, climateModel) %>% group_by(year, climateModel) %>% summarise_all(funs(mean(C138062mean)))


ggplot(pcp.agg, aes(x = year, y = C138062, colour = climateModel)) +
  geom_line(aes(colour = climateModel)) +
  geom_point(aes(colour = climateModel)) +
  theme_minimal() + 
  labs(title = "precip by year", y = "annual mm", x = "year")

ggplot(tmp.agg, aes(x = year, y = C138062mean / 2, colur = climateModel)) +
  geom_line(aes(colour = climateModel)) +
  geom_point(aes(colour = climateModel)) + 
  theme_minimal() + 
  labs(title = "mean max temp by year", y = "deg", x = "year")





