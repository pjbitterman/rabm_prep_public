###climate input comparisions
source("climateReaderFunctions.R")

library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggthemes)
library(RColorBrewer)

ex.data.path <- "/Volumes/pbittermSSD/climateInputs/"
out.data.path <- "/Volumes/pbittermSSD/climateInputsRDS/"

wi.station.code <- 10
wi.station.name <- "C130929"

ic.station.code <- 34
ic.station.name <- "C121319"

cc.station.code <- 37
cc.station.name <- "C138062"


# tmp.acc <- data.frame()
# pcp.acc <- data.frame()
# 
# clim.models <- list.files(ex.data.path, recursive = F)
# for(one.model in clim.models){
#   model.path <- paste0(ex.data.path, "/", one.model, "/")
#   timePeriods <- list.files(model.path)
#   for(tp in timePeriods){
#     tp.path <- paste0(model.path, tp, "/")
#     pcp.path <- paste0(tp.path, "pcp1.pcp")
#     tmp.path <- paste0(tp.path, "tmp1.tmp")
#     print(paste0("parsing: ", tp.path))
# 
#     pcp.parsed <- pcp.parser(pcp.path) %>% mutate(climateModel = one.model, timePeriod = tp)
#     tmp.parsed <- tmp.parser(tmp.path) %>% mutate(climateModel = one.model, timePeriod = tp)
#     pcp.acc <- bind_rows(pcp.acc, pcp.parsed)
#     tmp.acc <- bind_rows(tmp.acc, tmp.parsed)
# 
#     print(paste0("done with: ", tp.path))
#   }
# }
# 
# saveRDS(tmp.acc, paste0(out.data.path, "/all_tmp.RDS"))
# saveRDS(pcp.acc, paste0(out.data.path, "/all_pcp.RDS"))


allTmp <- readRDS(paste0(out.data.path, "/all_tmp.RDS")) %>% filter(climateModel != "MM5Iccsm")
allPcp <- readRDS(paste0(out.data.path, "/all_pcp.RDS")) %>% filter(climateModel != "MM5Iccsm")


diss.tmp <- allTmp %>% filter(climateModel == "CRCMccsm" | climateModel == "RCM3gfdl" | climateModel == "WRFGcgcm3")
diss.pcp <- allPcp %>% filter(climateModel == "CRCMccsm" | climateModel == "RCM3gfdl" | climateModel == "WRFGcgcm3")

ex.pcp <- diss.pcp %>% select(newdate, C138062, climateModel, timePeriod) %>% 
  mutate(month = format(newdate, "%m"), year = format(newdate, "%Y")) %>%
  mutate(C138062 = ifelse(C138062 < 0, NA, C138062)) %>% 
  mutate(yrMonStr = paste0(year, "-", month, "-1")) %>%
  mutate(yrMon = as.Date(yrMonStr)) %>%
  filter(timePeriod != "historical") %>%
  filter(as.numeric(month) > 3 & as.numeric(month) < 10) %>%
  mutate(timePeriod = ifelse(timePeriod == "Past", "Contemporary", timePeriod)) %>%
  group_by(year, climateModel, timePeriod) %>% summarise(annualPrecip = sum(C138062))

ggplot(data = ex.pcp, aes(x = as.numeric(year), y = annualPrecip)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~climateModel, nrow = 3) +
  labs(title = "total annual precipitation by climate model and modeled time period", 
       subtitle = "length of time periods: 30 years", x = "month of year", y = "mm") +
  theme_few() 


ex.pcp.mon <- diss.pcp %>% select(newdate, C138062, climateModel, timePeriod) %>% 
  mutate(month = format(newdate, "%m"), year = format(newdate, "%Y")) %>%
  mutate(C138062 = ifelse(C138062 < 0, NA, C138062)) %>% 
  mutate(yrMonStr = paste0(year, "-", month, "-1")) %>%
  mutate(yrMon = as.Date(yrMonStr)) %>%
  filter(timePeriod != "historical") %>%
  filter(as.numeric(month) > 3 & as.numeric(month) < 10) %>%
  mutate(timePeriod = ifelse(timePeriod == "Past", "Contemporary", timePeriod)) %>%
  group_by(year, month, climateModel, timePeriod) %>% summarise(monthlyPrecip = sum(C138062))

ggplot(data = ex.pcp.mon, aes(x = as.numeric(month), y = monthlyPrecip, colour = year)) +
  geom_line(alpha = 0.9) +
  facet_grid(timePeriod~climateModel) +
  labs(title = "monthly precip April-Sept", 
       subtitle = "length of time periods: 30 years", x = "month of year", y = "mm")  +
  theme_few() 
#theme(legend.position = c(.75, .10)) ##depends on number of facets

ex.tmp <- diss.tmp %>% select(newdate, C138062max, C138062min, climateModel, timePeriod) %>%
  mutate(C138062max = ifelse(C138062max < -50, NA, C138062max)) %>%
  mutate(C138062min = ifelse(C138062min < -50, NA, C138062min)) %>%
  mutate(C138062mean = (C138062max + C138062min) / 2) %>%
  mutate(month = format(newdate, "%m"), year = format(newdate, "%Y")) %>%
  mutate(yrMonStr = paste0(year, "-", month, "-1")) %>%
  mutate(yrMon = as.Date(yrMonStr)) %>%
  filter(timePeriod != "historical") %>%
  mutate(timePeriod = ifelse(timePeriod == "Past", "Contemporary", timePeriod)) %>%
  #filter(as.numeric(month) > 3 & as.numeric(month) < 10) %>%
  group_by(month, year, climateModel, timePeriod) %>% summarise(avgTemp = mean(C138062max))

ggplot(data = ex.tmp, aes(x = as.numeric(month), y = avgTemp, colour = year)) +
  geom_line(alpha = 0.5) +
  facet_grid(~climateModel) +
  labs(title = "daily mean temperatures by month May-Sept", 
       subtitle = "length of time periods: 30 years", x = "month of year", y = "degrees Celsius")  +
  theme_few() 
  #theme(legend.position = c(.75, .10)) ##depends on number of facets

  



cc.pcp <- diss.pcp %>% select(newdate, C138062, climateModel, timePeriod) %>% 
  mutate(month = format(newdate, "%m"), year = format(newdate, "%Y")) %>%
  mutate(C138062 = ifelse(C138062 < 0, NA, C138062)) %>% 
  mutate(yrMonStr = paste0(year, "-", month, "-1")) %>%
  mutate(yrMon = as.Date(yrMonStr)) %>%
  filter(timePeriod != "historical") %>%
  mutate(timePeriod = ifelse(timePeriod == "Past", "Contemporary", timePeriod)) %>%
  group_by(year, month, climateModel, timePeriod) %>% summarise(monthlyPrecip = sum(C138062))


ggplot(data = cc.pcp, aes(x = month, y = monthlyPrecip, fill = timePeriod)) + 
  geom_boxplot(aes(fill = timePeriod), position = position_dodge(width = .9)) +
  facet_wrap(~climateModel) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Monthly precipitation by climate model and time period", 
       subtitle = "length of time periods: 30 years", x = "month of year", y = "mm") +
  theme_few() #+
  #theme(legend.position = c(.75, .10)) ##depends on number of facets

pcps <- cc.pcp %>% group_by(month, timePeriod, climateModel) %>% summarise_each(funs(mean(monthlyPrecip)))

cc.tmp <- diss.tmp %>% select(newdate, C138062max, C138062min, climateModel, timePeriod) %>%
  mutate(C138062max = ifelse(C138062max < -50, NA, C138062max)) %>%
  mutate(C138062min = ifelse(C138062min < -50, NA, C138062min)) %>%
  mutate(C138062mean = (C138062max + C138062min) / 2) %>%
  mutate(month = format(newdate, "%m"), year = format(newdate, "%Y")) %>%
  mutate(yrMonStr = paste0(year, "-", month, "-1")) %>%
  mutate(yrMon = as.Date(yrMonStr)) %>%
  filter(timePeriod != "historical") %>%
  mutate(timePeriod = ifelse(timePeriod == "Past", "Contemporary", timePeriod)) %>%
  group_by(year, month, climateModel, timePeriod)

temps <- cc.tmp %>% group_by(month, timePeriod, climateModel) %>% summarise_each(funs(mean(C138062mean)))


ggplot(data = cc.tmp, aes(x = month, y = C138062mean, fill = timePeriod)) + 
  geom_boxplot(aes(fill = timePeriod), position = position_dodge(width = .9)) +
  facet_wrap(~climateModel) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Mean monthly temperature by climate model and time period", 
       subtitle = "length of time periods: 30 years", x = "month of year", y = "degrees Celsius")  +
  theme_few() #+
  #theme(legend.position = c(.75, .10)) ##depends on number of facets


cc.pcp.yr <- allPcp %>% select(newdate, C138062, climateModel, timePeriod) %>% 
  mutate(month = format(newdate, "%m"), year = format(newdate, "%Y")) %>%
  mutate(C138062 = ifelse(C138062 < 0, NA, C138062)) %>% 
  mutate(yrMonStr = paste0(year, "-", month, "-1")) %>%
  mutate(yrMon = as.Date(yrMonStr)) %>%
  group_by(year, climateModel, timePeriod) %>% summarise(monthlyPrecip = sum(C138062))


ggplot(data = cc.pcp.yr, aes(x = year, y = monthlyPrecip, colour = climateModel)) + 
  geom_point() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "total monthly precipitation by climate model and modeled time period", 
       subtitle = "length of time periods: 30 years", x = "month of year", y = "mm") +
  theme_few()


# ggplot(data = cc.tmp, aes(x = month, y = mean(C138062mean), colour = climateModel)) + 
#   geom_line() +
#   geom_point() +
#   facet_wrap(~timePeriod) +
#   labs(title = "mean monthly temp by climate model and modeled time period", 
#        subtitle = "length of time periods: 30 years", x = "month of year", y = "degrees Celsius")  +
#   theme_few()


contemporaryPeriod <- cc.tmp %>% filter(year >= 1968 & year <= 1998)
notEmp <- contemporaryPeriod %>% filter(climateModel != "empirical")
emp <- contemporaryPeriod %>% filter(climateModel == "empirical")
joined <- left_join(notEmp, emp, by = c("newdate")) %>% 
  mutate(modelbias = C138062mean.x - C138062mean.y)

ggplot(data = joined, aes(x = yrMon.x, y = modelbias, colour = climateModel.x)) +
  geom_point() +
  geom_line()


mto <- joined %>% group_by(climateModel.x, month.x, year.x)# %>% summarise(meanTempOff = (modelbias))
ggplot(data = mto, aes(x = month.x, y = modelbias)) +
  geom_boxplot(outlier.shape=NA) + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.1, height=0), alpha = 0.05, colour = "blue") +
  facet_wrap(~climateModel.x) +
  theme_few() + 
  labs(title = "Clear Creek (station 138062): daily temperature error grouped by month, 1968-1998", 
       subtitle = "daily modeled - empirical ((max - min) / 2)",
       x = "month", y = "degrees C")






contemporaryPeriod.pcp <- cc.pcp %>% filter(year >= 1968 & year <= 1998)
notEmp.pcp <- contemporaryPeriod.pcp %>% filter(climateModel != "empirical")
emp.pcp <- contemporaryPeriod.pcp %>% filter(climateModel == "empirical")
joined.pcp <- left_join(notEmp.pcp, emp.pcp, by = c("year", "month")) %>% 
  mutate(modelbias = monthlyPrecip.x- monthlyPrecip.y)

ggplot(data = joined.pcp, aes(x = yrMon.x, y = modelbias, colour = climateModel.x)) +
  geom_point() +
  geom_line()


mpo <- joined.pcp %>% group_by(climateModel.x, month, year) %>% summarise(meanPcpOff = (modelbias))
ggplot(data = mpo, aes(x = month, y = meanPcpOff)) +
  geom_boxplot(outlier.shape=NA) + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.1, height=0), alpha = 0.25, colour = "blue") +
  facet_wrap(~climateModel.x) +
  theme_few() + 
  labs(title = "Clear Creek (station 138062): precip error by month, 1968-1998", 
       subtitle = "modeled - empirical (monthly sums)",
       x = "month", y = "mm")

