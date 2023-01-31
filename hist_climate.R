#historical climate review
source("climateReaderFunctions.R")

wi.station.code <- 10
wi.station.name <- "C130929"

ic.station.code <- 34
ic.station.name <- "C121319"

cc.station.code <- 37
cc.station.name <- "C138062"

h.pcp.path <- "/Volumes/pbittermSSD/climateInputs/historicalObserved/pcp1.pcp"
h.tmp.path <- "/Volumes/pbittermSSD/climateInputs/historicalObserved/Tmp1.tmp"

h.pcp <- pcp.parser(h.pcp.path)
h.tmp <- tmp.parser(h.tmp.path)


cc.pcp <- h.pcp %>% select(newdate, C138062) %>% 
  mutate(month = format(newdate, "%m"), year = format(newdate, "%Y")) %>%
  mutate(C138062 = ifelse(C138062 < 0, 0, C138062)) %>% 
  group_by(year, month) %>% summarise(monthlyPrecip = sum(C138062)) %>%
  mutate(yrMonStr = paste0(year, "-", month, "-1")) %>%
  mutate(yrMon = as.Date(yrMonStr))
  
  

ggplot(data = cc.pcp, aes(x = month, y = monthlyPrecip, colour = year)) + 
  #geom_line(aes(group = year)) + 
  geom_point(aes(colour = year))
  


cc.tmp <- h.tmp %>% select(newdate, C138062max, C138062min) %>% mutate(C138062mean = (C138062max + C138062min) / 2)

