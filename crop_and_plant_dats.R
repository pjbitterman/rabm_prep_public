###read and write plant.dat or crop.dat files

setwd(file.path("~", "GitHub", "diss_prep"))

in.file <- "./data/crop_and_plant_files/plant.dat"

cropdatparser <- function(filename){
  library(stringr)
  library(dplyr)
  data <- scan(filename, what = "character", sep = "\n")
  toReturn <- data.frame()
  
  for(line in seq(1, length(data),5)){
    line.1 <- data[line]
    line.2 <- data[line + 1]
    line.3 <- data[line + 2]
    line.4 <- data[line + 3]
    line.5 <- data[line + 4]
    
    split.1 <- trimws(line.1) %>% strsplit(., "\\s+")
    split.2 <- trimws(line.2) %>% strsplit(., "\\s+")
    split.3 <- trimws(line.3) %>% strsplit(., "\\s+")
    split.4 <- trimws(line.4) %>% strsplit(., "\\s+")
    split.5 <- trimws(line.5) %>% strsplit(., "\\s+")
    
    icnum <- as.numeric(split.1[[1]][1])
    cpnm <- as.character(split.1[[1]][2])
    idc <- as.numeric(split.1[[1]][3])
    
    bio_e <- as.numeric(split.2[[1]][1])
    hvsti <- as.numeric(split.2[[1]][2]) 
    blai <- as.numeric(split.2[[1]][3])
    frgrw1 <- as.numeric(split.2[[1]][4])
    laimx1 <- as.numeric(split.2[[1]][5])
    frgrw2 <- as.numeric(split.2[[1]][6])
    laimx2 <- as.numeric(split.2[[1]][7])
    dlai <- as.numeric(split.2[[1]][8])
    chtmx <- as.numeric(split.2[[1]][9])
    rdmx <- as.numeric(split.2[[1]][10])
    
    t_opt <- as.numeric(split.3[[1]][1])
    t_base <- as.numeric(split.3[[1]][2])
    cnyld <- as.numeric(split.3[[1]][3])
    cpyld <- as.numeric(split.3[[1]][4])
    pltnfr1 <- as.numeric(split.3[[1]][5])
    pltnfr2 <- as.numeric(split.3[[1]][6])
    pltnfr3 <- as.numeric(split.3[[1]][7])
    pltpfr1 <- as.numeric(split.3[[1]][8])
    pltpfr2 <- as.numeric(split.3[[1]][9])
    pltpfr3 <- as.numeric(split.3[[1]][10])
    
    wsyf <- as.numeric(split.4[[1]][1])
    usle_c <- as.numeric(split.4[[1]][2])
    gsi <- as.numeric(split.4[[1]][3])
    vpdfr <- as.numeric(split.4[[1]][4])
    frgmax <- as.numeric(split.4[[1]][5])
    wavp <- as.numeric(split.4[[1]][6])
    co2hi <- as.numeric(split.4[[1]][7])
    bioehi <- as.numeric(split.4[[1]][8])
    rsdco_pl <- as.numeric(split.4[[1]][9])
    alai_min <- as.numeric(split.4[[1]][10])
    
    bio_leaf <- as.numeric(split.5[[1]][1])
    mat_yrs <- as.numeric(split.5[[1]][2])
    bmx_trees <- as.numeric(split.5[[1]][3])
    bmdieoff <- as.numeric(split.5[[1]][4])
    rsr1c <- as.numeric(split.5[[1]][5])
    
    toPut <- data.frame(icnum, cpnm, idc, 
                        bio_e, hvsti, blai, frgrw1, laimx1, frgrw2, laimx2, dlai, chtmx, rdmx, 
                        t_opt, t_base, cnyld, cpyld, pltnfr1, pltnfr2, pltnfr3, pltpfr1, pltpfr2, pltpfr3,
                        wsyf, usle_c, gsi, vpdfr, frgmax, wavp, co2hi, bioehi, rsdco_pl, alai_min,
                        bio_leaf, mat_yrs, bmx_trees, bmdieoff, rsr1c)
    toReturn <- bind_rows(toReturn, toPut)
    
  }
  
  names(toReturn) <- c("icnum", "cpnm", "idc", 
                       "bio_e", "hvsti", "blai", "frgrw1", "laimx1", "frgrw2", "laimx2", "dlai", "chtmx", "rdmx", 
                       "t_opt", "t_base", "cnyld", "cpyld", "pltnfr1", "pltnfr2", "pltnfr3", "pltpfr1", "pltpfr2", "pltpfr3",
                       "wsyf", "usle_c", "gsi", "vpdfr", "frgmax", "wavp", "co2hi", "bioehi", "rsdco_pl", "alai_min",
                       "bio_leaf", "mat_yrs", "bmx_trees", "bmdieoff", "rsr1c")
  return(toReturn)  
}

parsed.df <- cropdatparser(in.file)


## writes data frame of crop data to a text file
## only write data that you have used the cropdatparser function to create
## as this funciton assumes variable names
cropdatawriter <- function(df.to.write){
  #test for duplicates
  if(anyDuplicated(df.to.write$icnum) > 0){
    print("you have duplicate crop/plant ids, fix ")
    break
  }
  

  
  
}