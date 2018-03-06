library(ggplot2)
library(raster)
library(dplyr)
library(tidyr)
library(rgeos)
library(knitr)
library(maptools)
library(leaflet)

wg <- readShapePoly('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/WesternGhats-proper/WesternGhatsProper.shp')

all_lists <- all %>% group_by(SAMPLING.EVENT.IDENTIFIER) %>% slice(1) %>% ungroup()

all_lists1 <- all_lists

coordinates(all_lists) <- ~LONGITUDE+LATITUDE

proj4string(all_lists) <- proj4string(wg)

wg_lists <- over(wg,all_lists, returnList = T)

wg_lists1 <- do.call(rbind,wg_lists)


all_wg <- all %>% filter(SAMPLING.EVENT.IDENTIFIER %in% wg_lists1$SAMPLING.EVENT.IDENTIFIER)

spec_freq <- all_wg %>% filter(CATEGORY %in% c("species","domestic","issf","form") & APPROVED == 1 & ALL.SPECIES.REPORTED ==1) 

visits <- n_distinct(spec_freq$SAMPLING.EVENT.IDENTIFIER)

spec_freq <- spec_freq %>% group_by(SPECIES.NAME) %>% summarise(freq = n()/length(unique(all_wg$SAMPLING.EVENT.IDENTIFIER))) %>% arrange(desc(freq))

write.csv(spec_freq,'C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/western_ghat_frequencies.csv')
