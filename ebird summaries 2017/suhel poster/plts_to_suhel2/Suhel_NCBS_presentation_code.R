library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(rgdal)
library(gganimate)

rm(list = ls())

indmap <- readOGR('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/Shape files/India2011-fully-reduced','India_2011GEO')

basemap <- ggplot() + 
  geom_path(data = fortify(indmap),aes(x = long,y = lat, group = group)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  expand_limits(y = 2) +
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(1,1,0,1), "cm"),
        panel.border = element_blank(),
        legend.text = element_text(size = 15),
        legend.position = 'right')+
  coord_equal()

basemap

#load('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/All-eBird-India-data-2018-01-17.RData')

#all$week <- week(all$OBSERVATION.DATE)

#all$week[all$week == 53] <- 52

#all$weekdate <- as.Date(paste(year(all$OBSERVATION.DATE), all$week, 1, sep="-"), "%Y-%U-%u")

#allist1cs <- all %>% group_by(weekdate) %>% summarise(ls = n()) %>%  mutate(cumls = round(cumsum(ls)/1000000,2))

#allist <- all %>% group_by(SAMPLING.EVENT.IDENTIFIER) %>% slice(1) %>% select(SAMPLING.EVENT.IDENTIFIER,group.id,LATITUDE,LONGITUDE,ALL.SPECIES.REPORTED,OBSERVATION.DATE,upload.month,year.month)

rm(all)

#allist <- allist[!duplicated(allist),]

#write.csv(allist,'C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/All-eBird-India-lists-2018-01-17.csv')



allist <- read.csv('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/All-eBird-India-lists-2018-01-17.csv', stringsAsFactors = F)
  
allist1 <- allist %>% filter(year.month >= '2010-01')
  
allist1$week <- week(allist1$OBSERVATION.DATE)
  
allist1$week[allist1$week == 53] <- 52

allist1$weekdate <- as.Date(paste(year(allist1$OBSERVATION.DATE), allist1$week, 1, sep="-"), "%Y-%U-%u")

allist1$lab <- paste(year(allist1$weekdate),month(allist1$weekdate, label = T, abbr = T), sep = ' ')

allist1 <- left_join(allist1,allist1cs) %>% arrange(weekdate)

allist1 <- left_join(data.frame(weekdate = c(unique(allist1$weekdate),max(allist1$weekdate)+c(1:20))),allist1)

allist1$cumls[is.na(allist1$cumls)] <- max(allist1$cumls,na.rm = T)

allist1$lab[is.na(allist1$lab)] <- paste(year(max(allist1$weekdate)),month(max(allist1$weekdate), label = T, abbr = T), sep = ' ')

allist1$cumls <- format(allist1$cumls, nsmall = 2)

p1 <- basemap + 
  geom_point(data = allist1, aes(x = LONGITUDE,y = LATITUDE, frame = weekdate, cumulative = TRUE), color = 'darkgoldenrod1', size = .7, alpha = 0.05)+
  geom_point(data = allist1, aes(x = LONGITUDE,y = LATITUDE, frame = weekdate),color = 'firebrick1', size = 2, alpha = 0.02)+
  geom_text(data = allist1,aes(x = 87, y = 36, label = lab, frame = weekdate), size = 9, hjust = 0, color = 'gray80') + 
  geom_text(data = allist1,aes(x = 87, y = 34, label = paste0(cumls,' Million Records'), frame = weekdate), size = 7, hjust = 0, color = 'gray80')

gganimate(p1, interval = .15, filename  = 'C:/Users/Abinand Reddy/Desktop/test3.gif',title_frame = F,ani.width=800, ani.height=800)



#indmap1 <- readOGR('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/Shape files/India2011-fully-reduced','India_2011GEO')

#plot(indmap1)

allist <- allist %>% group_by(group.id) %>% slice(1)

allist$year <- year(allist$OBSERVATION.DATE)

allist$month <- month(allist$OBSERVATION.DATE)

allist1 <- allist %>% dplyr::select(year,month, LONGITUDE,LATITUDE) %>% filter(year < 2018)

#load('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/All-eBird-India-data-2018-01-17.RData')

species <- c('Yellow-billed Babbler','Cattle Egret','Indian Peafowl','Ashy Prinia','Malabar Whistling-Thrush','Common Myna','Black Kite','Malabar Gray Hornbill','Vernal Hanging-Parrot','Barn Owl', 'Malabar Parakeet', 'White-cheeked Barbet', 'Orange Minivet', 'Southern Hill Myna', 'Rosy Starling' , 'Indian Paradise-Flycatcher','Indian Pitta','Verditer Flycatcher','White-browed Wagtail','Gray Wagtail','Rosy Starling','Ashy Woodswallow')

allsp <- all %>% filter(COMMON.NAME %in% species)

unique(allsp$COMMON.NAME)

rm(all)

allsp1 <- allsp %>% filter(ALL.SPECIES.REPORTED == 1) %>% group_by(COMMON.NAME,group.id) %>% slice(1) %>% ungroup() %>% dplyr::select(COMMON.NAME,SAMPLING.EVENT.IDENTIFIER,group.id,LATITUDE,LONGITUDE,ALL.SPECIES.REPORTED,OBSERVATION.DATE,upload.month,year.month)

allsp1$year <- year(allsp1$OBSERVATION.DATE)

allsp1$month <- month(allsp1$OBSERVATION.DATE)

###############
library(raster)


list_to_raster <- function(year, species = NA,gridsize = .5, map = indmap1){
  ifelse(is.na(species),lists <- allist1[allist1$year <= year,] , lists <- allsp1[allsp1$year <= year & allsp1$COMMON.NAME == species,])
  print(c(year,species))
  coordinates(lists) <-  ~ LONGITUDE+LATITUDE
  r <- raster(map)
  res(r) <- gridsize
  r <- rasterize(map, r)
  nc <- rasterize(coordinates(lists), r, fun='count', background=0)
  return(nc)
}

year_raster <- data.frame(year = 2010:2018)

allraster <- year_raster %>% group_by(year) %>% do(year_raster = list_to_raster(.$year[1]))

spmap <- expand.grid(species = species, year = 2010:2018)

spmap <- spmap %>% group_by(species,year) %>% do(spec_raster = list_to_raster(.$year,.$species))

spmap1 <- left_join(spmap, allraster)

spmap1$prop <- mapply('/',spmap1$spec_raster,spmap1$year_raster,SIMPLIFY = F)

spmap1 <- spmap1 %>% group_by(year) %>% rowwise() %>% do(prop = mask(.$prop,indmap1))

spmap2 <- spmap1 %>% rowwise() %>% do(tdf = as.data.frame(as(.$prop,"SpatialPixelsDataFrame")))

spmap2$year <- 2010:2018

spmap2$species <- spmap$species

spmap2 <- spmap2 %>% unnest()

spmap2$id <- paste(spmap2$x,spmap2$y, sep = '-')

spmap2$layer = spmap2$layer*100


for(i in species){
  
  spmap2_sp <- spmap2 %>% dplyr::filter(species == i)
  print(i)
  spmap2_edit <- spmap2_sp %>%
    arrange(year) %>%
    rename(X=x,Y=y,time=year,id=id) %>%
    mutate(ease="linear")
  
  spmap2_tween <- tween_elements(spmap2_edit,
                                 "time", "id", "ease", nframes = 50) %>%
    mutate(year = round(time), id = .group) %>%
    left_join(spmap2_sp, by=c("year","id", 'species'))
  
  p2 <- basemap + geom_tile(data = spmap2_tween, aes(X,Y,fill = layer.x, frame = .frame),color=NA) +
    scale_fill_gradient(low="gray30",high="goldenrod1", name = 'Frequency %') + 
    geom_path(data = fortify(indmap1),aes(x = long,y = lat, group = group))+
    geom_text(aes(x = 85, y = 36, label = i), size = 9, hjust = 0,colour = 'gray80') + 
    geom_text(data = spmap2_tween,aes(x = 85, y = 34, label = year, frame = .frame), size = 7, hjust = 0,colour = 'gray80')
  
  
  gganimate(p2, interval = .2, filename  = paste0('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/ebird summaries 2017/suhel poster/tween_',i,'.gif'),title_frame = F,ani.width=800, ani.height=800)
}


library(rgeos)


allsp1 <- allsp1 %>% filter(year < 2018)


list_to_raster_month <- function(month, species = NA,gridsize = .5, map = indmap1){
  ifelse(is.na(species),lists <- allist1[allist1$month == month,] , lists <- allsp1[allsp1$month == month & allsp1$COMMON.NAME == species,])
  print(c(month,species))
  coordinates(lists) <-  ~ LONGITUDE+LATITUDE
  r <- raster(map)
  res(r) <- gridsize
  r <- rasterize(map, r)
  nc <- rasterize(coordinates(lists), r, fun='count', background=0)
  return(nc)
}

month_raster <- data.frame(month = 1:12)

allraster <- month_raster %>% group_by(month) %>% do(month_rast = list_to_raster_month(.$month[1]))

spmap <- expand.grid(species = species, month = 1:12)

spmap <- spmap %>% group_by(species,month) %>% do(spec_raster = list_to_raster_month(.$month,.$species))

spmap1 <- left_join(spmap, allraster)



spmap1$prop <- mapply('/',spmap1$spec_raster,spmap1$month_rast,SIMPLIFY = F)

spmap1 <- spmap1 %>% group_by(month) %>% rowwise() %>% do(prop = mask(.$prop,indmap1))

spmap2 <- spmap1 %>% rowwise() %>% do(tdf = as.data.frame(as(.$prop,"SpatialPixelsDataFrame")))

spmap2$month <- 1:12

spmap2$species <- spmap$species

spmap2 <- spmap2 %>% unnest()

spmap2$id <- paste(spmap2$x,spmap2$y, sep = '-')

spmap2$layer = spmap2$layer*100

plot(spmap1$prop[[1]])

for(i in species){
  
  spmap2_sp <- spmap2 %>% dplyr::filter(species == i)
  print(i)
  spmap2_edit <- spmap2_sp %>%
    arrange(month) %>%
    rename(X=x,Y=y,time=month,id=id) %>%
    mutate(ease="linear")
  
  spmap2_tween <- tween_elements(spmap2_edit,
                                 "time", "id", "ease", nframes = 50) %>%
    mutate(month = round(time), id = .group) %>%
    left_join(spmap2_sp, by=c("month","id", 'species'))
  
  p2 <- basemap + geom_tile(data = spmap2_tween, aes(X,Y,fill = layer.x, frame = .frame),color=NA) +
    scale_fill_gradient(low="gray30",high="goldenrod1", name = 'Frequency %') + 
    geom_path(data = fortify(indmap1),aes(x = long,y = lat, group = group))+
    geom_text(aes(x = 85, y = 36, label = i), size = 9, hjust = 0,colour = 'gray80') + 
    geom_text(data = spmap2_tween,aes(x = 85, y = 34, label = month.name[month], frame = .frame), size = 7, hjust = 0,colour = 'gray80')
  
  
  gganimate(p2, interval = .2, filename  = paste0('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/ebird summaries 2017/suhel poster/month_tween_',i,'.gif'),title_frame = T,ani.width=800, ani.height=800)
}


sptest <- allsp1 %>% filter(COMMON.NAME == 'Common Myna')

library(MCPMod)

plot(indmap1)
points(sptest$LONGITUDE[sptest$year < 2010],sptest$LATITUDE[sptest$year < 2010])

MCPMod::mcp

