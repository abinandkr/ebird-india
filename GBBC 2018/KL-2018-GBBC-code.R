library(tidyverse)
library(rgdal)

setwd('github repos/ebird-india')

indmap <- readOGR('District shape file eBird code','Distrits with eBird code')
kl <- indmap[indmap$ST_NM == 'Kerala',]

setwd('GBBC 2018')

lists <- read.csv('KL-2018-GBBC-lists.csv', stringsAsFactors = F)
valdat <- read.csv('KL-2018-GBBC-valid.csv', stringsAsFactors = F)



kl.basemap <- ggplot()+
  geom_polygon(data = fortify(kl), aes(x = long,y = lat, group = group), fill = 'grey90', col = 'black')+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  coord_map()


listplot <- kl.basemap + 
  geom_point(data = lists, aes( x = LONGITUDE, y = LATITUDE), col = 'darkorchid', alpha = .5, pch = 16)+
  geom_path(data = fortify(kl), aes(x = long,y = lat, group = group)) 


valdatsum <- valdat %>% filter(ALL.SPECIES.REPORTED == 1, !CATEGORY %in%  c('spuh','slash')) %>% group_by(SUBNATIONAL2_CODE) %>% summarise(richness = n_distinct(SPECIES.NAME), users = n_distinct(OBSERVER.ID), lists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

valdatsum$lists_discrete <- cut(valdatsum$lists,breaks = c(0,10,50,100,500,1500))
valdatsum$richness_discrete <- cut(valdatsum$richness,breaks = c(0,50,100,150,200,250))
valdatsum$users_discrete <- cut(valdatsum$users,breaks = c(0,10,20,30,40,50,60))

kldf <- fortify(kl,region = 'district10')

kldf <- left_join(kldf,valdatsum, by = c('id'= "SUBNATIONAL2_CODE"))

kl.listmap <- kl.basemap + 
  geom_polygon(data = kldf, aes(x = long, y = lat, group = group, fill = lists_discrete), col = 'black')+
  scale_fill_brewer(palette = 'RdPu', name = 'Lists', labels = c('10-50','50-100','100-500','>500'))
  
kl.richmap <- kl.basemap + 
  geom_polygon(data = kldf, aes(x = long, y = lat, group = group, fill = richness_discrete), col = 'black') +
  scale_fill_brewer(palette = 'YlOrRd', name = 'Species Recorded',labels = c('<50','50-100','100-150','150-200','>200'))

kl.usermap <- kl.basemap + 
  geom_polygon(data = kldf, aes(x = long, y = lat, group = group, fill = users_discrete), col = 'black') +
  scale_fill_brewer(palette = 'YlGn', name = 'Users', labels = c('<10','10-20','20-30','30-40','40-50','50-60'))
  

listplot
kl.listmap
kl.richmap
kl.usermap

##################################### Grid maps
library(raster)

list_to_raster <- function(species = NA,gridsize = .25, map = kl){
  ifelse(is.na(species),list <- lists,list <- valdat %>% filter(COMMON.NAME == species) %>% group_by(group.id) %>% slice(1))
  coordinates(list) <-  ~ LONGITUDE+LATITUDE
  r <- raster(map)
  res(r) <- gridsize
  r <- rasterize(map,r)
  nc <- rasterize(coordinates(list), r, fun = 'count', background=0)
  return(nc)
}

#coordinates(lists) <- ~ LONGITUDE+LATITUDE

gridlists <- list_to_raster()


gridlists1 <- as.data.frame(as(gridlists,'SpatialPixelsDataFrame'))



species <- c('White-cheeked Barbet',
             'House Crow',
             'Common Myna',
             'Asian Koel',
             'Red-whiskered Bulbul',
             'Oriental Magpie-Robin',
             'Large-billed Crow',
             'Purple-rumped Sunbird',
             'Rufous Treepie',
             'Rock Pigeon')


specrast <- data.frame(species= species)

specrast <- specrast %>% rowwise() %>% do(rast = list_to_raster(.$species))

specrast1 <- specrast %>% rowwise %>% do(prop = .$rast/gridlists*100)

box <- as(extent(kl@bbox),'SpatialPolygons')
proj4string(box) <- proj4string(kl)
klneg <- box-kl

library(RColorBrewer)

for(i in 1:10){
specrastdf <- as.data.frame(as(specrast1$prop[[i]],'SpatialPixelsDataFrame'))
specrastdf$frequency = cut(specrastdf$layer, c(-1,0,20,40,60,80,100))
mycols <- c('#FFFFFF',brewer.pal(n = 5, 'RdPu'))
sprastmap <- kl.basemap + 
  geom_tile(data = specrastdf, aes(x=x,y=y, fill = frequency)) +
  scale_fill_manual(values = mycols, guide = F)+
  geom_polygon(data = fortify(klneg),aes(x = long, y = lat, group = group), fill = 'white')+
  geom_path(data = fortify(kl), aes(x = long,y = lat, group = group), lwd = .8) 

#png(paste0('/github repos/ebird-india/GBBC 2018/gridmaps/',species[i],'.png'),width = 700,height = 1200)
print(sprastmap)
#dev.off()
}

mycols2 <- c(brewer.pal(n=5,name = 'Blues'))


gridlists1$list <- cut(gridlists1$layer,c(0,10,50,100,500,1000))



sprastmap <- kl.basemap + 
  geom_polygon(data = fortify(kl),aes(x = long, y = lat, group = group), fill = 'white')+
  geom_tile(data = gridlists1, aes(x=x,y=y, fill = list), col = 'black') +
  scale_fill_manual(values =  mycols2, name = 'Lists\n', breaks = c(NA,levels(gridlists1$list)),labels = c('  0','  <10','  20-50','  50-100','  100-500','  >500'))+
  geom_polygon(data = fortify(klneg),aes(x = long, y = lat, group = group), fill = 'white')+
  geom_path(data = fortify(kl), aes(x = long,y = lat, group = group), lwd = .8) +
  theme(legend.key.size = unit(1.5,'lines'),legend.title = element_text(size = 20), legend.text = element_text(size = 12))



sprastmap


list_to_rasterrichness <- function(species = NA,gridsize = .25, map = kl){
  ifelse(is.na(species),list <- lists,list <- valdat %>% filter(COMMON.NAME == species) %>% group_by(group.id) %>% slice(1))
  coordinates(list) <-  ~ LONGITUDE+LATITUDE
  r <- raster(map)
  res(r) <- gridsize
  r <- rasterize(map,r)
  nc <- rasterize(coordinates(list), r, fun = 'min', background=0)
  return(nc)
}




richrast <- raster(kl)
res(richrast) <- .25
values(richrast) <- 0

valdat1 <- valdat %>% filter(ALL.SPECIES.REPORTED == 1, !CATEGORY %in%  c('spuh','slash'))

for (i in unique(valdat1$COMMON.NAME)){
  r1 <- list_to_rasterrichness(i)
  r1[r1>0] <- 1   
  richrast <- richrast + r1
}

richrast1 <- as.data.frame(as(richrast,'SpatialPixelsDataFrame'))


richrast1$species <- cut(richrast1$layer,c(0,25,50,75,100,125,150,200))

sprastmap1 <- kl.basemap + 
  geom_polygon(data = fortify(kl),aes(x = long, y = lat, group = group), fill = 'white')+
  geom_tile(data = richrast1, aes(x=x,y=y, fill = species), col = 'black') +
  scale_fill_brewer(palette = 'Reds', labels = c('  Insufficient data','  <25','  25-50','  50-75','  75-100','  100-125','  125-150','  >150'), name = 'Species\n', breaks = c(NA,levels(richrast1$species)))+
  geom_polygon(data = fortify(klneg),aes(x = long, y = lat, group = group), fill = 'white')+
  geom_path(data = fortify(kl), aes(x = long,y = lat, group = group), lwd = .8) + 
  theme(legend.key.size = unit(1.5,'lines'),legend.title = element_text(size = 20), legend.text = element_text(size = 12))




sprastmap1


g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

spleg <- g_legend(sprastmap1)
lsleg <- g_legend(sprastmap)
plot(spleg)
plot(lsleg)


png('species_legend.png',width = 400,height = 700,res = 200)
plot(spleg)
dev.off()

png('list_legend.png',width = 400,height = 700,res = 200)
plot(lsleg)
dev.off()
