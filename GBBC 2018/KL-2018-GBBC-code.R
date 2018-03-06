library(tidyverse)
library(rgdal)

setwd('github repos/ebird-india')

indmap <- readOGR('District shape file eBird code','Distrits with eBird code')
kl <- indmap[indmap$ST_NM == 'Kerala',]

setwd('GBBC 2018')

lists <- read.csv('KL-2018-GBBC-lists.csv', stringsAsFactors = F)
valdat <- read.csv('KL-2018-GBBC-valid.csv', stringsAsFactors = F)



kl.basemap <- ggplot()+
  geom_polygon(data = fortify(kl), aes(x = long,y = lat, group = group), fill = 'ivory', col = 'black')+
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

