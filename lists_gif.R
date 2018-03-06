library(dplyr)
library(leaflet)
library(rgdal)
library(ggplot2)
library(ggmap)
library(animation)
library(gganimate)
library(rgeos)
library(grid)

load('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/All-eBird-India-data-2017-01-06.RData')

all1 <- all %>% filter(upload.month > '2009-12') %>% group_by(SAMPLING.EVENT.IDENTIFIER) %>% slice(1) %>% ungroup() %>% select(LONGITUDE,LATITUDE,upload.month,no.sp,SAMPLING.EVENT.IDENTIFIER) 

ind <- readOGR('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/IndiaStates_2011','IndiaStates_2011')

ind1 <- gSimplify(ind, tol = .01, topologyPreserve = T)

f_ind <- fortify(ind1)

all2 <- all1 %>% group_by(upload.month) %>% summarise(nosp = sum(no.sp)) %>% ungroup()

all2$nosp <- as.numeric(all2$nosp)

all2$nosp[is.na(all2$nosp)] <- 0

all2$cu.sp <- cumsum(all2$nosp)

all3 <- all1 %>% group_by(upload.month) %>% summarise(nols = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% ungroup()

all3$nols <- as.numeric(all3$nols)

all3$cu.ls <- cumsum(all3$nols)

all4 <- left_join(all2,all3) 

all4 <- all4 %>% select(upload.month, cu.ls,cu.sp)

all4$cu.sp <- as.character(round(all4$cu.sp/1000000,1))


ind <- ggplot() + 
  geom_polygon(data = f_ind, aes(x = long, y = lat, group = group)) + 
  geom_path(data = f_ind, aes(x = long, y = lat, group = group), color = 'gray61') + 
  expand_limits(y = 2) +
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.margin = unit(c(0,0,0,0),"mm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = 'gray30'))+
  coord_map()


all1$upload.month <- factor(all1$upload.month)

saveGIF({
  
  ani.options(interval = 0.2)
  
  for (i in levels(all1$upload.month)){

    print(i)
    
    all3 <- all1[all1$upload.month == i,]
    
    if(i != min(levels(all1$upload.month))){
      
      tt1 <- geom_point(data = all3, aes(x = LONGITUDE, y = LATITUDE), color = 'darkgoldenrod1', size = 2, alpha = 0.01)
      
      tt2 <- geom_point(data = all3, aes(x = LONGITUDE, y = LATITUDE), color = 'darkgoldenrod1', size = 4, alpha = 0.01)
      
      print(ind + t2 + tt1 + annotate('text', x = 89, y = 37, label = i, size = 8, colour = 'gray80', hjust = 0) +
              annotate('text', x = 89, y = 35, label = paste0(all4$cu.sp[all4$upload.month == i][1],' Million Records'), size = 5, colour = 'gray80', hjust = 0)+
              annotate('text', x = 68, y = 3.5, label = 'eBird India', size = 8, colour = 'gray80', hjust = 0)) 
      
      print(ind + t1 + tt2 + annotate('text', x = 89, y = 37, label = i, size = 8, colour = 'gray80', hjust = 0) +
              annotate('text', x = 89, y = 35, label = paste0(all4$cu.sp[all4$upload.month == i][1],' Million Records'), size = 5, colour = 'gray80', hjust = 0)+
              annotate('text', x = 68, y = 3.5, label = 'eBird India', size = 8, colour = 'gray80', hjust = 0)) 
      
      
      ind <- ind + geom_point(data = all3, aes(x = LONGITUDE, y = LATITUDE), color = 'darkgoldenrod1', size = 1, alpha = .1)
    }
    
    
    t1 <- geom_point(data = all3, aes(x = LONGITUDE, y = LATITUDE), color = 'darkgoldenrod1', size = 2, alpha = 0.01)    
    
    t2 <- geom_point(data = all3, aes(x = LONGITUDE, y = LATITUDE), color = 'darkgoldenrod1', size = 4, alpha = 0.01)
    
    t3 <- geom_point(data = all3, aes(x = LONGITUDE, y = LATITUDE), color = 'darkgoldenrod1', size = 6, alpha = 0.01)
    
    if( i == min(levels(all1$upload.month))){
      print(ind + t1 + annotate('text', x = 89, y = 37, label = i, size = 8, colour = 'gray80', hjust = 0) +
            annotate('text', x = 89, y = 35, label = paste0(all4$cu.sp[all4$upload.month == i][1],' Million Records'), size = 5, colour = 'gray80', hjust = 0) +
              annotate('text', x = 68, y = 3.5, label = 'eBird India', size = 8, colour = 'gray80', hjust = 0))
    
    
      print(ind + t2 + annotate('text', x = 89, y = 37, label = i, size = 8, colour = 'gray80', hjust = 0) +
            annotate('text', x = 89, y = 35, label = paste0(all4$cu.sp[all4$upload.month == i][1],' Million Records'), size = 5, colour = 'gray80', hjust = 0)+
              annotate('text', x = 68, y = 3.5, label = 'eBird India', size = 8, colour = 'gray80', hjust = 0))
    }
  
    print(ind + t3 + annotate('text', x = 89, y = 37, label = i, size = 8, colour = 'gray80', hjust = 0) +
            annotate('text', x = 89, y = 35, label = paste0(all4$cu.sp[all4$upload.month == i][1],' Million Records'), size = 5, colour = 'gray80', hjust = 0)+
            annotate('text', x = 68, y = 3.5, label = 'eBird India', size = 8, colour = 'gray80', hjust = 0))


    
  }
  
  for ( j in c(1:10)){
    print(ind + annotate('text', x = 89, y = 37, label = i, size = 8, colour = 'gray80', hjust = 0) +
            annotate('text', x = 89, y = 35, label = paste0(all4$cu.sp[all4$upload.month == i][1],' Million Records'), size = 5, colour = 'gray80', hjust = 0)+
            annotate('text', x = 68, y = 3.5, label = 'eBird India', size = 8, colour = 'gray80', hjust = 0))
  }

},



interval = 0.1, movie.name = "lists5mill.gif",

ani.width = 450, ani.height = 600)

dev.off()


