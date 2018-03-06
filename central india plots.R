library(dplyr)
library(sp)
library(rgdal)
library(ggplot2)
library(gridExtra)

cim <- readOGR('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/District shape file eBird code','Distrits with eBird code')

cim <- cim[cim@data$ST_NM %in% c('Chhattisgarh'),]

central <- all %>% filter(SUBNATIONAL1_CODE == 'IN-CT')

central1 <- central %>% filter(APPROVED == 1, ALL.SPECIES.REPORTED == 1,DURATION.MINUTES > 10, !CATEGORY %in% c('spuh','slash'))

stt <- readOGR('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/IndiaStates_2011','IndiaStates_2011')

stt <- stt[stt@data$ST_NM %in% c('Chhattisgarh'),]

proj4string(stt) <- proj4string(cim)

central.lists <- central1 %>% group_by(SUBNATIONAL2_CODE) %>% summarise(nlists = n_distinct(group.id))

cim@data$district10 <- factor(cim@data$district10)

cimdf <- fortify(cim, region = 'district10')

cimdf.lists <- left_join(cimdf,central.lists, by = c("id" = "SUBNATIONAL2_CODE"))

cimdf.lists$nlists.disc <- cut(cimdf.lists$nlists,breaks = c(0,10,50,100,500,2500))

par(mfrow = c(2,1))

ls <- ggplot() +
  geom_polygon(data = cimdf.lists, aes(x=long, y=lat, group=group, fill = nlists.disc), colour = 'black') +
  scale_fill_brewer(palette = 'YlGnBu', labels = c('0-10','10-50','50-100','100-500','500-2500'), name = 'Number of Lists')+
  geom_path(data = fortify(cim), aes(x=long, y=lat, group=group)) +
  geom_path(data = fortify(stt), aes(x=long, y=lat, group=group), size = 1.05) +
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
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  coord_map()

ls


central.spec <- central1 %>% group_by(SUBNATIONAL2_CODE) %>% summarise(Richness = n_distinct(SCIENTIFIC.NAME))



cimdf.spec <- left_join(cimdf,central.spec, by = c("id" = "SUBNATIONAL2_CODE"))


sp <- ggplot() +
  geom_polygon(data = cimdf.spec, aes(x=long, y=lat, group=group, fill = Richness), colour = 'black') +
  scale_fill_continuous(low = 'yellow', high = 'firebrick2')+
  geom_path(data = fortify(cim), aes(x=long, y=lat, group=group)) +
  geom_path(data = fortify(stt), aes(x=long, y=lat, group=group), size = 1.05) +
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
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        panel.background = element_blank())+
  coord_map()


sp

grid.arrange(ls,sp, nrow = 2)
central1 %>% group_by(SUBNATIONAL1_CODE) %>% summarise(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER), richness = n_distinct(SPECIES.NAME))

################ species accumulation curves #########

dist_cum <- function(var)
  sapply(seq_along(var), function(x) length(unique(head(var, x))))

cumdat <- central1 %>% group_by(SUBNATIONAL2_CODE) %>% arrange(OBSERVATION.DATE) %>% mutate(lists = dist_cum(group.id), species = dist_cum(COMMON.NAME)) %>% select(SUBNATIONAL2_CODE, lists,species)

cumdat$District <- cim@data$DISTRICT[match(cumdat$SUBNATIONAL2_CODE,cim@data$district10)]

cumdat$cat <- cimdf.lists$nlists.disc[match(cumdat$SUBNATIONAL2_CODE,cimdf.lists$id)]


cumdat %>% group_by(SUBNATIONAL2_CODE) %>% summarise(nl = max(lists))

titles <- c('0 to 10 Lists','11 to 50 Lists','51 to 100 Lists','101 to 500 Lists','Above 500 Lists')

g <- NULL

for (i in 1:length(unique(cumdat$cat))){
   g[[i]] <- ggplot(data = subset(cumdat, cat == levels(cumdat$cat)[i]), aes(x = lists,y = species, colour = District)) + 
    geom_line(size = 1.2) + 
    theme_classic() + 
    xlim(0,300) +
    ylim(0,300) +
    theme(legend.position = c(.8,.52),legend.title = element_blank(), plot.title = (element_text(hjust = 0.5)), legend.text = element_text(size = 12)) +
    ggtitle(titles[i])
}

nas <- data.frame('No Lists' = cim@data$DISTRICT[!cim@data$DISTRICT %in% cumdat$District])

nastg <- tableGrob(nas, theme = ttheme_minimal(), rows = NULL)



grid.arrange(nastg,g[[1]],g[[2]],g[[3]],g[[4]],g[[5]], ncol = 3)




