library(dplyr)
library(sp)
library(rgdal)
library(ggplot2)
library(zoo)
library(gridExtra)
library(cowplot)

load("C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/All-eBird-India-data-2018-01-17.RData")

pongdat <- all %>% filter(year.month > '2017-12')

dat1 <- all %>% filter(year.month > '2015-12',year.month < '2018-01') %>% select(SUBNATIONAL1_CODE,SUBNATIONAL2_CODE,LATITUDE,LONGITUDE,upload.month,OBSERVATION.DATE,OBSERVER.ID,FULL.NAME,SAMPLING.EVENT.IDENTIFIER,DURATION.MINUTES,ALL.SPECIES.REPORTED,group.id,CATEGORY,SPECIES.NAME,APPROVED,REVIEWED)

rm(all)

dat <- dat1

dat$OBSERVATION.DATE <- as.Date(dat$OBSERVATION.DATE)

dat$Month <- factor(month(dat$OBSERVATION.DATE,label = T, abbr = T), levels = month.abb)

dat$Year <- as.character(year(dat$OBSERVATION.DATE))

dat_users <- dat %>% group_by(Year,Month) %>% summarise(Users = n_distinct(OBSERVER.ID)) 

g_users <- ggplot(data = dat_users, aes(x = Month, y = Users, colour = factor(Year), group = Year)) + geom_line(size = 1.3) + geom_point(size = 2) + theme_classic() + labs(colour = '') + ggtitle('Users') + theme(plot.title = element_text(hjust = .5)) 

dat_checklists <- dat %>% group_by(Year,Month) %>% summarise(Lists = n_distinct(SAMPLING.EVENT.IDENTIFIER)/1000) %>% group_by(Year) %>% mutate(cumLists = cumsum(Lists))

g_check <- ggplot(data = dat_checklists, aes(x = Month, y = Lists, colour = factor(Year), group = Year)) + geom_line(size = 1.3) + geom_point(size = 2) + theme_classic() + labs(colour = '')+ ylab('Lists x 1000') + ggtitle('Lists') + theme(plot.title = element_text(hjust = .5))

dat_obse <- dat %>% group_by(Year,Month) %>% summarise(Observations = n()/1000) %>% group_by(Year) %>% mutate(cumobs = cumsum(Observations)/1000)

g_obs <- ggplot(data = dat_obse, aes(x = Month, y = Observations, colour = factor(Year), group = Year)) + geom_line(size = 1.3) + geom_point(size = 2) + theme_classic() + labs(colour = '') + ylab('Observations x 1000') + ggtitle('Observations') + theme(plot.title = element_text(hjust = .5))

dat_time <- dat %>% group_by(Year,Month) %>% summarise(Time = sum(DURATION.MINUTES, na.rm = T)/60000) %>% group_by(Year) %>% mutate(cumtime = cumsum(Time)/1000)

g_time <- ggplot(data = dat_time, aes(x = Month, y = Time, colour = factor(Year), group = Year)) + geom_line(size = 1.3) + geom_point(size = 2) + theme_classic() + labs(colour = '') + ylab('Hours x 1000')+ ggtitle('Duration') + theme(plot.title = element_text(hjust = .5))

g_leg <- get_legend(g_users + theme(legend.position = 'bottom', legend.text = element_text(size = 15),legend.key.size = unit(2,'lines')))

g1 <- grid.arrange(g_users + theme(legend.position = 'none'),g_check + theme(legend.position = 'none'),g_obs + theme(legend.position = 'none'),g_time + theme(legend.position = 'none'),ncol = 2)

plot_grid(g1,g_leg,nrow = 2, rel_heights = c(0.95,.05))

png('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/ebird summaries 2017/duration_no.png',width = 1000,height = 700,res = 120)
g_time
dev.off()

dist_cum <- function(year,month){
t <- dat %>%  filter(Year == year, Month <= month) %>% summarise(dist = n_distinct(OBSERVER.ID))
return(as.numeric(t))
}

dat %>% filter(Year == 2016, Month < 'Dec') %>% summarise(cu = n_distinct(OBSERVER.ID))

dist_cum(2016,'Dec')

dat_cum_users <- dat %>% group_by(Year,Month) %>% do(cumusers = as.numeric(dist_cum(.$Year,.$Month))/1000) %>% unnest()

g_cum_users <- ggplot(data = dat_cum_users, aes(x = Month, y = cumusers, colour = factor(Year), group = Year)) + geom_line(size = 1.3) + geom_point(size = 2) + theme_classic() + labs(colour = '')+ ylab('Users x 1000') + ggtitle('Users') + theme(plot.title = element_text(hjust = .5)) 

g_cum_check <- ggplot(data = dat_checklists, aes(x = Month, y = cumLists, colour = factor(Year), group = Year)) + geom_line(size = 1.3) + geom_point(size = 2) + theme_classic() + labs(colour = '')+ ylab('Lists x 1000') + ggtitle('Lists') + theme(plot.title = element_text(hjust = .5))

g_cum_obs <- ggplot(data = dat_obse, aes(x = Month, y = cumobs, colour = factor(Year), group = Year)) + geom_line(size = 1.3) + geom_point(size = 2) + theme_classic() + labs(colour = '') + ylab('Observations (Millions)') + ggtitle('Observations') + theme(plot.title = element_text(hjust = .5))

g_cum_time <- ggplot(data = dat_time, aes(x = Month, y = cumtime, colour = factor(Year), group = Year)) + geom_line(size = 1.3) + geom_point(size = 2) + theme_classic() + labs(colour = '') + ylab('Hours (Millions)')+ ggtitle('Duration') + theme(plot.title = element_text(hjust = .5))

g_cum_leg <- get_legend(g_cum_obs + theme(legend.position = 'bottom', legend.text = element_text(size = 11),legend.key.size = unit(2,'lines')))

gcum1 <- grid.arrange(g_cum_users + theme(legend.position = 'none'),g_cum_check + theme(legend.position = 'none'),g_cum_obs + theme(legend.position = 'none'),g_cum_time + theme(legend.position = 'none'),ncol = 2)

plot_grid(gcum1,g_leg,nrow = 2, rel_heights = c(0.95,.05))

png('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/ebird summaries 2017/cumulative_combined.png',width = 1000,height = 700, res = 100)
plot_grid(gcum1,g_leg,nrow = 2, rel_heights = c(0.95,.05))
dev.off()


indmap <- readOGR('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/IndiaStates_2011','IndiaStates_2011')

plot(indmap)

datmap <- dat %>% group_by(group.id) %>% slice(1)

datunloc <- dat %>% group_by(Year, LATITUDE,LONGITUDE) %>% slice(1)

basemap <- ggplot() + 
  geom_path(data = fortify(indmap),aes(x = long,y = lat, group = group)) +
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
        plot.margin=unit(c(0,0,0,0)+2, "cm"),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 20,face = 'bold'),
        panel.background = element_blank())+
  coord_equal()

birdingmap <- basemap + geom_point(data = datmap, aes(x = LONGITUDE, y = LATITUDE), col = 'red ', alpha = .3, size = .4) + facet_grid(.~Year)

locmap <- basemap + geom_point(data = datunloc, aes(x = LONGITUDE, y = LATITUDE), col = 'red ', size = .5) + facet_grid(.~Year)


png('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/ebird summaries 2017/birding_map.png',width = 3000,height = 2000, res = 200)
birdingmap
dev.off()


geom_point(data = all.lists, aes(x = LONGITUDE, y = LATITUDE), col = 'red ', alpha = .1, size = .1)+
  