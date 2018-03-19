library(ggplot2)
library(sp)
library(rgdal)
library(dplyr)

indmap <- readOGR('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/IndiaStates_2011','IndiaStates_2011')

load('C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/All-eBird-India-data-2017-12-06.RData')

all.rel <- all %>% select(LATITUDE,LONGITUDE,SAMPLING.EVENT.IDENTIFIER,OBSERVER.ID,COMMON.NAME,CATEGORY,upload.month)

rm(all)

all.lists <- all.rel %>% group_by(SAMPLING.EVENT.IDENTIFIER) %>% slice(1)

ggplot() + 
  geom_path(data = fortify(indmap),aes(x = long,y = lat, group = group)) +
  geom_point(data = all.lists, aes(x = LONGITUDE, y = LATITUDE), col = 'red ', alpha = .1, size = .1)+
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
        panel.background = element_blank(),
        legend.text = element_text(size = 15),
        legend.position = 'bottom')+
  coord_equal()


cum.obs <- all.rel %>% group_by(upload.month) %>% arrange(upload.month) %>%summarise(nobs = n()) %>% mutate(cumobs = cumsum(nobs)) %>% filter(upload.month >'2012-12')
xaxs <- c('2013-01-01','2014-01-01','2015-01-01','2016-01-01','2017-01-01','2018-01-01')


par(oma = c(0,2,0,0), bg = NA)
plot(cum.obs$cumobs~as.Date(paste0(cum.obs$upload.month,'-01')), typ = 'l', col = 'red',lwd = 4, xaxt = 'nan', yaxt = 'nan', xlab = NA, ylab = NA,bty = 'n', main = 'Growth in Observations from India')
axis(1,at = as.Date(xaxs),labels = c(2013:2018), cex.axis = 1.5, lwd = 2)
axis(2, at = seq(0,8000000,by = 2000000), labels = c('0','2 Million','4 Million','6 Million','8 Million'),las = 1, cex.axis = 1.5, lwd = 2)

length(unique(all.lists$OBSERVER.ID))

