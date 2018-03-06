library(dplyr)
library(ggplot2)
library(rgdal)
library(RColorBrewer)


month = '2016-05'
last_year_month = '2015-05'



load("C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/All-eBird-India-data-2016-07-06.RData")

states <- read.csv("C:/Users/Abinand Reddy/Desktop/Thesis/NCF/eBird/state_codes.csv")

first_upload <- all %>% group_by(FULL.NAME) %>% mutate(first = min(upload.month)) %>% select_('FULL.NAME', 'SUBNATIONAL1_CODE', 'first') %>% filter(row_number(first) == 1)

new_users <- first_upload %>% filter(first == month | first == last_year_month) %>% group_by(SUBNATIONAL1_CODE) %>% arrange(SUBNATIONAL1_CODE)

new_users$state <-  states$state[match(new_users$SUBNATIONAL1_CODE, states$SUBNATIONAL1_CODE)]

nrow(new_users %>% filter(first == month))


state_wise <- all %>% group_by(SUBNATIONAL1_CODE,upload.month) %>% summarise(npeople = n_distinct(OBSERVER.ID), nlists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) 

state_wise_two_months <- state_wise %>% filter(upload.month > '2016-04')

state_wise_two_months$state <- states$state[match(state_wise_two_months$SUBNATIONAL1_CODE, states$SUBNATIONAL1_CODE)]

state_wise_two_months1 <- with(state_wise_two_months,expand.grid(state = levels(factor(states$state)), upload.month = levels(factor(upload.month))))

state_wise_two_months2 <- merge(state_wise_two_months,state_wise_two_months1,all.y = T)

state_wise_two_months3 <- state_wise_two_months2 %>% arrange(upload.month, npeople)


state_wise_two_months3$npeople[is.na(state_wise_two_months3$npeople)== T] <- 0




p <- ggplot(state_wise_two_months3, aes(x = reorder(state, npeople), y= npeople, fill = upload.month))+
  geom_bar(stat = 'identity', position = position_dodge()) + 
  coord_flip()+
  theme_classic()
p



p <- ggplot(state_wise_two_months3, aes(x = reorder(state, nlists), y= nlists, fill = upload.month))+
  geom_bar(stat = 'identity', position = position_dodge()) + 
  coord_flip()+
  theme_classic()
p



state_wise_may <- state_wise_two_months %>% filter(upload.month == month)
state_wise_june <- state_wise_two_months %>% filter(upload.month == '2016-06')

#ind <- getData('GADM',country = 'IND', level = 1)


ind_dfj <- left_join(ind_df,state_wise_june, by = c('id' = 'state'))

p <- ggplot()+
  geom_polygon(data = ind_dfj, aes(x = long, y = lat, fill = nlists, group = group))+
  scale_fill_gradientn(colours = c('white', 'cornflowerblue', 'darkorchid4'), limits = c(1,2000), guide = guide_colorbar(title = 'Lists')) +
  geom_path(data = ind_dfj, aes(x = long, y = lat, group = group))+
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
p

ind_dfm <- left_join(ind_df,state_wise_may, by = c('id' = 'state'))

p <- ggplot()+
  geom_polygon(data = ind_dfm, aes(x = long, y = lat, fill = nlists, group = group))+
  scale_fill_gradientn(colours = c('white', 'cornflowerblue', 'darkorchid4'), limits = c(1,2000),guide = guide_colorbar(title = 'Lists')) +
  geom_path(data = ind_dfm, aes(x = long, y = lat, group = group))+
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
p





review <- all %>% group_by(SUBNATIONAL1_CODE, month = upload.month > '2016-04') %>% summarise(reviews = sum(REVIEWED))

review <- all %>% group_by(REVIEWED,APPROVED) %>% summarise(n = n())

review

review$state <- states$state[match(review$SUBNATIONAL1_CODE,states$SUBNATIONAL1_CODE)]

review1 <- with(review, expand.grid(state = levels(factor(states$state)), month = levels(factor(month))))

review2 <- merge(review,review1, all.y = T)

review2$reviews[is.na(review2$reviews) == T ] <- 0

review2 <- review2 %>% group_by(state) %>% mutate(label_pos = sum(reviews)) %>% arrange(state) %>% mutate(label = last(reviews))


review2

p <- ggplot(review2, aes(x = reorder(state, reviews), y = reviews, fill = month))+
  geom_bar(stat = 'identity') +
  geom_text(aes(x = reorder(state, reviews), y = label_pos, label = label), hjust = -0.2)+
  xlab('')+
  coord_flip()+
  theme_classic()
p



user_activity <- all %>% 
  group_by(FULL.NAME) %>% 
  summarise(n_lists = n_distinct(SAMPLING.EVENT.IDENTIFIER[upload.month > '2016-02']),f_upload = min(upload.month, na.rm = T), l_upload = max(upload.month, na.rm = T)) %>%
  mutate(activity = ifelse(f_upload >= '2016-05','New', ifelse(l_upload >= '2016-05' & f_upload < '2016-05', 'Active', ifelse(l_upload < '2016-05' & n_lists >= 10, 'Short Inactive', 'Long Inactive' )))) %>%
  group_by(activity) %>%
  summarise(nums = n()) 


user_activity

u <- ggplot(user_activity, aes(x = reorder(State, total), y = nums, fill = activity))+
  geom_bar(stat = 'identity')+
  coord_flip()+
  theme_classic()

u


number_lists <- all %>% filter(upload.month > '2013-12'& upload.month < '2016-06') %>% group_by(upload.month) %>% summarise(n_lists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

number_lists$year <- unlist(lapply(number_lists$upload.month, function(x) unlist(strsplit(x, split = '-'))[1]))

number_lists$month <- unlist(lapply(number_lists$upload.month, function(x) unlist(strsplit(x, split = '-'))[2]))

number_lists1 <- with(number_lists, expand.grid(month = levels(factor(number_lists$month)), year = levels(factor(number_lists$year))))

number_lists2 <- merge(number_lists,number_lists1, all.y = T)


l <- ggplot(number_lists2, aes(x = month, y = n_lists, color = year, group = year, shape = year))+
  geom_line() +
  geom_point() +
  ylab('Number of lists')+
  theme_classic()
l


usage <-  all %>% group_by(FULL.NAME) %>% summarise(nlists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>% arrange(desc(nlists))

hist(usage$nlists, breaks = 500)



user_activity <- all %>% filter(upload.month < '2016-07') %>% group_by(FULL.NAME) %>% mutate(f_upload = min(upload.month, na.rm = T)) %>% ungroup() %>% group_by(upload.month >= '2016-03', upload.month == '2016-06',f_upload == '2016-06' ) %>% summarise(n = n_distinct(FULL.NAME))

user_activity

all1 <- all %>% filter(upload.month < '2016-07') %>% group_by(FULL.NAME) %>% mutate(f_upload = min(upload.month,na.rm = T))


us_act <- rbind(c('New Users June 2016', length(unique(all1$FULL.NAME[all1$f_upload == '2016-06']))),
      c('All Users June 2016', length(unique(all1$FULL.NAME[all1$upload.month == '2016-06']))),
      c('All Users since March 2016', length(unique(all1$FULL.NAME[all1$upload.month >= '2016-03']))),
      c('All Users', length(unique(all1$FULL.NAME))))


us_act

length(unique(all1$FULL.NAME[all1$upload.month >= '2016-03']))



