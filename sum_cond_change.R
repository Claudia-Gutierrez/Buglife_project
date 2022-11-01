
library (dplyr)
library (ggplot2)

options(scipen = 100)

hist(allpoll$dist)

hist_info <- hist(allpoll$dist, breaks= c(10^seq(-3.5,2,0.1)))  

hist_info$density <- hist_info$counts /    # Compute density values
  sum(hist_info$counts)
plot(hist_info, freq = FALSE)

breaks<-cbind.data.frame(hist_info$breaks)

breaks<-as.data.frame(breaks[-c(1), ])

percentage<-cbind.data.frame(breaks,hist_info$density)
colnames(percentage)<- c("disp_dist","percentage")

percentage<- percentage %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 13)))
percentage<-percentage[c(17:40),]
percentage$X<-as.numeric(c(1:24))

cond_216<- as.data.frame(read.csv("conductance/test_216diff.csv"))

cond_216<- cond_216 %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 18)))


cond_216<-right_join(percentage, cond_216, by='X')

cond_216$perc_change.GCB<- ((cond_216$after.GCB-cond_216$before.GCB)/cond_216$before.GCB)


cond_216$weight_change<-cond_216$perc_change.GCB*cond_216$percentage

site216<-as.data.frame(sum(cond_216$weight_change))
site216$site<- 216
colnames(site216)<- c("sum_weight_change","site")

wieghted_change<-rbind(site180,site189,site190,site207,site209, site215,site216)




