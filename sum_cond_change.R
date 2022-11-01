################################################
#                                              #
#       Sum of speed weighted by species       #
#                                              #
################################################

#This script calculates the proportional change of speed before and after intervention
#It quantifies the percentage of species that fall within dispersal distance bins 

#INPUTS:
# allpoll - data frame with the all the species and their respective estimated dispersal distance
# test_Xdiff.csv- csv obtained after running Condatis analysis which contains speed before and after habitat regeneration per site

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

cond_180<- as.data.frame(read.csv("conductance/test_3kdiff.csv"))

cond_180<- cond_180 %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 18)))


cond_180<-right_join(percentage, cond_180, by='X')

cond_180$perc_change.GCB<- ((cond_180$after.GCB-cond_180$before.GCB)/cond_180$before.GCB)


cond_180$weight_change<-cond_180$perc_change.GCB*cond_180$percentage

site180<-as.data.frame(sum(cond_180$weight_change))
site180$site<- 180
colnames(site180)<- c("sum_weight_change","site")

weighted_change<-rbind(site180,site189,site190,site207,site209, site215,site216)


write.csv(weighted_change, "conductance/weighted_change.csv")



