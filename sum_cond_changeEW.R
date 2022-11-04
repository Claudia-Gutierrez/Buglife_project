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
#NOTE: negative conductance values converted to epsilon value (2.220446e-16) to avoid errors 

library (dplyr)
library (ggplot2)


# Percentage of species per bin -------------------------------------------

optioconductance(scipen = 100)

hist(allpoll$dist)

hist_info <- hist(allpoll$dist, breaks= c(10^seq(-3.5,2,0.1)))  

hist_info$deconductanceity <- hist_info$counts /    # Compute deconductanceity values
  sum(hist_info$counts)
plot(hist_info, freq = FALSE)

breaks<-cbind.data.frame(hist_info$breaks)

breaks<-as.data.frame(breaks[-c(1), ])

percentage<-cbind.data.frame(breaks,hist_info$deconductanceity)
colnames(percentage)<- c("disp_dist","percentage")

percentage<- percentage %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 13)))
percentage<-percentage[c(17:40),]
percentage$X<-as.numeric(c(1:24))

#Sites---------------------------------------------------------------------
# 180 ---------------------------------------------------------------------
cond180ns<- as.data.frame(read.csv("conductance/test180ns_diff.csv"))

cond180ns<- cond180ns %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 18)))


cond180ns<-right_join(percentage, cond180ns, by='X')

cond180ns$before.GCB[cond180ns$before.GCB < 0] <- 2.220446e-16
cond180ns$after.GCB[cond180ns$after.GCB < 0] <- 2.220446e-16 

cond180ns$perc_change.GCB<- ((cond180ns$after.GCB-cond180ns$before.GCB)/cond180ns$before.GCB)


cond180ns$weight_change<-cond180ns$perc_change.GCB*cond180ns$percentage

site180<-as.data.frame(sum(cond180ns$weight_change))
site180$site<- 180
colnames(site180)<- c("sum_weight_change","site")

# 189 ---------------------------------------------------------------------
cond189ns<- as.data.frame(read.csv("conductance/test189ns_diff.csv"))

cond189ns<- cond189ns %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 18)))


cond189ns<-right_join(percentage, cond189ns, by='X')

cond189ns$before.GCB[cond189ns$before.GCB < 0] <- 2.220446e-16
cond189ns$after.GCB[cond189ns$after.GCB < 0] <- 2.220446e-16 

cond189ns$perc_change.GCB<- ((cond189ns$after.GCB-cond189ns$before.GCB)/cond189ns$before.GCB)


cond189ns$weight_change<-cond189ns$perc_change.GCB*cond189ns$percentage

site189<-as.data.frame(sum(cond189ns$weight_change))
site189$site<- 189
colnames(site189)<- c("sum_weight_change","site")

# 190 ---------------------------------------------------------------------
cond190ns<- as.data.frame(read.csv("conductance/test190ns_diff.csv"))

cond190ns<- cond190ns %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 18)))


cond190ns<-right_join(percentage, cond190ns, by='X')

cond190ns$before.GCB[cond190ns$before.GCB < 0] <- 2.220446e-16
cond190ns$after.GCB[cond190ns$after.GCB < 0] <- 2.220446e-16 

cond190ns$perc_change.GCB<- ((cond190ns$after.GCB-cond190ns$before.GCB)/cond190ns$before.GCB)


cond190ns$weight_change<-cond190ns$perc_change.GCB*cond190ns$percentage

site190<-as.data.frame(sum(cond190ns$weight_change))
site190$site<- 190
colnames(site190)<- c("sum_weight_change","site")

# 205 ---------------------------------------------------------------------
cond205ns<- as.data.frame(read.csv("conductance/test205ns_diff.csv"))

cond205ns<- cond205ns %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 18)))


cond205ns<-right_join(percentage, cond205ns, by='X')

cond205ns$before.GCB[cond205ns$before.GCB < 0] <- 2.220446e-16
cond205ns$after.GCB[cond205ns$after.GCB < 0] <- 2.220446e-16 

cond205ns$perc_change.GCB<- ((cond205ns$after.GCB-cond205ns$before.GCB)/cond205ns$before.GCB)


cond205ns$weight_change<-cond205ns$perc_change.GCB*cond205ns$percentage

site205<-as.data.frame(sum(cond205ns$weight_change))
site205$site<- 205
colnames(site205)<- c("sum_weight_change","site")

# 206 ---------------------------------------------------------------------
cond206ns<- as.data.frame(read.csv("conductance/test206ns_diff.csv"))

cond206ns<- cond206ns %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 18)))


cond206ns<-right_join(percentage, cond206ns, by='X')

cond206ns$before.GCB[cond206ns$before.GCB < 0] <- 2.220446e-16
cond206ns$after.GCB[cond206ns$after.GCB < 0] <- 2.220446e-16 

cond206ns$perc_change.GCB<- ((cond206ns$after.GCB-cond206ns$before.GCB)/cond206ns$before.GCB)


cond206ns$weight_change<-cond206ns$perc_change.GCB*cond206ns$percentage

site206<-as.data.frame(sum(cond206ns$weight_change))
site206$site<- 206
colnames(site206)<- c("sum_weight_change","site")

# 207 ---------------------------------------------------------------------
cond207ns<- as.data.frame(read.csv("conductance/test207ns_diff.csv"))

cond207ns<- cond207ns %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 18)))


cond207ns<-right_join(percentage, cond207ns, by='X')

cond207ns$before.GCB[cond207ns$before.GCB < 0] <- 2.220446e-16
cond207ns$after.GCB[cond207ns$after.GCB < 0] <- 2.220446e-16 

cond207ns$perc_change.GCB<- ((cond207ns$after.GCB-cond207ns$before.GCB)/cond207ns$before.GCB)


cond207ns$weight_change<-cond207ns$perc_change.GCB*cond207ns$percentage

site207<-as.data.frame(sum(cond207ns$weight_change))
site207$site<- 207
colnames(site207)<- c("sum_weight_change","site")

# 209 ---------------------------------------------------------------------
cond209ns<- as.data.frame(read.csv("conductance/test209ns_diff.csv"))

cond209ns<- cond209ns %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 18)))


cond209ns<-right_join(percentage, cond209ns, by='X')

cond209ns$before.GCB[cond209ns$before.GCB < 0] <- 2.220446e-16
cond209ns$after.GCB[cond209ns$after.GCB < 0] <- 2.220446e-16 

cond209ns$perc_change.GCB<- ((cond209ns$after.GCB-cond209ns$before.GCB)/cond209ns$before.GCB)


cond209ns$weight_change<-cond209ns$perc_change.GCB*cond209ns$percentage

site209<-as.data.frame(sum(cond209ns$weight_change))
site209$site<- 209
colnames(site209)<- c("sum_weight_change","site")

# 215 ---------------------------------------------------------------------
cond215ns<- as.data.frame(read.csv("conductance/test215ns_diff.csv"))

cond215ns<- cond215ns %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 18)))


cond215ns<-right_join(percentage, cond215ns, by='X')

cond215ns$before.GCB[cond215ns$before.GCB < 0] <- 2.220446e-16
cond215ns$after.GCB[cond215ns$after.GCB < 0] <- 2.220446e-16 

cond215ns$perc_change.GCB<- ((cond215ns$after.GCB-cond215ns$before.GCB)/cond215ns$before.GCB)


cond215ns$weight_change<-cond215ns$perc_change.GCB*cond215ns$percentage

site215<-as.data.frame(sum(cond215ns$weight_change))
site215$site<- 215
colnames(site215)<- c("sum_weight_change","site")

# 216 ---------------------------------------------------------------------
cond216ns<- as.data.frame(read.csv("conductance/test216ns_diff.csv"))

cond216ns<- cond216ns %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 18)))


cond216ns<-right_join(percentage, cond216ns, by='X')

cond216ns$before.GCB[cond216ns$before.GCB < 0] <- 2.220446e-16
cond216ns$after.GCB[cond216ns$after.GCB < 0] <- 2.220446e-16 

cond216ns$perc_change.GCB<- ((cond216ns$after.GCB-cond216ns$before.GCB)/cond216ns$before.GCB)


cond216ns$weight_change<-cond216ns$perc_change.GCB*cond216ns$percentage

site216<-as.data.frame(sum(cond216ns$weight_change))
site216$site<- 216
colnames(site216)<- c("sum_weight_change","site")

# 218 ---------------------------------------------------------------------
cond218ns<- as.data.frame(read.csv("conductance/test218ns_diff.csv"))

cond218ns<- cond218ns %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 18)))


cond218ns<-right_join(percentage, cond218ns, by='X')

cond218ns$before.GCB[cond218ns$before.GCB < 0] <- 2.220446e-16
cond218ns$after.GCB[cond218ns$after.GCB < 0] <- 2.220446e-16 

cond218ns$perc_change.GCB<- ((cond218ns$after.GCB-cond218ns$before.GCB)/cond218ns$before.GCB)


cond218ns$weight_change<-cond218ns$perc_change.GCB*cond218ns$percentage

site218<-as.data.frame(sum(cond218ns$weight_change))
site218$site<- 218
colnames(site218)<- c("sum_weight_change","site")

# 220 ---------------------------------------------------------------------
cond220ns<- as.data.frame(read.csv("conductance/test220ns_diff.csv"))

cond220ns<- cond220ns %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 18)))


cond220ns<-right_join(percentage, cond220ns, by='X')

cond220ns$before.GCB[cond220ns$before.GCB < 0] <- 2.220446e-16
cond220ns$after.GCB[cond220ns$after.GCB < 0] <- 2.220446e-16 

cond220ns$perc_change.GCB<- ((cond220ns$after.GCB-cond220ns$before.GCB)/cond220ns$before.GCB)


cond220ns$weight_change<-cond220ns$perc_change.GCB*cond220ns$percentage

site220<-as.data.frame(sum(cond220ns$weight_change))
site220$site<- 220
colnames(site220)<- c("sum_weight_change","site")


# 221 ---------------------------------------------------------------------
cond221ns<- as.data.frame(read.csv("conductance/test221ns_diff.csv"))

cond221ns<- cond221ns %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 18)))


cond221ns<-right_join(percentage, cond221ns, by='X')

cond221ns$before.GCB[cond221ns$before.GCB < 0] <- 2.220446e-16
cond221ns$after.GCB[cond221ns$after.GCB < 0] <- 2.220446e-16 

cond221ns$perc_change.GCB<- ((cond221ns$after.GCB-cond221ns$before.GCB)/cond221ns$before.GCB)


cond221ns$weight_change<-cond221ns$perc_change.GCB*cond221ns$percentage

site221<-as.data.frame(sum(cond221ns$weight_change))
site221$site<- 221
colnames(site221)<- c("sum_weight_change","site")


# 231 ---------------------------------------------------------------------
cond231ns<- as.data.frame(read.csv("conductance/test231ns_diff.csv"))

cond231ns<- cond231ns %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 18)))


cond231ns<-right_join(percentage, cond231ns, by='X')

cond231ns$before.GCB[cond231ns$before.GCB < 0] <- 2.220446e-16
cond231ns$after.GCB[cond231ns$after.GCB < 0] <- 2.220446e-16 

cond231ns$perc_change.GCB<- ((cond231ns$after.GCB-cond231ns$before.GCB)/cond231ns$before.GCB)


cond231ns$weight_change<-cond231ns$perc_change.GCB*cond231ns$percentage

site231<-as.data.frame(sum(cond231ns$weight_change))
site231$site<- 231
colnames(site231)<- c("sum_weight_change","site")




# weighted change ---------------------------------------------------------
weighted_changeNS<-rbind(site180,site189,site189,site190,site205,site206,site207,site209, site215,site216, site218, site220, site221,site231)

write.csv(weighted_changeNS, "conductance/weighted_changeNS.csv")



