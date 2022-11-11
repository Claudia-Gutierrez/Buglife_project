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




# Weighted change function -------------------------------------------------------------
#This function calculates the proportional increase of habitat quality and area before and after intervention

#INPUTS:
# conductance - *.csv obtained after running Condatis analysis which contains speed before and after habitat regeneration per site
# site- site ID
# direction- "ns" (vertical, NS/SN) or "ew" (horizontal, EW/EW) 

weighted_change_func<-function(conductance, site, direction){
  library (dplyr)
  library (ggplot2)
  
  #read pollinators dipsersal list
  allpoll<- as.data.frame(read.csv("data/allpoll.csv"))
  
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
  
  conductance1<- conductance %>% 
    mutate(across(where(is.numeric), ~ round(., digits = 18)))
  
  
  conductance1<-right_join(percentage, conductance1, by='X')
  
  conductance1$before.GCB[conductance1$before.GCB < 0] <- 2.220446e-16
  conductance1$after.GCB[conductance1$after.GCB < 0] <- 2.220446e-16 
  
  conductance1$perc_change.GCB<- ((conductance1$after.GCB-conductance1$before.GCB)/conductance1$before.GCB)
  
  
  conductance1$weight_change<-conductance1$perc_change.GCB*conductance1$percentage
  
  site1<-as.data.frame(sum(conductance1$weight_change))
  site1$site<- site
  site1$direction<-direction
  colnames(site1)<- c("sum_weight_change","site","direction")
  
  return(site1)
}
  
#Sites NS---------------------------------------------------------------------
# 180 pending ---------------------------------------------------------------------
conductance<- as.data.frame(read.csv("conductance/test180ns_diff.csv")) #Condatis output
site<-180                 #site ID
direction<-"ns"            #ns or ew

site180ns<- weighted_change_func(conductance, site,direction)

# 189 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductance/test189ns_diff.csv"))
site<-189
direction<-"ns"
site189ns<-weighted_change_func(conductance,site,direction)

# 190 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductance/test190ns_diff.csv"))
site<-190
direction<-"ns"
site190ns<-weighted_change_func(conductance,site,direction)

# 205 pending ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductance/test205ns_diff.csv"))
site<-205
direction<-"ns"
site205ns<-weighted_change_func(conductance,site,direction)

# 206 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductance/test206ns_diff.csv"))
site<-206
direction<-"ns"
site206ns<-weighted_change_func(conductance,site,direction)

# 207 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductance/test207ns_diff.csv"))
site<-207
direction<-"ns"
site207ns<-weighted_change_func(conductance,site,direction)


# 209 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductance/test209ns_diff.csv"))
site<-209
direction<-"ns"
site209ns<-weighted_change_func(conductance,site,direction)

# 215 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductance/test215ns_diff.csv"))
site<-215
direction<-"ns"
site215ns<-weighted_change_func(conductance,site,direction)


# 216 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductance/test216ns_diff.csv"))
site<-216
direction<-"ns"
site216ns<-weighted_change_func(conductance,site,direction)

# 218 pending---------------------------------------------------------------------


# 220 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductance/test220ns_diff.csv"))
site<-220
direction<-"ns"
site220ns<-weighted_change_func(conductance,site,direction)


# 221 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductance/test221ns_diff.csv"))
site<-221
direction<-"ns"
site221ns<-weighted_change_func(conductance,site,direction)


# 231 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductance/test231ns_diff.csv"))
site<-231
direction<-"ns"
site231ns<-weighted_change_func(conductance,site,direction)



# weighted change NS ---------------------------------------------------------
weighted_changeNS<-rbind(site180ns,
                         #site189ns,
                         site190ns,
                         site205ns,
                         site206ns,
                         site207ns,
                         site209ns, 
                         site215ns,
                         site216ns, 
                         #site218ns, 
                         site220ns, 
                         site221ns,
                         site231ns)

write.csv(weighted_changeNS, "conductance/weighted_changeNS.csv")






#Sites EW---------------------------------------------------------------------
# 180 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductanceEW/test180ew_diff.csv"))
site<-180
direction<-"ew"
site180ew<-weighted_change_func(conductance,site,direction)

# 189 not available---------------------------------------------------------------------
# 190 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductanceEW/test190ew_diff.csv"))
site<-190
direction<-"ew"
site190ew<-weighted_change_func(conductance,site,direction)

# 205 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductanceEW/test205ew_diff.csv"))
site<-205
direction<-"ew"
site205ew<-weighted_change_func(conductance,site,direction)

# 206 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductanceEW/test206ew_diff.csv"))
site<-206
direction<-"ew"
site206ew<-weighted_change_func(conductance,site,direction)

# 207 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductanceEW/test207ew_diff.csv"))
site<-207
direction<-"ew"
site207ew<-weighted_change_func(conductance,site,direction)
# 209 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductanceEW/test209ew_diff.csv"))
site<-209
direction<-"ew"
site209ew<-weighted_change_func(conductance,site,direction)

# 215 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductanceEW/test215ew_diff.csv"))
site<-215
direction<-"ew"
site215ew<-weighted_change_func(conductance,site,direction)

# 216 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductanceEW/test216ew_diff.csv"))
site<-216
direction<-"ew"
site216ew<-weighted_change_func(conductance,site,direction)

# 218 pending ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductanceEW/test218ew_diff.csv"))
site<-218
direction<-"ew"
site218ew<-weighted_change_func(conductance,site,direction)

# 220 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductanceEW/test220ew_diff.csv"))
site<-220
direction<-"ew"
site220ew<-weighted_change_func(conductance,site,direction)

# 221 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductanceEW/test221ew_diff.csv"))
site<-221
direction<-"ew"
site221ew<-weighted_change_func(conductance,site,direction)

# 231 ---------------------------------------------------------------------
conductance<-as.data.frame(read.csv("conductanceEW/test231ew_diff.csv"))
site<-231
direction<-"ew"
site231ew<-weighted_change_func(conductance,site,direction)



# weighted change EW ---------------------------------------------------------
weighted_changeEW<-rbind(site180ew,
  #site189ew, not available
  site190ew,
  site205ew,
  site206ew,
  site207ew,
  site209ew, 
  site215ew,
  site216ew, 
  #site218ew, 
  site220ew, 
  site221ew,
  site231ew)

write.csv(weighted_changeEW, "conductanceEW/weighted_changeEW.csv")



# histogram of percentage of species --------------------------------------


spp_dist<-ggplot(percentage) +
  geom_bar(aes(disp_dist,percentage*100),stat="identity", fill='cyan4',alpha=0.5) +
  scale_x_log10("Dispersal distance (km)",labels = scales::number_format(accuracy = 0.01))+
  scale_y_continuous("% Species", breaks = c(seq(0, 8, 2)),
    labels = scales::number_format(accuracy = 1))+
  labs(title = 'Pollinators dispersal distance')+
  theme(text = element_text(size = 13, family="sans"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())
spp_dist
ggsave("figs/spp_dist.jpeg", spp_dist, width = 4250, height = 2500,
       units = "px", dpi = 500)

  
         
