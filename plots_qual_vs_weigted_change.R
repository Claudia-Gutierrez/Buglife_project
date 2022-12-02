################################################
#                                              #
#              Plots and models of             #
#   quality improvement and area increase      #
#                                              #
################################################

#Plots the improvement/area increase vs the sum of weighted change

#INPUTS:
#weighted_changeX- data frame with sum of weighted change per site calculated with the 'sum_cond_change' script 
#improvement-data frame with the proportional change of quality and proportional area increase before and after intervention calculated with the script 'qual_improvement'

library(ggplot2)
library(gganimate)
library(dplyr)
library(png)
library(transformr)
library(scales)
library(gifski)
library(ggrepel) 

# North to South movement ----------------------------------------
impro_weightNS<-left_join(weighted_changeNS, improvement, by='site')


#quality 
NSqplot<-ggplot(impro_weightNS, aes(log10(qual_improve), log10(sum_weight_change), size = area_after_ha))+
  ggtitle("Latitudinal movement") +
  xlab("Proportion of quality improvement")+
  scale_x_continuous(breaks=c(-3.0,-2,-1),labels = c("0.001","0.01","0.10"))+
  geom_line(aes(log10(qual_improve), log10(sq_qual_improve), linetype= "dashed",colour="red", size=0.2),
            show.legend=FALSE)+
  ylab("Sum of wheighted \npropotional change in speed")+
  scale_y_continuous(breaks = c(-5,-2.5,0,2.5,5), labels = c("1e-5","1e-2.5","1e0","1e2.5","1e5"))+
  geom_point(alpha =0.7, colour='cyan4')+
  scale_size("Total habitat \narea (ha)",labels = c("0-49", "50-99","100-149","150-199", "200-299",">300") , breaks = c(50,100,150,200, 250, 300), limits = c(0, 400))+
  geom_text_repel(mapping = aes(label = site_letter),hjust =-1,size=3.5,
                  max.overlaps = Inf, segment.color= "grey", segment.size=0.2)+
  theme(text = element_text(size = 11, family="sans"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())
NSqplot

ggsave("figs/Lat_qual.jpeg", NSqplot, width = 4250, height = 2500,
  units = "px", dpi = 500)


NSq<-lm(log10(impro_weightNS$sum_weight_change)~ log10(impro_weightNS$qual_improve))
summary(NSq)
# Estimate: 3.5, p= 0.0002***, AdjR^2= 0.74. Positive sig. relationship

#area 
NSaplot<-ggplot(impro_weightNS, aes(log10(area_incr), log10(sum_weight_change), size = area_after_ha))+
  ggtitle("Latitudinal movement") +
  xlab("Proportion of area increase")+
  scale_x_continuous(breaks=c(-3.0,-2,-1),labels = c("0.001","0.01","0.10"))+
  geom_line(aes(log10(area_incr), log10(sq_area_incr), linetype= "dashed",colour="red", size=0.2),
            show.legend=FALSE)+
  ylab("Sum of wheighted \npropotional change in speed")+
  scale_y_continuous(breaks = c(-5,-2.5,0,2.5,5), labels = c("1e-5","1e-2.5","1e0","1e2.5","1e5"))+
  geom_point(alpha =0.7, colour='cyan4')+
  scale_size("Total habitat \narea (ha)",labels = c("0-49", "50-99","100-149","150-199", "200-299",">300") , breaks = c(50,100,150,200, 250, 300), limits = c(0, 400))+
  geom_text_repel(mapping = aes(label = site_letter),hjust =-1,size=3.5,
                  max.overlaps = Inf, segment.color= "grey", segment.size=0.2)+
  theme(text = element_text(size = 11, family="sans"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())
NSaplot

ggsave("figs/Lat_area.jpeg", NSaplot, width = 4250, height = 2500,
       units = "px", dpi = 500)


NSa<-lm(log10(impro_weightNS$sum_weight_change)~ log10(impro_weightNS$area_incr))
summary(NSa)
# Estimate: 3.7, p= 0.0019***, AdjR^2= 0.63. Positive sig. relationship


# East to West movement ----------------------------------------

impro_weightEW<-left_join(weighted_changeEW, improvement, by='site')

#quality 
EWqplot<-ggplot(impro_weightEW, aes(log10(qual_improve), log10(sum_weight_change), size = area_after_ha))+
  ggtitle("Longitudinal movement") +
  xlab("Proportion of quality improvement")+
  scale_x_continuous(breaks=c(-3.0,-2,-1),labels = c("0.001","0.01","0.10"))+
  geom_line(aes(log10(qual_improve), log10(sq_qual_improve), linetype= "dashed",colour="red", size=0.2),show.legend=FALSE)+
  ylab("Sum of wheighted \npropotional change in speed")+
  scale_y_continuous(breaks = c(0,5,10,15), labels = c("1e0","1e5","1e10","1e15"))+
  geom_point(alpha =0.7, colour='cyan4')+
  scale_size("Total habitat \narea (ha)",labels = c("0-49", "50-99","100-149","150-199", "200-299",">300") , breaks = c(50,100,150,200, 250, 300), limits = c(0, 400))+
  geom_text_repel(mapping = aes(label = site_letter),hjust =-1,size=3.5,
                  max.overlaps = Inf, segment.color= "grey", segment.size=0.2)+
  theme(text = element_text(size = 11, family="sans"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())
EWqplot

ggsave("figs/Long_qual.jpeg", EWqplot, width = 4250, height = 2500,
       units = "px", dpi = 500)

EWq<-lm(log10(impro_weightEW$sum_weight_change)~ log10(impro_weightEW$qual_improve))
summary(EWq)
# Estimate: 3.4, p= 0.063***, AdjR^2= 0.23. Positive sig. relationship

#area 
EWaplot<-ggplot(impro_weightEW, aes(log10(area_incr), log10(sum_weight_change), size = area_after_ha))+
  ggtitle("Latitudinal movement") +
  xlab("Proportion of area increase")+
  scale_x_continuous(breaks=c(-3.0,-2,-1),labels = c("0.001","0.01","0.10"))+
  geom_line(aes(log10(area_incr), log10(sq_area_incr), linetype= "dashed",colour="red", size=0.2),show.legend=FALSE)+
  ylab("Sum of wheighted \npropotional change in speed")+
  scale_y_continuous(breaks = c(0,5,10,15), labels = c("1e0","1e5","1e10","1e15"))+
  geom_point(alpha =0.7, colour='cyan4')+
  scale_size("Total habitat \narea (ha)",labels = c("0-49", "50-99","100-149","150-199", "200-299",">300") , breaks = c(50,100,150,200, 250, 300), limits = c(0, 400))+
  geom_text_repel(mapping = aes(label = site_letter),hjust =-1,size=3.5,
                  max.overlaps = Inf, segment.color= "grey", segment.size=0.2)+
  theme(text = element_text(size = 11, family="sans"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())
EWaplot

ggsave("figs/Long_area.jpeg", EWaplot, width = 4250, height = 2500,
       units = "px", dpi = 500)

EWa<-lm(log10(impro_weightEW$sum_weight_change)~ log10(impro_weightEW$area_incr))
summary(EWa)
# Estimate: 3.8, p= 0.064 ., AdjR^2= 0.23. Positive sig. relationship


# Animation ---------------------------------------------------------------

joint_impro_weight<-rbind(impro_weightNS,impro_weightEW)

joint_impro_weight$direction[joint_impro_weight$direction=="ns"]<- "Latitudinal"
joint_impro_weight$direction[joint_impro_weight$direction=="ew"]<- "Longitudinal"

joint<-ggplot(joint_impro_weight, 
              aes(log10(qual_improve), log10(sum_weight_change), size = area_after_ha))+
  xlab("Proportion of quality improvement")+
  scale_x_continuous(breaks=c(-3.0,-2,-1),labels = c("0.001","0.01","0.10"))+
  geom_line(aes(log10(area_incr), log10(sq_area_incr), linetype= "dashed",colour="red", size=0.2),show.legend=FALSE)+
  ylab("Sum of wheighted \npropotional change in speed")+
  scale_y_continuous(breaks = c(0,5,10,15), labels = c("1e0","1e5","1e10","1e15"))+
  geom_line(aes(log10(qual_improve), log10(sq_qual_improve), linetype= "dashed",colour="red", size=0.2),show.legend=FALSE)+
  geom_point(alpha =0.7, colour='cyan4')+
  scale_size("Total habitat \narea (ha)",labels = c("0-49", "50-99","100-149","150-199", "200-299",">300") , breaks = c(50,100,150,200, 250, 300), limits = c(0, 400))+
  geom_text(mapping = aes(label = site_letter),hjust =-0.2,size=12, check_overlap=TRUE)+
  guides(size = "legend", colour = "none")+
  theme(text = element_text(size = 30, family="sans"),
        axis.text = element_text(size = 30),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=25),
        legend.title = element_text(size=30),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())+
  transition_states(direction, state_length = 2,
                    transition_length = 0.5) +
  labs(title= "Movement: {closest_state}")

joint2<-animate(joint, height = 750, width =1275)
anim_save("figs/movement_qual_line.gif", joint2)


# R base plot ---------------------------------------------------------------

####Latitudinal
#area
plot(log10(impro_weightNS$area_incr), log10(((1+impro_weightNS$area_incr)^2)-1),xlab="",ylab="", type="l", ylim=c(-5,7), col="red")
par(new=TRUE)
plot(log10(impro_weightNS$area_incr), log10(impro_weightNS$sum_weight_change),ylim=c(-5,7), xlab="Proportion of area increase",ylab="Sum of wheighted propotional change in speed", main="Latitudinal")


#quality
plot(log10(impro_weightNS$qual_improve), log10(impro_weightNS$sq_qual_improve),xlab="",ylab="", type="l", ylim=c(-5,7), col="red")
par(new=TRUE)
plot(log10(impro_weightNS$qual_improve), log10(impro_weightNS$sum_weight_change),ylim=c(-5,7), xlab="Proportion of quality improvement",ylab="Sum of wheighted propotional change in speed",main="Latitudinal")



####Longitudinal
#area
plot(log10(impro_weightEW$area_incr), log10(((1+impro_weightEW$area_incr)^2)-1),xlab="",ylab="", type="l", ylim=c(-5,7), col="red")
par(new=TRUE)
plot(log10(impro_weightEW$area_incr), log10(impro_weightEW$sum_weight_change),ylim=c(-5,7), xlab="Proportion of area increase",ylab="Sum of wheighted propotional change in speed", main="Longitudinal")


#quality
plot(log10(impro_weightEW$qual_improve), log10(impro_weightEW$sq_qual_improve),xlab="",ylab="", type="l", ylim=c(-5,7), col="red")
par(new=TRUE)
plot(log10(impro_weightEW$qual_improve), log10(impro_weightEW$sum_weight_change),ylim=c(-5,7), xlab="Proportion of quality improvement",ylab="Sum of wheighted propotional change in speed",main="Longitudinal")

     