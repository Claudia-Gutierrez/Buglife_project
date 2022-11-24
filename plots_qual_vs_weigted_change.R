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


#quality vs log (weighted change)

NSqplot<-ggplot(impro_weightNS, aes(sq_qual_improve, sum_weight_change, size = area_after_ha))+
  ggtitle("Latitudinal movement") +
  scale_x_log10("Squared proportion of quality improvement")+
                # breaks = trans_breaks("log10", function(x) 10^x),
                # labels = trans_format("log10", math_format(1^.x)))+
  scale_y_log10("Sum of wheighted \npropotional change in speed")+
               # breaks = trans_breaks("log10", function(x) 10^x),
               # labels = trans_format("log10", math_format(1^.x)))+
  geom_point(alpha =0.7, colour='cyan4')+
  scale_size("Total habitat \narea (ha)",labels = c("0-49", "50-99","100-149","150-199", "200-299",">300") , breaks = c(50,100,150,200, 250, 300), limits = c(0, 400))+
  geom_text_repel(mapping = aes(label = site_letter),hjust =-1,size=3.5,
                  max.overlaps = Inf, segment.color= "grey", segment.size=0.2)+
    stat_smooth(method="lm",se=TRUE, color="#669999", show.legend = FALSE, alpha = 0.2)+
  guides(size = "legend", colour = "none")+
  theme(text = element_text(size = 11, family="sans"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())
NSqplot

ggsave("figs/Lat_sqqual.jpeg", NSqplot, width = 4250, height = 2500,
  units = "px", dpi = 500)


NSq<-lm(log10(impro_weightNS$sum_weight_change)~ log10(impro_weightNS$sq_qual_improve))
summary(NSq)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.5048 -0.4548  0.2290  0.9640  2.4778 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                             5.7326     1.0936   5.242 0.000378 ***
#   log10(impro_weightNS$sq_qual_improve)   3.4553     0.6065   5.697 0.000199 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.952 on 10 degrees of freedom
# Multiple R-squared:  0.7645,	Adjusted R-squared:  0.7409 
# F-statistic: 32.46 on 1 and 10 DF,  p-value: 0.0001991

#area vs log (weighted change)
ggplot(impro_weightNS, aes(area_incr, sum_weight_change, size = area_after_ha))+
  ggtitle("Latitudinal movement") +
  scale_y_log10("Sum of wheighted \nchange in speed",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_point(alpha =0.7, colour='cyan4')+
  scale_size("Total habitat \narea (ha)",labels = c("0-49", "50-99","100-149","150-199", "200-299",">300") , breaks = c(50,100,150,200, 250, 300), limits = c(0, 400))+
  geom_text_repel(mapping = aes(label = site),hjust =-0.8,size=3.5,
                   max.overlaps = Inf, segment.color= "grey", segment.size=0.2)+
  labs(x = 'Proportion of area increase')+
  stat_smooth(method="lm",se=TRUE, color="#669999", show.legend = FALSE, alpha = 0.2)+
  guides(size = "legend", colour = "none")+
  theme(text = element_text(size = 11, family="sans"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())

NSa<-lm(log10(impro_weightNS$sum_weight_change)~ log10(impro_weightNS$area_after_ha))
summary(NSa)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.5456 -0.8203 -0.1853  1.3602  4.7734 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                -2.000      1.052  -1.902  0.08638 . 
# impro_weightNS$area_incr   95.095     27.719   3.431  0.00643 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.726 on 10 degrees of freedom
# Multiple R-squared:  0.5406,	Adjusted R-squared:  0.4947 
# F-statistic: 11.77 on 1 and 10 DF,  p-value: 0.006431

# East to West movement ----------------------------------------

impro_weightEW<-left_join(weighted_changeEW, improvement, by='site')

#quality vs log (weighted change)

EWqplot<-ggplot(impro_weightEW, aes(sq_qual_improve, sum_weight_change, size = area_after_ha))+
  ggtitle("Longitudinal movement") +
  scale_x_log10("Squared proportion of quality improvement")+
  scale_y_log10("Sum of wheighted \npropotional change in speed")+
                # breaks = trans_breaks("log10", function(x) 10^x),
                # labels = trans_format("log10", math_format(10^.x)))+
  geom_point(alpha =0.7, colour='cyan4')+
  scale_size("Total habitat \narea (ha)",labels = c("0-49", "50-99","100-149","150-199", "200-299",">300") , breaks = c(50,100,150,200, 250, 300), limits = c(0, 400))+
  geom_text_repel(mapping = aes(label = site_letter),hjust =-0.8,size=3.5,
                  max.overlaps = Inf, segment.color= "grey", segment.size=0.2)+
  stat_smooth(method="lm",se=TRUE, color="#669999", show.legend = FALSE, alpha = 0.2)+
  guides(size = "legend", colour = "none")+
  theme(text = element_text(size = 11, family="sans"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())
EWqplot

ggsave("figs/Long_sqqual.jpeg", EWqplot, width = 4250, height = 2500,
       units = "px", dpi = 500)

EWq<-lm(log10(impro_weightEW$sum_weight_change)~ log10(impro_weightEW$sq_qual_improve))
summary(EWq)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.1649 -1.5283 -0.8664 -0.2588 12.9625 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                   -1.842      1.933  -0.953   0.3633  
# impro_weightEW$qual_improve   37.637     19.253   1.955   0.0791 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.274 on 10 degrees of freedom
# Multiple R-squared:  0.2765,	Adjusted R-squared:  0.2041 
# F-statistic: 3.822 on 1 and 10 DF,  p-value: 0.0791


#area vs log (weighted change)
ggplot(impro_weightEW, aes(area_incr, sum_weight_change, size = area_after_ha))+
  ggtitle("Longitudinal movement") +
  scale_y_log10("Sum of wheighted \nchange in speed",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_point(alpha =0.7, colour='cyan4')+
  scale_size("Total habitat \narea (ha)",labels = c("0-49", "50-99","100-149","150-199", "200-299",">300") , breaks = c(50,100,150,200, 250, 300), limits = c(0, 400))+
  geom_text_repel(mapping = aes(label = site),hjust =-0.8,size=3.5,
                  max.overlaps = Inf, segment.color= "grey", segment.size=0.2)+
  labs(x = 'Proportion of area increase')+
  stat_smooth(method="lm",se=TRUE, color="#669999", show.legend = FALSE, alpha = 0.2)+
  guides(size = "legend", colour = "none")+
  theme(text = element_text(size = 11, family="sans"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())

EWa<-lm(log10(impro_weightEW$sum_weight_change)~ log10(impro_weightEW$area_incr))
summary(EWa)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.0473 -1.6190 -0.7988  0.1772 12.7846 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                -2.403      1.955  -1.229   0.2470  
# impro_weightEW$area_incr  114.897     51.529   2.230   0.0499 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.067 on 10 degrees of freedom
# Multiple R-squared:  0.3321,	Adjusted R-squared:  0.2653 
# F-statistic: 4.972 on 1 and 10 DF,  p-value: 0.04986



# Animation ---------------------------------------------------------------

joint_impro_weight<-rbind(impro_weightNS,impro_weightEW)

joint_impro_weight$direction[joint_impro_weight$direction=="ns"]<- "Latitudinal"
joint_impro_weight$direction[joint_impro_weight$direction=="ew"]<- "Longitudinal"

joint<-ggplot(joint_impro_weight, 
              aes(qual_improve, sum_weight_change, size = area_after_ha))+
  scale_y_log10("Sum of wheighted \npropotional change in speed")+
                # breaks = trans_breaks("log10", function(x) 10^x),
                # labels = trans_format("log10", math_format(10^.x)))+
  scale_x_log10("Proportion of quality improvement")+
                # breaks = trans_breaks("log10", function(x) 10^x),
                # labels = trans_format("log10", math_format(10^.x)))+
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
anim_save("figs/movement_qual1.gif", joint2)
