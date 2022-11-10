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
library(png)
library(transformr)
library(scales)
library(gifski)

# North to South movement ----------------------------------------
impro_weightNS<-left_join(weighted_changeNS, improvement, by='site')

#quality vs log (weighted change)

NSqplot<-ggplot(impro_weightNS, aes(qual_improve, sum_weight_change, size = area_after_ha))+
  ggtitle("South to North movement") +
  scale_y_log10("Sum of wheighted \nchange in speed",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_point(alpha =0.7, colour='cyan4')+
  scale_size_area("Total habitat \narea",labels = c("0.2", "0.8", "1.4", "2.0","2.6","3.2") , breaks = c(0.2, 0.8, 1.4, 2.0,2.6,3.2))+
  geom_text(mapping = aes(label = site),hjust =-0.2,size=3.5)+
  labs(x = 'Proportion of quality improvement')+
  stat_smooth(method="lm",se=TRUE, color="#669999", show.legend = FALSE, alpha = 0.2)+
  guides(size = "legend", colour = "none")+
  theme(text = element_text(size = 11, family="sans"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())
NSqplot

NSq<-lm(log10(impro_weightNS$sum_weight_change)~ impro_weightNS$qual_improve)
summary(NSq)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.4457 -1.3621 -0.1785  1.8703  2.9738 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                  -1.9343     0.8832  -2.190  0.05624 . 
# impro_weightNS$qual_improve  38.6649     8.4208   4.592  0.00131 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.243 on 9 degrees of freedom
# Multiple R-squared:  0.7008,	Adjusted R-squared:  0.6676 
# F-statistic: 21.08 on 1 and 9 DF,  p-value: 0.001306

#area vs log (weighted change)
ggplot(impro_weightNS, aes(area_incr, sum_weight_change, size = area_after_ha))+
  ggtitle("South to North movement") +
  scale_y_log10("Sum of wheighted \nchange in speed",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_point(alpha =0.7, colour='cyan4')+
  scale_size_area("Total habitat \narea",labels = c("0.2", "0.8", "1.4", "2.0","2.6","3.2") , breaks = c(0.2, 0.8, 1.4, 2.0,2.6,3.2))+
  geom_text(mapping = aes(label = site),hjust =-0.2,size=3.5)+
  labs(x = 'Proportion of area increase')+
  stat_smooth(method="lm",se=TRUE, color="#669999", show.legend = FALSE, alpha = 0.2)+
  guides(size = "legend", colour = "none")+
  theme(text = element_text(size = 11, family="sans"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())

NSa<-lm(log10(impro_weightNS$sum_weight_change)~ impro_weightNS$area_incr)
summary(NSa)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.5185 -0.9006 -0.0089  1.4904  4.7141 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                -1.872      1.194  -1.568   0.1514  
# impro_weightNS$area_incr   92.894     30.146   3.082   0.0131 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.861 on 9 degrees of freedom
# Multiple R-squared:  0.5134,	Adjusted R-squared:  0.4593 
# F-statistic: 9.496 on 1 and 9 DF,  p-value: 0.01311

# East to West movement ----------------------------------------

impro_weightEW<-left_join(weighted_changeEW, improvement, by='site')

#quality vs log (weighted change)

EWqplot<-ggplot(impro_weightEW, aes(qual_improve, sum_weight_change, size = area_after_ha))+
  ggtitle("East to West movement") +
  scale_y_log10("Sum of wheighted \nchange in speed",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_point(alpha =0.7, colour='cyan4')+
  scale_size_area("Total habitat \narea",labels = c("0.2", "0.8", "1.4", "2.0","2.6","3.2") , breaks = c(0.2, 0.8, 1.4, 2.0,2.6,3.2))+
  geom_text(mapping = aes(label = site),hjust =-0.2,size=3.5)+
  labs(x = 'Proportion of quality improvement')+
  stat_smooth(method="lm",se=TRUE, color="#669999", show.legend = FALSE, alpha = 0.2)+
  guides(size = "legend", colour = "none")+
  theme(text = element_text(size = 11, family="sans"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())
EWqplot


EWq<-lm(log10(impro_weightEW$sum_weight_change)~ impro_weightEW$qual_improve)
summary(EWq)

lm(formula = log10(impro_weightEW$sum_weight_change) ~ impro_weightEW$qual_improve)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -8.0555 -1.5032 -0.9375 -0.3881 12.9848 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)                   -1.608      2.178  -0.738    0.479
# impro_weightEW$qual_improve   36.216     20.768   1.744    0.115
# 
# Residual standard error: 5.533 on 9 degrees of freedom
# Multiple R-squared:  0.2525,	Adjusted R-squared:  0.1695 
# F-statistic: 3.041 on 1 and 9 DF,  p-value: 0.1152


#area vs log (weighted change)
ggplot(impro_weightEW, aes(area_incr, sum_weight_change, size = area_after_ha))+
  ggtitle("East to West movement") +
  scale_y_log10("Sum of wheighted \nchange in speed",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_point(alpha =0.7, colour='cyan4')+
  scale_size_area("Total habitat \narea",labels = c("0.2", "0.8", "1.4", "2.0","2.6","3.2") , breaks = c(0.2, 0.8, 1.4, 2.0,2.6,3.2))+
  geom_text(mapping = aes(label = site),hjust =-0.2,size=3.5)+
  labs(x = 'Proportion of area increase')+
  stat_smooth(method="lm",se=TRUE, color="#669999", show.legend = FALSE, alpha = 0.2)+
  guides(size = "legend", colour = "none")+
  theme(text = element_text(size = 11, family="sans"),
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())

EWa<-lm(log10(impro_weightEW$sum_weight_change)~ impro_weightEW$area_incr)
summary(EWa)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.9865 -1.8676 -0.7576  0.1175 12.8067 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                -2.234      2.225  -1.004   0.3416  
# impro_weightEW$area_incr  111.972     56.158   1.994   0.0773 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.33 on 9 degrees of freedom
# Multiple R-squared:  0.3064,	Adjusted R-squared:  0.2293 
# F-statistic: 3.975 on 1 and 9 DF,  p-value: 0.07731



# Animation ---------------------------------------------------------------

joint_impro_weight<-rbind(impro_weightNS,impro_weightEW)

joint_impro_weight$direction[joint_impro_weight$direction=="ns"]<- "South to North"
joint_impro_weight$direction[joint_impro_weight$direction=="ew"]<- "East to West"

joint<-ggplot(joint_impro_weight, 
              aes(qual_improve, sum_weight_change, size = area_after_ha))+
  scale_y_log10("Sum of wheighted \nchange in speed",
                breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  geom_point(alpha =0.7, colour='cyan4')+
  scale_size_area("Total habitat \narea",labels = c("0.2", "0.8", "1.4", "2.0","2.6","3.2") , breaks = c(0.2, 0.8, 1.4, 2.0,2.6,3.2))+
  geom_text(mapping = aes(label = site),hjust =-0.2,size=5)+
  labs(x = 'Proportion of quality improvement')+
  guides(size = "legend", colour = "none")+
  theme(text = element_text(size = 20, family="sans"),
        axis.text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(colour = "black", fill=NA),
        panel.background = element_blank())+
  transition_states(direction, state_length = 2,
                    transition_length = 0.5) +
  labs(title= "Direction of Movement: {closest_state}")

joint2<-animate(joint, height = 500, width =850)
anim_save("figs/movement.gif", joint2)
