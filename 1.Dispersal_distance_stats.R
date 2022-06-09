################################################
#                                              #
#    Stats of estimated dispersal distances    #
#        for UK insect pollinators             #
#                                              #
################################################

## CLAUDIA GUTIERREZ, MARCH 2022 ###

#This script calculates the range of pollinators' dispersal distances required by Condatis. The distances are calculated for three groups: bees, hoverflies and moths. 

#The distances were calculated using linear regression models using morphological characters of the species. See 'UK insect pollinator species and dispersal distance' section in the methods report for details.

#An ANOVA and (post hoc) Dunnett Test is performed to assess if there is a significant difference among the dispersal distances of the groups  

#library(car)
library(DescTools)
library(ggplot2)

# Explore data ------------------------------------------------------------

#Read text file with species names and respective estimated dispersal distance
data<-as.data.frame(read.delim(file.path("data","all_spp_dist.txt"),na.strings = c("NA",""),stringsAsFactors=T))

summary(data$dist)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.0004   0.0443   0.2116   1.9534   0.4516 738.4855 

hist(data$dist,breaks = 100, xlab="distance[km]", ylab="Frequency", main=NULL)

boxplot(dist~Group, data)

#Log-transformed data
data_1<-data
data_1$logdist<-log10(data$dist)
hist(data_1$logdist)

boxplot(logdist~Group, data_1)

#Remove outliers

moths<-subset(data_1, data_1$Group=="Moths")
mothsout<-boxplot(moths$logdist, plot=FALSE)$out
moths<- moths[-which(moths$logdist %in% mothsout),]
mothsout<-boxplot(moths$logdist)
summary(moths$dist)


hoverfly<-subset(data_1, data_1$Group=="Hoverfly")
hoverflyout<-boxplot(hoverfly$logdist, plot=FALSE)$out
hoverfly<- hoverfly[-which(hoverfly$logdist %in% hoverflyout),]
hoverflyout<-boxplot(hoverfly$logdist)
summary(hoverfly$dist)


bees<-subset(data_1, data_1$Group=="Bees")
summary(bees$dist)

allpoll<- rbind(moths, bees, hoverfly)

boxplot(logdist~Group, allpoll)

ggplot(allpoll, aes(x=Group, y=logdist)) +
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(width=0.5)+
  theme_minimal()+
  theme(legend.position="none", aspect.ratio = 1, text = element_text(size = 18))+
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2), labels=c("0.001", "0.01", "0.1","1","10","100"))+
  ylab('Log (Dispersal distance) [km]')+
  scale_x_discrete(label=c("Bees", "Hoverflies","Moths"))
  



# Tests difference among groups -------------------------------------------

# Levene's Test for Homogeneity of Variance to choose post hoc test
leveneTest(logdist~Group, allpoll)

# Levene's Test for Homogeneity of Variance (center = median)
#         Df F value    Pr(>F)    
# group    2  157.11 < 2.2e-16 ***
#       1228                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#The variances are unequal

#ANOVA
model <- kruskal.test(logdist~ Group, data = allpoll)

# Kruskal-Wallis rank sum test
# data:  logdist by Group
# Kruskal-Wallis chi-squared = 46.165, df = 2, p-value = 9.448e-11

#There is a significant difference among groups


#Dunnett's test for comparing between groups with a control (95% confidence level)

DunnettTest(allpoll$logdist, g=allpoll$Group)
# $Bees
#                  diff     lwr.ci     upr.ci    pval    
# Hoverfly-Bees -0.1324511 -0.5231872  0.2582849  0.6608    
# Moths-Bees    -1.1367991 -1.4470014 -0.8265968 2.7e-15 ***
# # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


DunnettTest(allpoll$logdist, control="Moths", g=allpoll$Group)
# $Moths
#                     diff    lwr.ci   upr.ci    pval    
# Bees-Moths     1.136799 0.8222483 1.451350 1.7e-15 ***
# Hoverfly-Moths 1.004348 0.6733853 1.335311 3.7e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#There is a significant difference between moths and bees but not between bees and hoverflies

# Dispersal distance values -----------------------------------------------

#Use the following intervals for Bees and Hoverflies
summary(bees$dist)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.01503  0.08470  0.24826  0.82198  0.66327 10.42849 
  #######                                     ########

#Use the following intervals for Moths
summary (moths$dist)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.00043  0.01367  0.14425  1.12600  0.47880 81.11324 
 ########                                     ######## 


