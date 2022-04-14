################################################
#                                              #
#    Stats of estimated dispersal distances    #
#        for UK insect pollinators             #
#                                              #
################################################

## CLAUDIA GUTIERREZ, MARCH 2022 ###

#This code calculates the range of pollinators' dispersal distances required by Condatis. The distances are calculated for three groups: bees, hoverflies and moths. 
#The distances were calculated using linear regression models using morphological characters of the species. See Methods for details.
#An ANOVA and (post hoc) Dunnett Test is performed to assess if there is a significant diffrence among the dispersal distances of the groups  

library(car)
library(DescTools)

# Explore data ------------------------------------------------------------

#Read text file with species names and respective estimated dispersal distance
data<-as.data.frame(read.delim(file.path("data","all_spp_dist.txt"),na.strings = c("NA",""),stringsAsFactors=T))

summary(data$dist)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.0004   0.0463   0.2119   1.9512   0.4523 741.1294 

hist(data$dist,breaks = 100, xlab="distance[km]", ylab="Frequency", main=NULL)

boxplot(dist~Group, data)

#Log-transformed data
data_1<-data
data_1$logdist<-log(data$dist)
hist(data_1$logdist)

boxplot(logdist~Group, data_1)

#Remove outliers

moths<-subset(data_1, data_1$Group=="Moths")
mothsout<-boxplot(moths$logdist, plot=FALSE)$out
moths<- moths[-which(moths$logdist %in% mothsout),]
mothsout<-boxplot(moths$logdist)

hoverfly<-subset(data_1, data_1$Group=="Hoverfly")
hoverflyout<-boxplot(hoverfly$logdist, plot=FALSE)$out
hoverfly<- hoverfly[-which(hoverfly$logdist %in% hoverflyout),]
hoverflyout<-boxplot(hoverfly$logdist)

bees<-subset(data_1, data_1$Group=="Bees")

allpoll<- rbind(moths, bees, hoverfly)
boxplot(logdist~Group, allpoll)


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
model <- aov(logdist ~ Group, data = allpoll)
summary(model)
# Df Sum Sq Mean Sq F value Pr(>F)    
# Group          2    343  171.39   44.78 <2e-16 ***
#   Residuals   1228   4700    3.83                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

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

#Use the following intervals
summary(bees$dist)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.01503  0.08470  0.24826  0.82198  0.66327 10.42849 

summary (moths$dist)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.00043  0.01367  0.14425  1.12600  0.47880 81.11324 

