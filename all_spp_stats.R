############ STATS ESTIMATED DISTANCES FOR UK POLLINATORS ##############
## CLAUDIA GUTIERREZ, 2022 ###

data<-as.data.frame(read.delim(file.path("data","all_spp_dist.txt"),na.strings = c("NA",""),stringsAsFactors=T))

summary(data$dist)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.0004   0.0463   0.2119   1.9512   0.4523 741.1294 

hist(data$dist,breaks = 100, xlab="distance[km]", ylab="Frequency", main=NULL)
hist(data$dist,breaks = 100 )
boxplot(dist~Group, data)


logdist<-log(data$dist)
summary(logdist)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-7.7464 -3.0729 -1.5518 -1.9889 -0.7933  6.6082 

hist(logdist,breaks = 100, xlab="logdistance[km]", ylab="Frequency", main=NULL )


hist(logdist,breaks = 100 )
quantile(logdist, c(.05,.95))
#     5%        95% 
#  -6.1791303  0.9601137 

exp(-6.1791303)
#0.002072229
exp(0.9601137)
#2.611993

data_90<-subset(logdist, logdist>-6.1791303  & data$dist<0.9601137)
hist(data_90, breaks = 50, xlab=" log distance[km]", ylab="Frequency")
hist(exp(data_90),breaks = 10, xlab="distance[km]", ylab="Frequency", main=NULL)
data_90<-subset(logdist, logdist>-6.1791303  & logdist<0.9601137)
hist(data_90, breaks = 50)
hist(exp(data_90),breaks = 10)


######Difference in dispersal distance among groups 
library(car)
library(DescTools)

data_1<-data
data_1$logdist<-logdist
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
mothsout<-boxplot(hoverfly$logdist)

bees<-subset(data_1, data_1$Group=="Bees")

allpoll<- rbind(moths, bees, hoverfly)
boxplot(logdist~Group, allpoll)


# Levene's Test for Homogeneity of Variance (center = median)
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
#each group does not have the same average values


# Dunnett's test for comparing several treatments with a control :  
#     95% family-wise confidence level

DunnettTest(allpoll$logdist, g=allpoll$Group)
# # $Bees
# #                     diff     lwr.ci     upr.ci    pval    
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


