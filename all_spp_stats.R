############ STATS ESTIMATED DISTANCES FOR UK POLLINATORS ##############
## CLAUDIA GUTIERREZ, 2022 ###

data<-as.data.frame(read.delim(file.path("data","all_spp_dist.txt"),na.strings = c("NA",""),stringsAsFactors=T))

summary(data$dist)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.0004   0.0463   0.2119   1.9512   0.4523 741.1294 
hist(data$dist,breaks = 100, xlab="distance[km]", ylab="Frequency", main=NULL)

logdist<-log(data$dist)
summary(logdist)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-7.7464 -3.0729 -1.5518 -1.9889 -0.7933  6.6082 

hist(logdist,breaks = 100, xlab="logdistance[km]", ylab="Frequency", main=NULL )
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




