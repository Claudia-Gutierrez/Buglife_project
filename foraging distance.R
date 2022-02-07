############ FORAGING DISTANCE FOR UK BEES ##############
## CLAUDIA GUTIERREZ, 2022 ###


library(pollimetry)

#Import Intertegular distance (ITD) data [mm]
ITDdata<-read.delim(file.path("data","ITD.txt"),na.strings = c("NA",""),stringsAsFactors=T)

#Calculate GrMhd ("Maximum homing distance"), GrThd ("Typical homing distance"), GrMfd ("Maximum feeder training distance"), GrMcd("Maximum communication distance")
forage_dist<-as.data.frame(foragedist(ITDdata$ITD.Mean_f_mm, type = "GreenleafAll"))

#join table with species name and ITD
foragedistall<-cbind(ITDdata, forage_dist)


####REFERENCES####
##Data: Roberts & Potts 'The Database on Functional Traits 
##Pollimetry package:Kendall et al. (2019) Pollinator size and its consequences: Predictive allometry for pollinating insects. <doi:10.1101/397604>; Greenleaf et al. (2007) Bee foraging ranges and their relationship to body size. Oecologia, 153, 589-596. <doi:10.1007/s00442-007-0752-9>

