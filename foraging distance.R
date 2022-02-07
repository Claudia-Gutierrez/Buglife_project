############ FORAGING DISTANCE FOR UK BEES ##############
## CLAUDIA GUTIERREZ, 2022 ###


library(pollimetry)

#Import Intertegular distance (ITD) data [mm]
ITDdata<-read.delim(file.path("data","ITD.txt"),na.strings = c("NA",""),stringsAsFactors=T)
save(ITD,file=file.path("data","ITDdata.Rdata"))

foragedist(x, type = "GreenleafAll")









####REFERENCES####
##Data: Roberts & Potts 'The Database on Functional Traits 
##Pollimetry package:Kendall et al. (2019) Pollinator size and its consequences: Predictive allometry for pollinating insects. <doi:10.1101/397604>; Greenleaf et al. (2007) Bee foraging ranges and their relationship to body size. Oecologia, 153, 589-596. <doi:10.1007/s00442-007-0752-9>

