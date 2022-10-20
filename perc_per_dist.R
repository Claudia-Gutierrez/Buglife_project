################################################
#                                              #
#       Speed change weighted by species       #
#                                              #
################################################

#This script calculates the proportional change of speed before and after intervention
#It quantifies the percentage of species that fall within dispersal distance bins 

#INPUTS:
# allpoll - data frame with the all the species and their respective estimated dispersal distance
# conductance- data frame obtained after runing Condatis analysis which contains speed before and after habitat regeneration 



library (dplyr)
library (ggplot2)

options(scipen = 100)

hist(allpoll$dist)

hist_info <- hist(allpoll$dist, breaks= c(10^seq(-3.5,2,0.1)))  # Store output of hist function

hist_info$density <- hist_info$counts /    # Compute density values
  sum(hist_info$counts) * 100
plot(hist_info, freq = FALSE)

breaks<-cbind.data.frame(hist_info$breaks)

breaks<-as.data.frame(breaks[-c(1), ])

percentage<-cbind.data.frame(breaks,hist_info$density)
colnames(percentage)<- c("disp_dist","percentage")


percentage$cumperc<-cumsum(percentage$percentage)
percentage$inv_perc<-100-percentage$cumperc

percentage<- percentage %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 13)))


conductance<- data.frame(read.csv("conductance/test_3kdiff.csv"))


conductance<- conductance %>% 
  mutate(across(where(is.numeric), ~ round(., digits = 13)))


condperc<-right_join(percentage, conductance, by='disp_dist')

condperc$perc_change.GCB<- ((condperc$after.GCB-condperc$before.GCB)/condperc$before.GCB)

condperc$times<-(condperc$perc_change.GCB)/100


condperc$weight_change<-condperc$perc_change.GCB*condperc$percentage

condperc$wgt_times<-(condperc$weight_change)/100


condpercplot<- ggplot(condperc, aes(log10(disp_dist), log10(perc_change.GCB), label = cumperc))+
  geom_point(size = 4, aes(colour= inv_perc))+
#  geom_text(aes(label=round(perc_change.GCB, digits=2), hjust=0.5, vjust = -0.5))+
  labs(x = 'log10 (Dispersal distance) [km]', y='log10 (% change in speed)', colour ="% Species" )+
  scale_x_continuous(breaks=c(-1.5,-1,-0.5,0,0.5))+
  theme(text = element_text(size = 15), legend.position="right")


ggplot(condperc, aes(log10(disp_dist), inv_perc, size= times))+
  geom_point(color='cyan4')+
  scale_size_area("Speed\nincrease\n(times)", labels = c(">0.1", "1", "10", "24") , breaks = c(0.1, 1, 10, 24))+
  labs(x = 'log(Dispersal distance) [km]', y='% of species', colour = blues9 )+
  scale_x_continuous(breaks=c(-1.5,-1,-0.5,0,0.5))+
  theme(text = element_text(size = 15), legend.position="right")

ggplot(condperc, aes(log10(disp_dist), inv_perc, size= times))+
  geom_point(color='cyan4')+
  scale_size_area("Speed\nincrease\n(times)", labels = c(">0.1", "1", "10", "24") , breaks = c(0.1, 1, 10, 24))+
  labs(x = 'Dispersal distance [m]', y='% of species', colour = blues9 )+
  scale_x_continuous(breaks=c(-1.5,-1,-0.5,0,0.5),labels =c(30,100,320, 1000, 3160))+
  theme(text = element_text(size = 15), legend.position="right")

ggplot(condperc, aes(log10(disp_dist), times, size = perc_15m_3k))+
  geom_point(color='cyan4', shape= 1 )+
  scale_size_area("% Species")+
  labs(x = 'log10 (Dispersal distance) [km]', y="Speed increase (times)")+
  scale_x_continuous(breaks=c(-1.5,-1,-0.5,0,0.5))+
  theme(text = element_text(size = 15), legend.position="right")

#%species by bin
ggplot(condperc, aes(log10(disp_dist), log10(times), size = percentage))+
  geom_point(color='cyan4')+
  scale_size_area("% species")+ 
  labs(x = 'Dispersal distance (m)', y="Speed increase (times)")+
  scale_x_continuous(breaks=c(-2,-1.5,-1,-0.5,0, 0.5),labels =c("10","32","100","320", "1,000", "3200"))+
  scale_y_continuous(breaks=c(-3,-2,-1,0,1),labels =c(0.001,0.010,0.1,1,10))+
  theme(text = element_text(size = 15), legend.position="right")


#% species that will have at least this increase
ggplot(condperc, aes(log10(disp_dist), log10(times), size=cumperc))+
  geom_point(color='cyan4')+
  scale_size_area("% species", n.breaks = 10, max_size = 6)+
  labs(title="% species that will have at least this increase",x = 'Dispersal distance (m)', y="Speed increase (times)")+
  scale_x_continuous(breaks=c(-2,-1.5,-1,-0.5,0, 0.5),labels =c("10","32","100","320", "1,000", "3200"))+
  scale_y_continuous(breaks=c(-3,-2,-1,0,1),labels =c(0.001,0.010,0.1,1,10))+
  theme(text = element_text(size = 15), legend.position="right")


#%species vs speed increase
ggplot(condperc, aes(cumperc, log10(times)))+
  geom_point(color='cyan4', size=4)+
  scale_x_continuous(breaks=c(15,25,35,45,55,65,75,85,95))+
  scale_y_continuous(breaks=c(-3,-2,-1,0,1),labels =c(0.001,0.010,0.1,1,10))+
  labs(x = '% species', y="Speed increase (times)")+
  theme(text = element_text(size = 15))



