
library (dplyr)

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

plot(condperc$cumperc,condperc$perc_change.GCB)


plot(log(condperc$disp_dist),log(condperc$perc_change.GCB))


condpercplot<- ggplot(condperc, aes(log10(disp_dist), log10(perc_change.GCB, label = cumperc)))+ 
  geom_point(size = 3, aes(colour= inv_perc))+
  labs(x = 'log (Dispersal distance) [km]', y='log(%change in speed)' )+
  scale_x_continuous(breaks=c(-1.5,-1,-0.5,0,0.5))+
  theme(text = element_text(size = 10), legend.position="right",
        legend.title=element_blank())
 


