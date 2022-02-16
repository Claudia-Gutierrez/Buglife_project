########Correlation Dry weight—Mean Flight distance

library(ggplot2)

data<-as.data.frame(read.delim(file.path("data","Moths_flightdist.txt"),na.strings = c("NA",""),stringsAsFactors=T))

model <- lm(log(data$AvgFlightDistance) ~ log(data$estimated_dry_mass))

plot(log(AvgFlightDistance) ~ log(estimated_dry_mass), data = data)
abline(model)

summary(model)




ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


fit1 <- lm(log(data$AvgFlightDistance) ~ log(data$estimated_dry_mass))
ggplotRegression(fit1)


