library(shinystan)

myshiny<-as.shinystan(fit.mixture1)

launch_shinystan(myshiny, rstudio = T)
