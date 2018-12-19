library(MCMCpack)
library(ggplot2)

dsample <- diamonds[sample(1:nrow(diamonds),500,replace = FALSE),]

model <- MCMCregress(price ~x,data = dsample)
summary(model)

plot(model)

plot(dsample$x,dsample$price)