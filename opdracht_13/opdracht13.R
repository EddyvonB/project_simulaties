##Opdracht 13
library(dplyr)
library(ggplot2)

data <- data.frame(avg = integer())

for(i in 1:250){
  data[i,1] <- ceiling(sum(runif(12,0,1)))
}
  
rm(i)

ggplot() +
  geom_freqpoly(aes(data$avg), stat = "bin",  binwidth = 1)

##Y lijkt normaal verdeeld.

##Opdracht 13B
y = (y-6)

##Opdracht 13 C
y = (y - mu)/sigma
