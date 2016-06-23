##opdracht 16a
http://www.columbia.edu/~ks20/4404-Sigman/4404-Notes-ITM.pdf


##opdracht 16b
De top van de grafiek zit bij 1/2	
want we weten dat de oppervlakte onder de grafiek gelijk aan 1 moet zijn. De oppervlakte van een driehoek is 1/2*b*h.
b-6 opp =1 h = 0.5

functievoorschrift voor de grafiek								
als	0	>=	X			dan	Y = 0
als	0	<	X	=<	3	dan 	y = X/6
als	3	<	X	<	6	dan	y = 1-X/6
als			X	>=	6	dan	Y = 0

##opdracht 16c
library(ggplot2)
library(dplyr)

data <- data.frame(val = double(),
                   x = integer(),
                   som = double())
j = 1
for(i in seq(0,6,0.01)){
  data[j,1] <- ifelse(i<3,i/6,1-i/6)
  data[j,2] <- j/100
  data[j,3] <- sum(data$val)
  j <- j+1
}
ggplot() +
  geom_point(aes(data$x, data$val))

##opdracht 16d en e
ggplot() +
  geom_point(aes(data$x, data$som))