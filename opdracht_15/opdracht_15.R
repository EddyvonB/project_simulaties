##opdracht 15a

library(ggplot2)
library(dplyr)

e <- data.frame(x = double(),
                y = double())


j <- 1
for(i in seq(0,10,0.2)){
  e[j,1] <- i
  e[j,2] <- 1- exp(-1*i)
  j <- j+1
}

ggplot() +
  geom_line(aes(e$x, e$y)) +
  scale_x_continuous("t") +
  scale_y_continuous("y") 

##Opdracht 15b
r <- runif(1000,0,1)

p_great_34 <- sum(r<=0.34)/1000
p_great_98 <- sum(r<=0.98)/1000
p_u <- sum(r<=u)/1000

##opdracht 15c
##tekst niet executable
r = 1-e^(-lambda*t)
r-1 = -e^(-lambda*)t
ln(r-1) = lambda*t
t = ln(r-1)/lambda

##opdracht 15d

  
##opdracht 15e

##opdracht 15f

data <- ceiling(rexp(250,0.2))

#opdracht 15g
ggplot() +
  geom_freqpoly(aes(data), stat = "bin",  binwidth = 1)

Ja die lijkt best goed op de grafiek van de kansfunctie.

