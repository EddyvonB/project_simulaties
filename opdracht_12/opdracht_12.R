##opdracht 12a

library(ggplot2)
library(dplyr)

f <- data.frame(x = double(),
                y = double())

f2 <- data.frame(x = double(),
                 y = double())


j <- 1
for(i in seq(-4,4,0.2)){
  f2[j,1] <- i
  f2[j,2] <- ifelse(i<0 | i>1, 0,1)
  f[j,1] <- i
  f[j,2] <- ifelse(i<0,0,ifelse(i>1,1,i))
  j <- j+1
}

##grafiek met de blauwe lijn gelijk aan f en de rode aan F
ggplot() +
  geom_line(aes(f$x, f$y , colour="blue")) +
  scale_x_continuous("x") +
  scale_y_continuous("y") +
  geom_line(aes(f2$x, f2$y, colour="red" )) + 
  theme(legend.position="none")

##opdracht 12b 
## tekst niet executable
E(x) =  integraal(x*f(x) dx)				
E(x) = integraal (x(dx))				
= 0,5*x^2				
verwachtingswaarde E(x) = 0,5	

## tekst niet executable
Var(x) = E((x-E(x))^2)				
Var(x) = E(x^2) - E(x)^2				
(E(x))^2 = 	1/4			
E(x^2)''= 	1/3			
Var(x) = 0,083333333	

##opdracht 12c
## tekst niet executable
E(y) = E(x1 + x2 ...)
E(y) = E(x1) + E(x2) ... = 6

Var(y) = Var(x1 + x2 ...)
Var(y) = Var(x1) + Var(x2) ... = 1

##opdracht 12d
v <- runif(1000,0,1)
avg <- mean(v)
varv <- var(v)
