library(caTools)

steek <- function(x, mu, sigma){
  1/(sigma * sqrt(2*pi)) * exp(-1/2 * ((x-mu)/sigma)^2)
}
df <- data.frame(x = double(),
                 y = double(),
                 z = double())
j <- 1
for(i in seq(-5, 5, 0.01)){
  df[j,1] <- i
  df[j,2] <- steek(i,0,1)
  df[j,3] <- 0
  j <- j + 1
}
ggplot()+
  geom_point(aes(df$x, df$y))

j <- 1
for(i in seq(-5, 5, 0.01)){
  df[j,3] <- ifelse(j != 1, trapz(df$x[1:j],df$y[1:j]), 0)
  j <- j + 1
}

ggplot()+
  geom_point(aes(df$x, df$y)) +
  geom_line(aes(df$x, df$z)) +
  geom_vline(xintercept = 1.28)

voor t = mu geeft r= 0 want top 