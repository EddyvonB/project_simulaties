steek <- function(x, mu, sigma){
  1/(sigma * sqrt(2*pi)) * exp(-1/2 * ((x-mu)/sigma)^2)
}
df <- data.frame(x = double(),
                 y = double())
j <- 1
for(i in seq(-5, 5, 0.1)){
  df[j,1] <- i
  df[j,2] <- steek(i,0,1)
  j <- j + 1
}
ggplot()+
  geom_point(aes(df$x, df$y))