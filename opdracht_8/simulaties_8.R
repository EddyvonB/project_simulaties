# Project simulaties opdracht 8
# created by: e.vanbeekveld@student.fontys.nl
# github: EddyvonB
# version: 1505-001

##always a clean start
rm(list = ls() )

###required packages


##Create the function
fx = function(x){
  return(sin(x^2))
}

##make an empty data frame
df <- data.frame(x = numeric(),
                 y = numeric())
##generate n random points
n <- sample(1000:10000, 1, replace = T)

##generate n points in the interval [0 , 1.5] for x and for y
for(i in 1:n){
  df[i,1] <- runif(1, 0, 1.5)
  df[i,2] <- runif(1, 0 ,1.5)
}

##keep our workspace clean
rm(i)
##check how many points are below the function
above <- 0
m <- 0
for(i in 1:nrow(df)){
  if(fx(df$x[i]) < df$y[i]){
    above = above + 1
  } else {
    m = m + 1
  }
}

##small check
zero <- above + m - n

rm(above, i, fx, df)
##determine the relation between m and n and the surface.
relation <- m/n
surface <- relation*2.25

##keep our workspace clean
rm(list = ls())
