# Project simulaties opdracht 7
# created by: e.vanbeekveld@student.fontys.nl
# github: EddyvonB
# version: 1505-001

##always a clean start
rm(list = ls() )

###required packages
library(ggplot2)

##create an empty data frame for the hypothesis test
total <- data.frame(chance = numeric())

##do this excersize a 100 times for a good approach
for(l in 1:100){
  ##do this excercize 100 times to check for the chance
  out <- 0
  for(k in 1:100){
    ##Make a vectors which way the point can go
    states <- c("up", "down", "right", "left")
    df <- data.frame(x    = 0,
                     y    = 0)
    
    ##100 steps to generate
    for(i in 1:100){
      randomstate  <- sample(states, 1, replace = T)
      if(randomstate == "up"){
        df[i+1,1] = df$x[i]
        df[i+1,2] = df$y[i] + 1
      } else if(randomstate == "down"){
        df[i+1,1] = df$x[i]
        df[i+1,2] = df$y[i] - 1
      } else if(randomstate == "left"){
        df[i+1,1] = df$x[i] - 1
        df[i+1,2] = df$y[i] 
      } else {
        df[i+1,1] = df$x[i] + 1
        df[i+1,2] = df$y[i] 
      }
    }
    
    ##keep the workspace clean
    rm(i, randomstate, states)
    
    ##check if is inside circle
      if ((df$x[nrow(df)] - 0)^2 + (df$y[nrow(df)] - 0)^2 > 10^2){
        out=out+1
      }
  }
  
  ##function for a circle
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(xx = xx, yy = yy))
  }
  
  ##make the circle
  dat <- circleFun(c(0,0),20,npoints = 100)
  
  #create the plot
  ggplot() +
    geom_point(data = df, aes(x, y )) +
    geom_path(data = dat, aes(xx,yy))
  
  ##keep our workspace clean
  rm(df,dat,circleFun)
  ##chance not in circle in k calculations
  chanceOut <- out/k

  total[l,1] <- chanceOut
}

approachChance <- round(sum(total$chance)/ l, 2)

##keep our workspace clean
rm(k,out)


##chance that the last point from the data is outside the circle is 0.36
##keep our workspace clean
rm(list = ls() )