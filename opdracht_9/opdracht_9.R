library(dplyr)
library(conf.design)

voorwaarde <- function(a, b, m){
  gcd(b,m) == 1 & factor2(a, m) & ifelse((a-1)%%4 == 0,ifelse((m%%4) == 0, TRUE, FALSE), FALSE)
}

gcd <- function(x,y) {
  r <- x%%y;
  return(ifelse(r, gcd(y, r), y))
}

factor2 <- function(a,m){
  c <- factorize(m)
  ret <- T
  fin <- T
  while(ret & fin){
    for(i in c){
      ret <- ifelse((a-1)%%i == 0, T, F)
    }
    fin <- F
  }
  ret
}


##opdracht 9a
voorwaarde(17,11,1000)

##opdracht 9b
voorwaarde(53,4,1009)

##opdracht 9c
voorwaarde(21,11,1000)
voorwaarde(41,11,1000)
voorwaarde(61,11,1000)

