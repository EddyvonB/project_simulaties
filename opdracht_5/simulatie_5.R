# Project simulaties opdracht 5
# created by: e.vanbeekveld@student.fontys.nl
# version: 1505-001

#### setup ########################################################################################
##start clean
rm(list = ls() )

##create an empty list for the hypothesis test
total <- data.frame(chance = numeric())

##loop 100 times for the hypothesis test.
for(j in 1:100){
  ##Making an empty data frame
  df <- data.frame(a = integer(),
                   b = integer(),
                   c = integer()
                   )
  
  ##variable for the range
  q <- 100
  
  ## Fill the data frame with 1000 randomly chosen numbers between [-20,20]
  for(k in 1:1000){
    df[k,1] <- runif(1, -1*q, q)
    df[k,2] <- runif(1, -1*q, q)
    df[k,3] <- runif(1, -1*q, q)
  }
  
  ##Keeping the workspace clean
  rm(q)
  
  ##making 2 variables which we are going to use to calculate the roots.
  noRoots <- 0
  roots   <- 0
  
  ##calculate the discriminant
  for(i in 1:nrow(df)){
    if((df$b[i]^2 - 4*df$a[i]*df$c[i]) < 0){
      noRoots <- noRoots + 1
    } else {
      roots   <- roots + 1
    }
  }
  total[j,1] <- round(roots / 1000,2)
  rm(df,i,k)
}


##Keeping the workspace clean
rm(noRoots,j)


##determine h0
mu_h0 <- round(roots / 1000,2)

##clean our workspace
rm(roots)
## h1 is not h0

##calculate x and s
mu_x <- round(sum(total$chance)/ 100, 2)
s <- sd(total$chance, na.rm = FALSE)

##determine alpha
alpha <- 0.05

##determining critical value
t = (mu_x - mu_h0)/(s/sqrt(100))


t.half.alpha = qt(1-alpha/2, df=100-1) 
c(-t.half.alpha, t.half.alpha) 

if(t>=-t.half.alpha & t<=t.half.alpha){
  h0 <- 'dont reject'
} else {
  h0 <- 'reject'
}

## if h0 is not rejected the thesis was correct

##cleaning the workspace after the assignment
rm(list = ls() )


