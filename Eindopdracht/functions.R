info <- function(df, b) {
  
  min <- df %>%
    summarise(min = min(df[,b]))
  
  max <- df %>%
    summarise(max = max(df[,b]))
  
  median <- median(df[,b]) 
  
  mean <- mean(df[,b]) 
  
  summation <- df %>%
    summarise(sum = sum(df[,b]))
  
  total <- data.frame("Minimum" =      prettyNum(round(min$min         ,0),big.mark = ",", decimal.mark = "."),
                      "Maximum" =      prettyNum(round(max$max         ,0),big.mark = ",", decimal.mark = "."),
                      "Median"  =      prettyNum(round(median          ,0),big.mark = ",", decimal.mark = "."),
                      "Mean"    =      prettyNum(round(mean            ,0),big.mark = ",", decimal.mark = "."),
                      "Total"   =      prettyNum(round(summation$sum   ,0),big.mark = ",", decimal.mark = "."))
  
  rownames(total) <- b
  
  total
}
