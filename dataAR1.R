
  source("https://raw.githubusercontent.com/AparicioJohan/lme4.plus/master/lme4_plot.R")           # coords & dup
  require(tidyverse)


dataAR1 <- function(data, response="YDHA", col="col", row="row", experiment="dataset"){
  
  data <- as.data.frame(data)
  data$Environment <- data[,experiment]
  
  # Listas with Trials
  VEFr <- data %>%  filter(., response!=0) %>% split(x = ., f = .$Environment, drop = T) %>% lapply(., droplevels)
  
  # Check col - row duplicated by Trial and Fill with NA
  VEFr <- VEFr %>% lapply(.,dup) %>% lapply(.,coords,col=col,row=row)
  
  dataFinal <-  data.table(plyr::ldply(VEFr[], data.frame))
  names(dataFinal)[1] <- c("Environment")
  
  dataFinal$Environment <- as.factor(dataFinal$Environment)
  dataFinal$col_f <- as.factor(dataFinal$col)
  dataFinal$row_f <- as.factor(dataFinal$row)
  
  dataFinal <-  dataFinal[order(dataFinal$Environment, dataFinal$col_f, dataFinal$row_f), ]
  
  return(dataFinal)
}

## Example
## dataAR1(dt,response = "Response", col="col", row="row" ,  experiment = "Environment") 
