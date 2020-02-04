
require(tidyverse)

desinG <- function(data, col="", row="", rep="", block="", experiment=""){
  
  dt <- as.data.frame(data)
  
  if (col==""&row=="") {
    design <- dt %>%
      group_by(Experiment=get(experiment)) %>%
      summarise(rep=n_distinct(get(rep)),
                block=n_distinct(get(block)), 
                design=ifelse(block>1,"Alpha-L","RCBD"))
  } else {
    
    design <- dt %>%
      group_by(Experiment=get(experiment)) %>%
      summarise(rep=n_distinct(get(rep)),
                block=n_distinct(get(block)), 
                ncol=n_distinct(get(col)),nrow=n_distinct(get(row)),
                design=ifelse(block>1,"Alpha-L","RCBD"), spatial=ifelse(ncol>=2&nrow>=2,"Spatial","Nonspa"))
  }
  
  
  trial.RCBD <<- design %>% filter(design=="RCBD") %>% pull(Experiment) %>% as.character()
  trial.alpha <<- design %>% filter(design=="Alpha-L") %>% pull(Experiment) %>% as.character()
  
  return(design)
  
}

## Example
# desinG(data = CRIB, rep = "REP", block = "Block", experiment = "Experiment_ID" )



