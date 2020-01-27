 require("gplots")

"gen_share" <- function(data=NULL, genotype="line", env="Exp", response=NA , plot=TRUE){

  data <- as.data.frame(data)
  nomb <- c(genotype,env)
  
  if(sum(nomb%in%names(data))!=2){
    message("columns not found in the data")
    return()
   }
  
  data=type.convert(data)
  data[,genotype] <- as.factor(data[,genotype])
  data[,env] <- as.factor(data[,env])
 
  if(!is.na(response)) data <- data[ !is.na(data[,response]) , ]

  nexp <- nlevels(data[,env] ) #    experiments
  ngen <- nlevels(data[,genotype] ) #   genotypes
  
  share <- matrix(NA,nrow=nexp,ncol = nexp) 
  rownames(share) <- levels(data[,env] )
  colnames(share) <- levels(data[,env] )
  
  # share by Experiment
  for (i in 1:nexp) {
    
    eitmp <- levels( droplevels( subset(data, get(env)==colnames(share)[i]) )[,genotype]  )
    
    for (j in 1:nexp) {
      ejtmp <- levels( droplevels( subset(data, get(env)==colnames(share)[j]) )[,genotype] )
      
      share[i,j] <- sum(eitmp%in%ejtmp)
    }
  }
  
  M <-  share
  diag(M) <- NA
  M <- mean(M,na.rm = T)
  
  cat("\n")
  cat("\n----------------------")
  cat("\n","Environments:", nexp)
  cat("\n","Genotypes:", ngen)
  cat("\n","Mean shared", M)
  cat("\n----------------------\n")
  
  
  # windows()
 if(isTRUE(plot)){
  try(heatmap.2(share, scale = "none", col = terrain.colors(100),
            trace = "none", density.info = "none", margins = c(15,15)),silent = T)
  }
  return(share)
  
}


## windows()
## ww <- gen_share(data = MET,genotype = "gen", env = "county")
