#' PES - Optimise parameters
#' 
#' Function takes indicator matrix and data
#' returns a vector of the optimised parameters alpha, phi and omega.
#' 
#' @export
#' 
#' @examples 
#' opt.p(PESIndicator,data)
#'

opt.p <- function(InitialSeasons,data,Indic){
  best.SSEs <- matrix(NA, nrow = 10, ncol = 4)
  for(i in 1:1000){
    get.runifs <- runif(n = 3, min = 0, max = 1)
    SSE <- get.SSE(x = get.runifs, data = data, InitialSeasons = InitialSeasons,Indic = Indic)
    if(i > 10){
      if(SSE < best.SSEs[10,1]){
        best.SSEs[10,] <- c(SSE,get.runifs)
        best.SSEs <- best.SSEs[order(best.SSEs[,1],best.SSEs[,2],best.SSEs[,3],best.SSEs[,4],decreasing = FALSE,na.last = TRUE),]
      }
    }
    else{
      best.SSEs[10,] <- c(SSE,get.runifs)
      best.SSEs <- best.SSEs[order(best.SSEs[,1],best.SSEs[,2],best.SSEs[,3],best.SSEs[,4],decreasing = FALSE,na.last = TRUE),]
    }
    print(i)
  }
  
  oparams <- matrix(nrow = 10, ncol = 3)
  for (i in 1:10){
    pars <- best.SSEs[i,(2:4)]
    oparams[i,] <- as.vector(optim(pars,fn = get.SSE,method = "BFGS", data = data, InitialSeasons = InitialSeasons,Indic = Indic)[[1]])
    print(i)
  }
  
  final.SSEs <- numeric(10)
  for(i in 1:10){
    final.SSEs[i] <- get.SSE(oparams[i,],data = data, InitialSeasons = InitialSeasons,Indic = Indic)
  }
  
  return(oparams[which.min(final.SSEs),])
}