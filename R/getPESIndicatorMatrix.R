getPESIndicatorMatrix <- function(seasons){
  M <- max(seasons) ; n <- length(seasons)
  IndicMat <- matrix(NA, nrow = M, ncol = n)
  for(i in 1:M){
    Indices <- which(seasons == i)
    IndicMat[i,Indices] <- 1
  }
  IndicMat[is.na(IndicMat)] <- 0
  return(IndicMat)
}