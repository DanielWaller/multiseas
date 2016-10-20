#' PES - Indicator Matrix
#' 
#' A function that takes a vector of seasons, where the i-th entry = k indicates that datapoint i is in season k, and forms
#' the indicator matrix required for the PES method
#' 
#' @param seasons
#' 
#' @export
#' @examples 
#' seasons <- c(1,2,3,3)
#' getPESIndicator(seasons)
#' 1 0 0 0
#' 0 1 0 0
#' 0 0 1 1

getPESIndicator <- function(seasons){
  M <- max(seasons) ; n <- length(seasons)
  IndicMat <- matrix(NA, nrow = M, ncol = n)
  for(i in 1:M){
    Indices <- which(seasons == i)
    IndicMat[i,Indices] <- 1
  }
  IndicMat[is.na(IndicMat)] <- 0
  return(IndicMat)
}