#' PES - Initial Seasons
#' 
#' Function takes data and indicator matrix,
#' returns a vector of initial seasons
#' 
#' @export
#' 
#' @examples 
#' getInitSeasons(data,PESIndicator)
#' 

getInitSeasons <- function(data,Ind){
  M <- nrow(Ind) ; InitSeasons <- numeric(M)
  for (i in 1:M){
    if (sum(Indic[i,]) <= 3){
      InitSeasons[i] <- sum(data*Ind[i,])/sum(Ind[i,])
    }
    else{
      Indices <- which(Ind[i,] == 1)[1:3]
      InitSeasons[i] <- mean(data[Indices])
    }
  }
  return(InitSeasons)
}