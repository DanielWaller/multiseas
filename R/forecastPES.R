#' PES - Forecasts
#' 
#' Function takes PES model object and future indicator matrix,
#' returns fixed origin forecasts over that period
#' 
#' @export
#' 
#' @examples 
#' forecast.pes(PES.model,Indic)
#'

forecast.pes <- function(PES.model,IndicFut){
  fittingdata <- PES.model[[1]] ; Indic <- PES.model[[2]] ; InitSeasons <- PES.model[[3]]
  params <- PES.model[[4]] ; alpha <- params[1] ; omega <- params[2] ; phi <- params[3]
  
  n2 <- ncol(IndicFut) ; ForecastVec <- numeric(n2) ; FutureSeasons <- matrix(NA, nrow = M, ncol = (n2+1))
  FutureAutoregressives <- numeric(n2+1)
  
  n1 <- length(fittingdata) ; n <- n1 + n2
  Seasons <- matrix(NA, nrow = M, ncol = (n1+1))
  Seasons[,1] <- InitSeasons
  Autoregressives <- numeric(n1+1)
  Autoregressives[1] <- 0
  Forecasts <- numeric(n1)
  Errors <- numeric (n1)
  
  for (i in 1:n1){
    Forecasts[i] <- sum(Indic[,i]*Seasons[,i]) + phi*Autoregressives[i]
    Errors[i] <- Forecasts[i] - data[i]
    Autoregressives[i+1] <- data[i] - sum(Indic[,i]*Seasons[,i])
    Seasons[,(i+1)] <- Seasons[,i] + (alpha + omega*Indic[,i])*Autoregressives[i+1]
  }
  
  FutureSeasons[,1] <- Seasons[,(n1+1)]
  FutureAutoregressives[1] <- Autoregressives[(n1+1)]
  
  for(i in 1:n2){
    ForecastVec[i] <- sum(IndicFut[,i]*FutureSeasons[,i]) + phi*FutureAutoregressives[i]
    FutureAutoregressives[i+1] <- ForecastVec[i] - sum(IndicFut[,i]*FutureSeasons[,i])
    FutureSeasons[,(i+1)] <- FutureSeasons[,i] + (alpha + omega*IndicFut[,i])*FutureAutoregressives[i+1]
  }
  
  return(ForecastVec)
}