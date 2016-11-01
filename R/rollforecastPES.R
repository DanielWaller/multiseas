#' PES - Rolling Forecasts
#' 
#' Function takes PES model object and future indicator matrix,
#' along with future data and data horizon,
#' returns rolling origin forecasts over that period
#' 
#' @export
#' 
#' @examples 
#' rollforecast.pes(PES.model,Indic)
#'

rollforecast.pes <- function(PES.model,IndicFut,dataFut,h){
  fittingdata <- PES.model[[1]] ; Indic <- PES.model[[2]] ; InitSeasons <- PES.model[[3]]
  params <- PES.model[[4]] ; alpha <- params[1] ; omega <- params[2] ; phi <- params[3]
  
  n1 <- length(fittingdata)
  Seasons <- matrix(NA, nrow = M, ncol = (n1+1))
  Seasons[,1] <- InitSeasons
  Autoregressives <- numeric(n1+1)
  Autoregressives[1] <- 0
  Forecasts <- numeric(n1)
  Errors <- numeric (n1)
  
  for(i in 1:n1){
    Forecasts[i] <- sum(Indic[,i]*Seasons[,i]) + phi*Autoregressives[i]
    Errors[i] <- Forecasts[i] - data[i]
    Autoregressives[i+1] <- data[i] - sum(Indic[,i]*Seasons[,i])
    Seasons[,(i+1)] <- Seasons[,i] + (alpha + omega*Indic[,i])*Autoregressives[i+1]
  }
  
  n2 <- length(dataFut) + 1 - h
  SeasonsFut <- matrix(NA, nrow = M, ncol = (n2+2-h))
  SeasonsFut[,1] <- Seasons[,(n1+1)]
  AutoregressivesFut <- numeric(n2+2 - h)
  AutoregressivesFut[1] <- 0
  ForecastVec <- numeric(n2 + 1 - h)
  ErrorsFut <- numeric (n2 + 1 - h)
  
  for(i in 1:(n2+1-h)){
    ForecastVec[i] <- sum(IndicFut[,(i+h-1)]*SeasonsFut[,i]) + phi*AutoregressivesFut[i]
    ErrorsFut[i] <- ForecastVec[i] - dataFut[(i-1+h)]
    AutoregressivesFut[(i+1)] <- dataFut[(i-1+h)] - sum(IndicFut[,(i+h-1)]*SeasonsFut[,i])
    SeasonsFut[,(i+1)] <- SeasonsFut[,i] + (alpha + omega*IndicFut[,(i+h-1)])*AutoregressivesFut[(i+1)]
  }
  
  return(ForecastVec)
  
}