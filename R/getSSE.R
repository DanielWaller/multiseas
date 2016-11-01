#' PES - Calculate SSE
#' 
#' Function takes parameters, data, Init Seasons and Indicator Matrix,
#' returns the SSE for univariate PES
#' 
#' @export
#' 
#' @examples 
#' get.SSE(x,data,InitialSeasons,IndicMat)
#'

get.SSE <- function(x,data,InitialSeasons,Indic){
  
  # Initialise parameters
  
  alpha <- x[1]
  omega <- x[2]
  phi <- x[3]
  if(alpha < 0 | alpha > 1){
    return(Inf)
  }
  if(omega < 0 | omega > 1){
    return(Inf)
  }
  if(phi < 0 | phi > 1){
    return(Inf)
  }
  
  # Initialise other quantities
  
  M <- nrow(Indic) ; n <- length(data)
  Seasons <- matrix(NA, nrow = M, ncol = (n+1))
  Seasons[,1] <- InitialSeasons
  Autoregressives <- numeric(n + 1)
  Autoregressives[1] <- 0
  Forecasts <- numeric(n)
  Errors <- numeric (n)
  
  # The main loop
  for (i in 1:n){
    Forecasts[i] <- sum(Indic[,i]*Seasons[,i]) + phi*Autoregressives[i]
    Errors[i] <- Forecasts[i] - data[i]
    Autoregressives[i+1] <- data[i] - sum(Indic[,i]*Seasons[,i])
    Seasons[,(i+1)] <- Seasons[,i] + (alpha + omega*Indic[,i])*Autoregressives[i+1]
  }
  
  SSE <- sum(Errors*Errors)
  return(SSE)
}