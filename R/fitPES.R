#' PES - Fit PES model
#' 
#' Function takes data and indicator matrix,
#' returns a fitted PES model with initial seasonal indices and optimised parameter values
#' 
#' @export
#' 
#' @examples 
#' fit.PES(data,PESIndicator)
#' [[1]] <- data
#' [[2]] <- Indicator Matrix (large!)
#' [[3]] <- Initial Seasons
#' [[4]] <- c(alpha,omega,phi)
#' 

fit.PES <- function(data,seasons){
  n <- length(data)
  Indic <- getPESIndicator(seasons) ; M <- nrow(Indic)
  InitSeasons <- getInitSeasons(data,Indic)
  Oparams <- opt.p(InitSeasons,data,Indic)
  PES.model <- list(data,Indic,InitSeasons,Oparams)
  return(PES.model)
}