#' Mean Absolute Percentage Error (MAPE)
#' 
#' Computes MAPE of forecasts.
#' 
#' @param ForecastVec The vector of forecasts
#' @param ObservedVec The vector of observations
#' @export

MeanAbsolutePercentageError <- function(ForecastVec,ObservedVec){
  m <- length(ForecastVec); n <- length(ObservedVec)
  if (m == n){
    Error <- abs((ForecastVec - ObservedVec)/ObservedVec)
    MAPE <- 100*(sum(Error))/n
    return(MAPE)
  }
  else{
    print("Error: vectors not equal length")
  }
}