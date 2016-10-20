#' Mean Absolute Error (MAE)
#' 
#' Computes MAE of forecasts.
#' 
#' @param ForecastVec The vector of forecasts
#' @param ObservedVec The vector of observations
#' @export

MeanAbsoluteError <- function(ForecastVec,ObservedVec){
  m <- length(ForecastVec); n <- length(ObservedVec)
  if (m == n){
    Error <- abs(ForecastVec - ObservedVec)
    MAE <- (sum(Error))/n
    return(MAE)
  }
  else{
    print("Error: vectors not equal length")
  }
}