#' Root Mean Squared Error (RMSE)
#' 
#' Computes RMSE of forecasts.
#' 
#' @param ForecastVec The vector of forecasts
#' @param ObservedVec The vector of observations
#' @export

RootMeanSquaredError <- function(ForecastVec,ObservedVec){
  m <- length(ForecastVec); n <- length(ObservedVec)
  if (m == n){
    Error <- ForecastVec - ObservedVec
    RMSE <- sqrt((sum(Error*Error))/n)
    return(RMSE)
  }
  else{
    print("Error: vectors not equal length")
  }
}


