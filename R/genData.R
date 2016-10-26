#' Generate Data
#' 
#' Generates data for a toy Shiny app
#' 
#' @export

gen.Data <- function(Trend,Seasonality,Period,Length){
  data <- numeric(Length)
  
  if(Seasonality == 1){
    SeasInd <- c(1)
  }
  
  if(Seasonality == 2){
  SeasInd <- numeric(Period)
  SeasInd[1:(Period - 1)] <- runif(n = Period - 1, min = -5, max = 5)
  SeasInd[Period] <- -1*sum(SeasInd)
  }
  
  if(Seasonality == 3){
    SeasInd <- numeric(Period)
    SeasInd[1:(Period - 1)] <- rnorm(n = Period - 1, mean = 1.02, sd = 0.1)
    if(min(SeasInd) < 0){
      SeasInd <- 1 - min(SeasInd)
    }
    SeasInd[Period] <- 1; SeasInd <- log(SeasInd)
    SeasInd[Period] <- 1; SeasInd[Period] <- 1/prod(SeasInd)
  }
  
if(Trend == 2){
  T <- runif(1,3)
}
  
if(Trend == 3){
  T <- runif(1.01,1.08)
}

InitPoint <- runif(n = 1, min = 5, max = 10)

if(Seasonality == 2){
  data[1] <- InitPoint + SeasInd[1]
}
if(Seasonality == 3){
  data[1] <- InitPoint*SeasInd[1]
}

temp <- SeasInd[1]
SeasInd[1:(Period - 1)] <- SeasInd[2:Period]
SeasInd[Period] <- temp

for(i in 2:Period){
  Last <- data[(i-1)]
  if(Seasonality == 2 && Trend == 2){
    data[i] <- Last + SeasInd[1] + T + runif(n=1, mean = 0, sd = 1)
  }
  if(Seasonality == 2 && Trend == 3){
    data[i] <- Last*T + SeasInd[1] + runif(n=1, mean = 0, sd = 1)
  }
  if(Seasonality == 3 && Trend == 2){
    data[i] <- Last*SeasInd[1] + T + runif(n=1, mean = 0, sd = 1)
  }
  if(Seasonality == 3 && Trend == 3){
    data[i] <- Last*SeasInd[1]*T + runif(n=1, mean = 0, sd = 1)
  }
  if(Seasonality == 1 && Trend == 1){
    data[i] <- Last + runif(n=1, mean = 0, sd = 1)
  }
  if(Seasonality == 1 && Trend == 2){
    data[i] <- Last + T + runif(n=1, mean = 0, sd = 1)
  }
  if(Seasonality == 1 && Trend == 3){
    data[i] <- Last*T + runif(n=1, mean = 0, sd = 1)
  }
  if(Seasonality == 2 && Trend == 1){
    data[i] <- Last + SeasInd[1] + runif(n=1, mean = 0, sd = 1)
  }
  if(Seasonality == 3 && Trend == 1){
    data[i] <- Last*SeasInd[1] + runif(n=1, mean = 0, sd = 1)
  }
  
  temp <- SeasInd[1]
  SeasInd[1:(Period - 1)] <- SeasInd[2:Period]
  SeasInd[Period] <- temp
}

return(data)
}