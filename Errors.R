## Function to calculate error if DMwR::regr.eval does not work
regr.error <- function(predicted,actual){
  #MAE
  mae <- mean(abs(actual-predicted))
  #MSE
  mse <- mean((actual-predicted)^2)
  #RMSE
  rmse <- sqrt(mean((actual-predicted)^2))
  #MAPE
  mape <- mean(abs((actual-predicted)/actual))
  errors <- c(mae,mse,rmse,mape)
  names(errors) <- c("mae","mse","rmse","mape")
  return(errors)
}