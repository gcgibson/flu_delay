
run_true_model <- function(){
  current_observed_data <- fully_observed_data[fully_observed_data$region == test_region & fully_observed_data$epiweek <=paste0(test_season_formatted,test_week_formatted),]
  arima_updated_fit <- Arima(current_observed_data$wili,model = arima_fit)
  mean_forecast_trajectory <- forecast(arima_updated_fit,h=4)$mean
  trajectory_samples <- matrix(rnorm(1000,mean_forecast_trajectory,model_var),ncol=4,byrow = T)
  return (trajectory_samples)
}