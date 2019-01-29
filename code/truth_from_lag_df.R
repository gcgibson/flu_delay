run_model_truth_from_lag <- function(test_week_formatted,test_season_formatted,test_region,current_observed_data){
  if (test_week_formatted >=40){
    for (lag_itr in seq(40,test_week_formatted)){
      current_lag <- test_week_formatted -lag_itr
      current_observed_data[current_observed_data$epiweek == paste0(test_season_formatted,lag_itr),]$wili <-
        current_observed_data[current_observed_data$epiweek == paste0(test_season_formatted,lag_itr),]$wili/lag_df[lag_df$Region == test_region & lag_df$week == paste0(test_season_formatted,lag_itr) , paste0("X",current_lag)]
    }
  } else{
    for (lag_itr in seq(40,52)){
      current_lag <- 52 -lag_itr
      current_observed_data[current_observed_data$epiweek == paste0(test_season_formatted-1,lag_itr),]$wili <-
        current_observed_data[current_observed_data$epiweek == paste0(test_season_formatted-1,lag_itr),]$wili/lag_df[lag_df$Region == test_region & lag_df$week == paste0(test_season_formatted-1,lag_itr) , paste0("X",current_lag)]
    }
    for (lag_itr in seq(1,as.numeric(test_week_formatted))){
      current_lag <- as.numeric(test_week_formatted) -lag_itr
      current_observed_data[current_observed_data$epiweek == paste0(test_season_formatted,lag_itr),]$wili <-
        current_observed_data[current_observed_data$epiweek == paste0(test_season_formatted,lag_itr),]$wili/lag_df[lag_df$Region == test_region & lag_df$week == paste0(test_season_formatted,lag_itr) , paste0("X",current_lag)]
    }
  }
  arima_updated_fit <- Arima(current_observed_data$wili,model = arima_fit)
  mean_forecast_trajectory <- forecast(arima_updated_fit,h=4)$mean
  trajectory_samples <- matrix(rnorm(1000,mean_forecast_trajectory,model_var),ncol=4,byrow = T)
  return (trajectory_samples)
}