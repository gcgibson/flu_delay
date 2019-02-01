
n_traj_sims <-200

run_model_2 <- function(test_week_formatted,test_season_formatted,test_region,current_observed_data){
  trajectory_samples <- matrix(NA,ncol=4)
  if (test_week_formatted >=40){
    for (samp_idx in 1:n_traj_sims){
      current_observed_data_local <- current_observed_data
      for (lag_itr in seq(40,test_week_formatted)){
        current_lag <- test_week_formatted -lag_itr
        prop_estimate_sample_data <- lag_df[lag_df$Region == test_region & lag_df$week < 201540 & lag_df$season_week == test_week_formatted , paste0("X",current_lag)]
        prop_estimate_sample_data <- prop_estimate_sample_data[!is.na(prop_estimate_sample_data)]
        if (length(prop_estimate_sample_data) > 0){
          prop_estimate_sample <- sample(prop_estimate_sample_data[!is.na(prop_estimate_sample_data)],1)
        } else{
          prop_estimate_sample <- 1
        }
        
        current_observed_data_local[current_observed_data_local$epiweek == paste0(test_season_formatted,lag_itr),]$wili <-
          current_observed_data_local[current_observed_data_local$epiweek == paste0(test_season_formatted,lag_itr),]$wili/prop_estimate_sample
        }
      #arima_updated_fit <- Arima(current_observed_data_local$wili,model = arima_fit)
      mean_forecast_trajectory <-
        simulate(
          object = arima_fit,
          nsim = 1000,
          seed = 1,
          newdata =current_observed_data$wili,
          h = 52
        )
      trajectory_samples <- rbind(trajectory_samples,mean_forecast_trajectory)
    }
  } else{
    
    for (samp_idx in 1:n_traj_sims){
      current_observed_data_local <- current_observed_data
      for (lag_itr in seq(40,52)){
        current_lag <- 52 -lag_itr
        prop_estimate_sample_data <- lag_df[lag_df$Region == test_region & lag_df$week < 201540 & lag_df$season_week == test_week_formatted , paste0("X",current_lag)]
        prop_estimate_sample_data <- prop_estimate_sample_data[!is.na(prop_estimate_sample_data)]
        if (length(prop_estimate_sample_data) > 0){
          prop_estimate_sample <- sample(prop_estimate_sample_data[!is.na(prop_estimate_sample_data)],1)
        } else{
          prop_estimate_sample <- 1
        }
        current_observed_data_local[current_observed_data_local$epiweek == paste0(test_season_formatted-1,lag_itr),]$wili <-
          current_observed_data_local[current_observed_data_local$epiweek == paste0(test_season_formatted-1,lag_itr),]$wili/prop_estimate_sample
      }
      for (lag_itr in seq(1,as.numeric(test_week_formatted))){
        current_lag <- as.numeric(test_week_formatted) -lag_itr
        prop_estimate_sample_data <- lag_df[lag_df$Region == test_region & lag_df$week < 201540 & lag_df$season_week == test_week_formatted , paste0("X",current_lag)]
        prop_estimate_sample_data <- prop_estimate_sample_data[!is.na(prop_estimate_sample_data)]
        
        if (length(prop_estimate_sample_data) > 0){
          prop_estimate_sample <- sample(prop_estimate_sample_data[!is.na(prop_estimate_sample_data)],1)
        } else{
          prop_estimate_sample <- 1
        }
        current_observed_data_local[current_observed_data_local$epiweek == paste0(test_season_formatted,lag_itr),]$wili <-
          current_observed_data_local[current_observed_data_local$epiweek == paste0(test_season_formatted,lag_itr),]$wili/prop_estimate_sample
      }
      #arima_updated_fit <- Arima(current_observed_data_local$wili,model = arima_fit)
      mean_forecast_trajectory <- 
        simulate(
          object = arima_fit,
          nsim = 1000,
          seed = 1,
          newdata =current_observed_data$wili,
          h = 52
        )
      trajectory_samples <- rbind(trajectory_samples,mean_forecast_trajectory)
    }
  }
  
  trajectory_samples <- trajectory_samples[2:nrow(trajectory_samples),]
  return (trajectory_samples)
}