library(forecast)
library(dplyr)
## Load needed objects
data <- readRDS("flu_data_with_backfill.rds")
lag_df <- read.csv("lag_df")
fully_observed_data <- read.csv("fully_observed_data")
model_params <- read.csv("model_params.csv")

model_var <- model_params$model_variance
data_for_training <- fully_observed_data[fully_observed_data$region ==test_region & fully_observed_data$epiweek < 201540,]
arima_fit <- auto.arima(data_for_training$wili)

for (test_region in c("nat",paste0("hhs",1:10))){
  for (test_season in c("2015","2016","2017")){
    if (test_season == "2017"){
      end_week <- 12
    }else{
      end_week <- 20
    }
    for (test_week in c(seq(40,52),seq(end_week))){
      if (test_week < 40){
        test_season_formatted <- as.numeric(test_season) + 1
        if (test_week <= 9){
          test_week_formatted <- paste0("0",test_week)
        }else{
          test_week_formatted <- test_week
        }
      }else{
        test_season_formatted <- test_season
        test_week_formatted <- test_week
      }
      for (reporting_delay_adjustment in c(TRUE,FALSE,"TRUTH")){
        current_observed_data_with_lags <- data[data$region == test_region & data$issue <= as.numeric(paste0(test_season_formatted,test_week_formatted)),]
        current_observed_data <- current_observed_data_with_lags %>% group_by(region,epiweek) %>%
          filter(lag == max(lag))
        current_observed_data <- current_observed_data[order(current_observed_data$epiweek),]
        
        if (reporting_delay_adjustment == "LAGDF"){
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
        } else if(reporting_delay_adjustment == "TRUTH"){
          current_observed_data <- fully_observed_data[fully_observed_data$region == test_region & fully_observed_data$epiweek <=paste0(test_season_formatted,test_week_formatted),]
        } else if(reporting_delay_adjustment == TRUE){
          if (test_week_formatted >=40){
            for (lag_itr in seq(40,test_week_formatted)){
              current_lag <- test_week_formatted -lag_itr
              prop_estimate <- mean(lag_df[lag_df$Region == test_region & lag_df$week < 201540 , paste0("X",current_lag)],na.rm = T)
              current_observed_data[current_observed_data$epiweek == paste0(test_season_formatted,lag_itr),]$wili <-
                current_observed_data[current_observed_data$epiweek == paste0(test_season_formatted,lag_itr),]$wili/prop_estimate
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
        }
        
        arima_updated_fit <- Arima(current_observed_data$wili,model = arima_fit)
        mean_forecast_trajectory <- forecast(arima_updated_fit,h=4)$mean
        trajectory_samples <- matrix(rnorm(1000,mean_forecast_trajectory,model_var),ncol=4,byrow = T)
        trajectory_samples_bins <- apply(trajectory_samples,1:2,get_inc_bin)
        trajectory_log_prob <- c()
        for (ph in 1:4){
          trajectory_log_prob <- cbind(trajectory_log_prob,bin_to_log_prob(trajectory_samples_bins[,ph]))
        }
        write.csv(trajectory_log_prob,paste0("output/",test_region,"-",test_season_formatted,"-",test_week_formatted,"-",reporting_delay_adjustment,"-",model_var))
       }
    }
  }
}