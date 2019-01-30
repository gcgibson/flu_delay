library(forecast)
library(dplyr)
## Load needed objects
data <- readRDS("flu_data_with_backfill.rds")
lag_df <- read.csv("lag_df")
fully_observed_data <- read.csv("fully_observed_data")
model_params <- read.csv("model_params.csv")
source("utils.R")
##models
source("model_1.R")
source("model_2.R")
source("truth_from_lag_df.R")
source("truth.R")
models_to_test <- c("M2")

model_var <- model_params$model_variance

for (test_region in c("nat",paste0("hhs",1:10))){
  
  data_for_training <- fully_observed_data[fully_observed_data$region ==test_region & fully_observed_data$epiweek < 201540,]
  arima_fit <- auto.arima(data_for_training$wili)
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
      for (reporting_delay_adjustment in models_to_test){
        current_observed_data_with_lags <- data[data$region == test_region & data$issue <= as.numeric(paste0(test_season_formatted,test_week_formatted)),]
        current_observed_data <- current_observed_data_with_lags %>% group_by(region,epiweek) %>%
          filter(lag == max(lag))
        current_observed_data <- current_observed_data[order(current_observed_data$epiweek),]
        
        if (reporting_delay_adjustment == "LAGDF"){
          trajectory_samples <- run_model_truth_from_lag(test_week_formatted,test_season_formatted,test_region,current_observed_data)
          
        } else if(reporting_delay_adjustment == "TRUTH"){
          trajectory_samples <- run_true_model()
          
        } else if(reporting_delay_adjustment == "M1"){
          trajectory_samples <- run_model_1(test_week_formatted,test_season_formatted,test_region,current_observed_data)
          
        } else if(reporting_delay_adjustment == "M2"){
          trajectory_samples <- run_model_2(test_week_formatted,test_season_formatted,test_region,current_observed_data)
          
        }
        
        
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