fully_observed_data <- read.csv("fully_observed_data")
model_params <- read.csv("model_params.csv")


test_region <-toString(model_params$region)
model_var <- model_params$model_variance
step_ahead <- 1
score <- "MULTIBIN"

delay_adjusted_total_prob <- c()
non_delay_adjusted_total_prob <- c()
true_total_prob <- c()

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
    
    delay_adjusted_forecasts <- read.csv(paste0("output/",test_region,"-",test_season_formatted,"-",test_week_formatted,"-","TRUE","-",model_var))
    non_delay_adjusted_forecasts <- read.csv(paste0("output/",test_region,"-",test_season_formatted,"-",test_week_formatted,"-","FALSE","-",model_var))
    true_forecasts <- read.csv(paste0("output/",test_region,"-",test_season_formatted,"-",test_week_formatted,"-","TRUTH","-",model_var))
    
    
    truth_time <- NULL
    
    if (as.numeric(test_week_formatted) + step_ahead > 52){
      tmp_week <- as.numeric(test_week_formatted) +step_ahead-52
      if (tmp_week <= 9){
        tmp_week_formatted <- paste0("0",tmp_week)
      }else{
        tmp_week_formatted <- tmp_week
      }
      truth_time <- paste0(as.numeric(test_season_formatted)+1,tmp_week_formatted)
    }else{
      tmp_week <- as.numeric(test_week_formatted) +step_ahead
      if (tmp_week <= 9){
        tmp_week_formatted <- paste0("0",tmp_week)
      }else{
        tmp_week_formatted <- tmp_week
      }
      truth_time <- paste0(test_season_formatted,tmp_week_formatted)
    }
    truth <- get_inc_bin(fully_observed_data[fully_observed_data$region == test_region &fully_observed_data$epiweek == truth_time,]$wili)
    truth_l <- max(0,as.numeric(truth)-.1)
    truth_ll <- max(0,as.numeric(truth)-.2)
    truth_r <- min(13,as.numeric(truth)+.1)
    truth_rr <- min(13,as.numeric(truth)+.2)
    
    prob_delay_adjusted_center <- delay_adjusted_forecasts[delay_adjusted_forecasts$X == truth,paste0("V",step_ahead)]
    prob_non_delay_adjusted_center <- non_delay_adjusted_forecasts[non_delay_adjusted_forecasts$X == truth,paste0("V",step_ahead)]
    prob_true_center <- true_forecasts[true_forecasts$X == truth,paste0("V",step_ahead)]
    
    
    
    prob_delay_adjusted_l <- delay_adjusted_forecasts[delay_adjusted_forecasts$X == truth_l,paste0("V",step_ahead)]
    prob_non_delay_adjusted_l <- non_delay_adjusted_forecasts[non_delay_adjusted_forecasts$X == truth_l,paste0("V",step_ahead)]
    prob_true_l <- true_forecasts[true_forecasts$X == truth_l,paste0("V",step_ahead)]
    
    
    
    prob_delay_adjusted_ll <- delay_adjusted_forecasts[delay_adjusted_forecasts$X == truth_ll,paste0("V",step_ahead)]
    prob_non_delay_adjusted_ll <- non_delay_adjusted_forecasts[non_delay_adjusted_forecasts$X == truth_ll,paste0("V",step_ahead)]
    prob_true_ll <- true_forecasts[true_forecasts$X == truth_ll,paste0("V",step_ahead)]
    
    prob_delay_adjusted_r <- delay_adjusted_forecasts[delay_adjusted_forecasts$X == truth_r,paste0("V",step_ahead)]
    prob_non_delay_adjusted_r <- non_delay_adjusted_forecasts[non_delay_adjusted_forecasts$X == truth_r,paste0("V",step_ahead)]
    prob_true_r <- true_forecasts[true_forecasts$X == truth_r,paste0("V",step_ahead)]
    prob_true_rr <- true_forecasts[true_forecasts$X == truth_rr,paste0("V",step_ahead)]
    
    
    
    prob_delay_adjusted_rr <- delay_adjusted_forecasts[delay_adjusted_forecasts$X == truth_rr,paste0("V",step_ahead)]
    prob_non_delay_adjusted_rr <- non_delay_adjusted_forecasts[non_delay_adjusted_forecasts$X == truth_rr,paste0("V",step_ahead)]
    prob_true_rr <- true_forecasts[true_forecasts$X == truth_rr,paste0("V",step_ahead)]
    
    if (score == "MULTIBIN"){
      prob_delay_adjusted <- sum(prob_delay_adjusted_center,prob_delay_adjusted_l,prob_delay_adjusted_ll,prob_delay_adjusted_r,prob_delay_adjusted_rr)
      prob_non_delay_adjusted <- sum(prob_non_delay_adjusted_center,prob_non_delay_adjusted_l,prob_non_delay_adjusted_ll,prob_non_delay_adjusted_r,prob_non_delay_adjusted_rr)
      prob_true <- sum(prob_true_center,prob_true_r,prob_true_rr,prob_true_ll,prob_true_l)
    }else{
      prob_delay_adjusted <- prob_delay_adjusted_center
      prob_non_delay_adjusted <- prob_non_delay_adjusted_center
      prob_true <- prob_true_center
    }
    delay_adjusted_total_prob <- c(delay_adjusted_total_prob,prob_delay_adjusted)
    non_delay_adjusted_total_prob <- c(non_delay_adjusted_total_prob,prob_non_delay_adjusted)
    true_total_prob <- c(true_total_prob,prob_true)
  }
}

print (c("Score",score))
print ("Delay Adjusted")
print (mean(delay_adjusted_total_prob))
print ("Non-delay Adjusted")
print (mean(non_delay_adjusted_total_prob))
print ("True")
print (mean(true_total_prob))