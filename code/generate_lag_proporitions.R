download_backfill_data <- function(){
  library(plyr) # for rbind.fill
  library(dplyr)
  source("https://raw.githubusercontent.com/cmu-delphi/delphi-epidata/master/src/client/delphi_epidata.R")
  
  # Fetch data
  all_obs <- lapply(c("nat", paste0("hhs", 1:10)),
                    function(region_val) {
                      lapply(0:51,
                             function(lag_val) {
                               obs_one_lag <- Epidata$fluview(
                                 regions = list(region_val),
                                 epiweeks = list(Epidata$range(199740, 201815)),
                                 lag = list(lag_val))
                               
                               lapply(obs_one_lag$epidata,
                                      function(x) {
                                        x[sapply(x, function(comp) is.null(comp))] <- NA
                                        return(as.data.frame(x))
                                      }) %>%
                                 rbind.fill()
                             }) %>%
                        rbind.fill()
                    }) %>%
    rbind.fill()
  
  saveRDS(all_obs,
          file = "flu_data_with_backfill.rds")
  
}
#download_backfill_data()
data <- readRDS("flu_data_with_backfill.rds")

lag_df <- matrix(NA,ncol=length(unique(data$lag))+2)

for (region in unique(data$region)){
  for (week in unique(data$epiweek)){
    tmp_data <- data[data$region == region &data$epiweek == week,]
    tmp_row <- c()
    for (lag in unique(tmp_data$lag)){
      current_observed_data <- tmp_data[tmp_data$lag == lag,]$wili
      finally_observed_data <- tmp_data[tmp_data$lag == max(tmp_data$lag),]$wili
      prop <- current_observed_data/finally_observed_data
      tmp_row <- c(tmp_row,prop)
    }
    while (length(tmp_row) < 52){
      tmp_row <- c(tmp_row, NA)
    }
    lag_df <- rbind(lag_df,c(region,week,tmp_row))
  }
}

lag_df <- as.data.frame(lag_df)
lag_df <- lag_df[2:nrow(lag_df),]
colnames(lag_df) <- c("Region","week",seq(0,51))

write.csv(lag_df,"lag_df")
