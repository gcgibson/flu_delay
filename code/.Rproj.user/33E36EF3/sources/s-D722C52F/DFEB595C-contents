data <- readRDS("flu_data_with_backfill.rds")

fully_observed_data <- data %>% group_by(region,epiweek) %>%
  filter(lag == max(lag))

fully_observed_data <- fully_observed_data[order(fully_observed_data$epiweek),]

write.csv(fully_observed_data,"fully_observed_data")
