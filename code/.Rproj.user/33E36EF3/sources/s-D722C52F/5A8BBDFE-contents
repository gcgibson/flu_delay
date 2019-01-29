incidence_bin_names = as.character(seq(from = 0, to = 13, by = 0.1))

get_inc_bin <- function(inc,max=13,
                        return_character = TRUE) {
  inc <- round(inc, 1)
  bin_numeric <- ifelse(inc < max,
                        floor(inc*10)/10, ## floors to 1st decimal place
                        max)
  if(return_character) {
    return(as.character(bin_numeric))
  } else {
    return(bin_numeric)
  }
}


bin_to_log_prob <- function(ph_inc_bin_by_sim_ind){
  ph_inc_bin_log_probs <- log(sapply(
    incidence_bin_names,
    function(bin_name) {
      sum(ph_inc_bin_by_sim_ind == bin_name)
    })) -
    log(length(ph_inc_bin_by_sim_ind))
  return (exp(ph_inc_bin_log_probs))
}
