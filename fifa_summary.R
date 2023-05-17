library(dplyr)

how_many_bad_present <- function() {
  fifa %>%
    filter(recieved_for_free) %>%
    mutate(present_status = if_else(voted_up, 'good_present', 'bad_present')) %>%
    select(present_status) %>%
    group_by(present_status) %>%
    count(present_status)
}

compare_mean_by_vote_status <- function(column_name) {
  end_column_name <- paste0('mean_', column_name)
  column_name <- ensym(column_name)
  
  fifa %>%
    select(voted_up, !!column_name) %>%
    group_by(voted_up) %>%
    summarise(!!end_column_name := mean(!!column_name))
}


