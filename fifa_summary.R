library(dplyr)

compare_up_and_down <- function() {
  fifa %>%
    mutate(vote_status = if_else(voted_up, 'upvote', 'downvote')) %>%
    group_by(vote_status) %>%
    summarise(
      mean_votes_up = mean(votes_up),
      mean_comment_count= mean(comment_count),
      mean_num_games_owned = mean(author_num_games_owned),
      mean_num_reviews = mean(author_num_reviews),
      mean_playtime_forever = mean(author_playtime_forever),
      mean_playtime_last_two_weeks = mean(author_playtime_last_two_weeks),
    ) 
}


get_info_vote_by_month <- function() {
  fifa %>%
    mutate(month_year = format(created, "%B %Y")) %>%
    select(created, month_year, voted_up) %>%
    group_by(month_year) %>%
    summarise(nb_votes=length(voted_up), ratio_upvotes=mean(voted_up), created=first(created)) %>%
    arrange(created) %>%
    select(created=month_year, nb_votes, ratio_upvotes)
}

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


