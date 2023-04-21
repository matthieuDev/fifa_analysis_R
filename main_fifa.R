load_fifa <- function() {
  fifa <- read.csv('fifa23_steam_reviews.csv')
  for (column in c('created', 'author_last_played')){
    fifa[, column] <- as.POSIXct(fifa[, column], format="%Y-%m-%d %H:%M:%S", tz="UTC")
  }
  
  for (column in c('voted_up', 'steam_purchase', 'recieved_for_free', 'written_during_early_access')){
    fifa[, column] <- as.logical(fifa[, column])
  }
  
  fifa
}

get_upvote_score_evolution <- function() {
  voted_up <- fifa[order(fifa$created),]$voted_up
  last_val <- 0
  res <- c()
  for (has_voted in voted_up) {
    last_val <- last_val + (if(has_voted)1 else -1)
    res <- c(res, last_val)
  }
  plot(res, type='l', main='Evolution upvote score', xlab='nth user', ylab='upvote score')
}

draw_hour_evolution_upvote <- function(vec_time, ylab, title_) {
  #+1 because tabulate only count after 0
  by_minute <- (tabulate(as.numeric(vec_time) %% (3600 * 24) / 3600 + 1))
  #add to have the 24:00 and see the loop
  by_minute <- c(by_minute, by_minute[1])
  
  
  to_hour <- function(x) {
    sprintf("%02d:%02d", floor(x), floor(floor(x) %% 1))
  }
  date_xaxis <- sapply(seq(0,24, 25 / length(by_minute)), to_hour)
  plot(seq(0,24), by_minute, type='l', xaxt = "n", xlab='heure', ylab=ylab, main=title_,)
  axis(1,at=c(0,6, 12, 18, 24),labels=c('00:00','06:00','12:00','18:00', '24:00'))
}


get_nb_comments_less_than_10_upvote <- function(vec_less_10) {
  #+1 is because tabulate only start at 1
  res <- tabulate( vec_less_10 + 1, nbins=11)
  #replace 0 by NaN
  res %>% map_dbl(function(x) if (x) x else NaN)
}

compare_comments_for_comments_under_10 <- function(fifa, bool_column, true_str, false_str, title_) {
  nb_true <- sum(fifa[, bool_column])
  nb_false <- length(fifa$votes_up) - nb_true
  
  fifa_not_upvoted <- fifa[fifa$votes_up <= 10,]
  
  voted_up_true <- fifa_not_upvoted[fifa_not_upvoted[, bool_column], ]
  voted_up_false <- fifa_not_upvoted[(!fifa_not_upvoted[, bool_column]), ]
  
  count_up_true <- get_nb_comments_less_than_10_upvote(voted_up_true$votes_up)
  #add factor to allow comparison between the true and the false 
  normalized_count_up_false <- round(get_nb_comments_less_than_10_upvote(voted_up_false$votes_up)/ nb_false * nb_true)
  
  colours <- c("green","red")
  
  df <- t(data.frame(count_up_true, normalized_count_up_false))
  barplot(
    df, 
    col=colours,
    log="y", beside=TRUE,
    ylab='Number of Comments', xlab='Number upvote',
    main=title_
  )
  legend('topright', fill=colours, legend=c(true_str, false_str))
  axis(1, at=seq(2, 32, 3), labels=seq(0,10))
}

compare_upvote_distribution_on_bool <- function(fifa, bool_column, true_str, false_str, title_) {
  #set at 10 because otherwise almost everything is between 0 and 10
  fifa_was_upvoted = fifa[fifa$votes_up > 10,]
  
  voted_up_true = fifa_was_upvoted[fifa_was_upvoted[, bool_column], ]
  voted_up_false = fifa_was_upvoted[(!fifa_was_upvoted[, bool_column]), ]
  
  boxplot(
    voted_up_false$votes_up, voted_up_true$votes_up,
    at=c(1,2),
    names = c(true_str, false_str),
    log="y",
    main=title_
  )
}