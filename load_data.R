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