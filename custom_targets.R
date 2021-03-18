custom_labeling <- function(file){
  unlabled_df <- read.csv(file = file, sep = ";")
  unlabled_df$intention = FALSE
  unlabled_df$adv = FALSE
  last_tweet <- length(unlabled_df)
  
  for (n_tweet in 1:last_tweet){
    message("Showing tweet number : ",n_tweet)
    unlabled_df[n_tweet]$text
    scelta_1 <- readline(prompt = "Is this adv? ")
    unlabled_df[n_tweet]$adv <- scelta_1
    scelta_2 <- readline(prompt = "Is this intention? ")
    unlabled_df[n_tweet]$intention <- scelta_2
    
  }
  
}
