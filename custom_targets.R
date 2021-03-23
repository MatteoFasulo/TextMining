custom_labeling <- function(df){
  unlabled_df <- read.csv(file = df, sep = ",")
  unlabled_df$intention = FALSE
  unlabled_df$adv = FALSE
  last_tweet <- nrow(unlabled_df)
  for (n_tweet in 1:as.numeric(last_tweet)){
    message("Showing tweet number : ",n_tweet)
    print(unlabled_df$testo[n_tweet])
    scelta_1 <- readline(prompt = "Is this adv? ")
    unlabled_df$adv[n_tweet] <- as.character(scelta_1)
    scelta_2 <- readline(prompt = "Is this intention? ")
    unlabled_df$intention[n_tweet] <- as.character(scelta_2)
  }
  labled_df <- as.data.frame(unlabled_df)
  return (labled_df)
}

labled_df <- custom_labeling(df = "20210319-tweets_italy-travel-5.csv")
labeled_df <- as.data.frame(labled_df)
write.csv(labeled_df,file = "20210319-tweets_italy-travel-5-labeled.csv",fileEncoding = "UTF-8")
