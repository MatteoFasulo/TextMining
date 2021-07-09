#################################START##########################################
if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(rvest, dplyr, tidyr, stringr, stopwords, syuzhet, tm, ggplot2, hrbrthemes, wordcloud, xlsx, textdata, sentimentr,lubridate,purrr,forcats,BTM,scales,udpipe,data.table,textplot,ggraph,topicmodels,tidytext,word2vec)


one_page_scraper <- function(url, reviewer = T, throttle = 0){
  
  if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
  pacman::p_load(RCurl, XML, dplyr, stringr, rvest, purrr, xlsx)
  
  # FLOOD PREVENTION
  sec = 0
  if(throttle < 0) warning("throttle was less than 0: set to 0")
  if(throttle > 0) sec = max(0, throttle + runif(1, -1, 1))
  
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)
  
  # HTML of URL
  doc <- read_html(url)
  
  # Parse elements from HTML
  title <- doc %>%
    html_nodes("#cm_cr-review_list .a-color-base") %>%
    html_text() %>%
    str_trim()
  
  author <- doc %>%
    html_nodes("#cm_cr-review_list .a-profile-name") %>%
    html_text() %>%
    str_trim()
  
  state <- doc %>%
    html_nodes("#cm_cr-review_list .review-date") %>%
    html_text() %>%
    gsub(".*the ","", .) %>%
    gsub("on.*","", .)
  
  date <- doc %>%
    html_nodes("#cm_cr-review_list .review-date") %>%
    html_text() %>% 
    gsub(".*on ", "", .)
  
  stars <- doc %>%
    html_nodes("#cm_cr-review_list  .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  helpful <- doc %>%
    html_nodes("#cm_cr-review_list  .cr-vote-text") %>%
    html_text() %>%
    gsub(",","",.)%>%
    str_extract("[:digit:]+|One") %>%
    gsub("One", "1", .) %>%
    as.numeric()
  
  comments <- doc %>%
    html_nodes("#cm_cr-review_list .review-text") %>%
    html_text()  %>%
    str_replace(pattern = "Your browser does not support HTML5 video.", replacement = "") %>%
    str_trim()
  
  ver.purchase <- doc%>%
    html_nodes(".review-data.a-spacing-mini") %>%
    html_text() %>%
    grepl("Verified Purchase", .) %>%
    as.numeric()
  
  format <- doc %>% 
    html_nodes(".review-data.a-spacing-mini") %>% 
    html_text() %>%
    gsub("Color: |\\|.*|Verified.*", "", .)
  
  suppressWarnings(n_helpful <- doc %>%
                     html_nodes(".a-expander-inline-container") %>%
                     html_text() %>%
                     gsub("\n\n \\s*|found this helpful.*", "", .) %>%
                     gsub("One", "1", .) %>%
                     map_chr(~ str_split(string = .x, pattern = " ")[[1]][1]) %>%
                     as.numeric())
  
  if(reviewer == T){
    
    rver_url <- doc %>%
      html_nodes(".review-byline .author") %>%
      html_attr("href") %>%
      gsub("/ref=cm_cr_othr_d_pdp\\?ie=UTF8", "", .) %>%
      gsub("/gp/pdp/profile/", "", .) %>%
      paste0("https://www.amazon.com/gp/cdp/member-reviews/",.) 
    
    #average rating of past 10 reviews
    rver_avgrating_10 <- rver_url %>%
      sapply(., function(x) {
        read_html(x) %>%
          html_nodes(".small span img") %>%
          html_attr("title") %>%
          gsub("out of.*|stars", "", .) %>%
          as.numeric() %>%
          mean(na.rm = T)
      }) %>% as.numeric()
    
    rver_prof <- rver_url %>%
      sapply(., function(x) 
        read_html(x) %>%
          html_nodes("div.small, td td td .tiny") %>%
          html_text()
      )
    
    rver_numrev <- rver_prof %>%
      lapply(., function(x)
        gsub("\n  Customer Reviews: |\n", "", x[1])
      ) %>% as.numeric()
    
    rver_numhelpful <- rver_prof %>%
      lapply(., function(x)
        gsub(".*Helpful Votes:|\n", "", x[2]) %>%
          trim()
      ) %>% as.numeric()
    
    rver_rank <- rver_prof %>%
      lapply(., function(x)
        gsub(".*Top Reviewer Ranking:|Helpful Votes:.*|\n", "", x[2]) %>%
          removePunctuation() %>%
          trim()
      ) %>% as.numeric()
    
    # Combine fields into data frame
    df <- data.frame(title, date, ver.purchase, format, stars, comments, helpful,
                     rver_url, rver_avgrating_10, rver_numrev, rver_numhelpful, rver_rank, stringsAsFactors = F)
    
  }
  else
    if (length(helpful) < 10){
      helpful <- NA
    }
  df <- data.frame(title, author, state, date, ver.purchase, helpful, format, stars, comments, stringsAsFactors = F)
  
  return(df)
}
cycle_scraper <- function(product_id, from_page = 1, to_page){
  # Get N (to_page) dozens of reviews from URL
  reviews_all <- NULL
  page_range <- from_page:to_page
  for(page_num in from_page:to_page){
    url <- paste("https://www.amazon.com/product-reviews/",as.character(product_id),"/?pageNumber=",as.character(page_num),sep = "")
    reviews <- one_page_scraper(url, reviewer = F, throttle = 15)
    reviews_all <- rbind(reviews_all, cbind(reviews))
    message("Getting page ",page_num, " of ",length(page_range), "; Current elements:",page_num * 10)
  }
  write.csv(reviews_all,"alexa_echo_dot.csv", row.names = FALSE)
  write.xlsx(reviews_all, file = "alexa_echo_dot.xlsx", sheetName = "alexa_echo_dot", append = FALSE)
  return(reviews_all)
}

review <- cycle_scraper(product_id = "B084J4MZK8",from_page = 1, to_page = 500)

review <- read.csv("alexa_echo_dot.csv", encoding = "utf-8") #in case of no scraping

word_analytics <- function(df){
  parole <-strsplit(df$text, " ",fixed = TRUE)
  par <- unlist(parole)
  conti <- table(par)
  conti <- sort(conti, decreasing =TRUE)
  conti <- conti[-4]
  words <- as.data.frame(conti[1:20])
  return(words)
}
stemmed_reviews <- function(df){
  sentences <- syuzhet::get_sentences(df$comments)
  corpus <- VCorpus(VectorSource(sentences))
  release_corpus <- tm_map(corpus, removeNumbers)
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    gsub(x, pattern = "[[:punct:]]", replacement = " ")))
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    tolower(x)))
  release_corpus <- tm_map(release_corpus, removeWords, words = stopwords("en"))
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    gsub(x, pattern = "[^0-9a-zA-Z]+",replacement= " ")))
  release_corpus <- tm_map(release_corpus, stripWhitespace)
  release_corpus <- tm_map(release_corpus, stemDocument)
  release_corpus <- tm_map(release_corpus, removeWords, c("t","s","can","get","just","rd","m","th","thing","also","don","want","got","now","alexa","echo","dot")) 
  release_corpus <- tm_map(release_corpus, stripWhitespace)
  stemmed_df <- data.frame(text=unlist(sapply(release_corpus,'[',"content")))
  return(stemmed_df)
}
cleaned_reviews <- function(df){
  sentences <- syuzhet::get_sentences(df$comments)
  corpus <- VCorpus(VectorSource(sentences))
  release_corpus <- tm_map(corpus, removeNumbers)
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    gsub(x, pattern = "[[:punct:]]", replacement = " ")))
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    tolower(x)))
  release_corpus <- tm_map(release_corpus, removeWords, words = stopwords("en"))
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    gsub(x, pattern = "[^0-9a-zA-Z]+",replacement= " ")))
  release_corpus <- tm_map(release_corpus, stripWhitespace)
  release_corpus <- tm_map(release_corpus, removeWords, c("t","s","can","get","just","rd","m","th","thing","also","don","want","got","now","alexa","echo","dot"))
  release_corpus <- tm_map(release_corpus, stripWhitespace)
  cleaned_df <- data.frame(text=unlist(sapply(release_corpus,'[',"content")))
  return(cleaned_df)
}
stemmed_df <- stemmed_reviews(review)
cleaned_df <- cleaned_reviews(review)

plot_cloud <- function(df){
  list_phrases <- get_sentences(df$text)
  word_corpus <- VCorpus(VectorSource(list_phrases))
  return(wordcloud(word_corpus, max.words = 200, min.freq = 1, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2")))
}

stars_count <- function(df){
  count <- tapply(df$comments, df$stars, length)
  stars <- as.data.frame(count)
  return(stars)
}

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

emotion_classification <- function(df){
  emotions <- get_nrc_sentiment(df$comments)
  t_emotions<-data.frame(t(emotions))
  td_new <- data.frame(rowSums(t_emotions[2:253]))
  names(td_new)[1] <- "count"
  td_new <- cbind("sentiment" = rownames(td_new), td_new)
  rownames(td_new) <- NULL
  td_new2<-td_new[1:8,]
  return(td_new2)
}
emotions <- emotion_classification(review)

sentences <- sentimentr::get_sentences(review$comments)
sentiment <- sentiment(sentences)
gruppi <- sentiment_by(sentences)

sentiment_words <- extract_sentiment_terms(sentences)
sentiment_counts <- attributes(sentiment_words)$counts

polarity <- function(df){
  negatives <- length(which(df$ave_sentiment < 0))
  positives <- length(which(df$ave_sentiment > 0))
  neutrals <- nrow(df) - (negatives + positives)
  table <- cbind(positives, neutrals, negatives)
  new_df <- data.frame(name=c("positives","neutrals","negatives"),value=c(positives,neutrals,negatives))
  return(new_df)
}

fix_date <- function(df){
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
  df["ave_sentiment"] <- gruppi$ave_sentiment
  df <- df %>%
    mutate(
      date = as.Date(date, format="%B %d, %Y"),
      dir = sign(ave_sentiment)
    ) %>%
    group_by(date) %>%
    summarise(
      avg_sent = mean(ave_sentiment)
    ) %>%
    ungroup()
  return(df)
}
year_comparison <- fix_date(review)

stars_over_time <- function(df){
  df <- df %>%
    mutate(
      date = as.Date(date, format="%B %d, %Y"),
      dir = sign(stars)
    ) %>%
    group_by(date) %>%
    summarise(
      stars = mean(stars)
    ) %>%
    ungroup()
  return(df)
}

df_annotator <- function(df){
  x <- df
  x$doc_id <- c(1:nrow(df))
  x$text <- tolower(paste(df$title,df$comments,sep="\n"))
  x$text <- gsub("'","", x$text)
  x$text <- gsub("<.+>","",x$text)
  anno <- udpipe(x, "english", trace=10)
  return(anno)
}
review_annotated <- df_annotator(review)

################################################################################
ggplot(data = word_analytics(stemmed_df), aes(x=par, y=Freq)) + 
  geom_bar(stat = "identity") +
  labs(title = "Words", 
       subtitle = "Top 20 for frequency",
       x = "Words",
       y = "Frequence",
       caption = "(based on previous stemming phase)") +
  theme(axis.text.x = element_text(angle = 90), 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

plot_cloud(stemmed_df)

if("helpful" %in% colnames(review)) 
{ # HELPFUL DISTRIBUTION
  ggplot(data = review, mapping = aes(x = helpful, y = stars, color = stars)) + 
    layer(geom = "point", stat="identity", position = position_dodge(width = 0.1)) +
    labs(x="Number of helpful votes", y= "Star rating", color = "Star rating")
}

if("ver.purchase" %in% colnames(review))
{ # VERIFIED PURCHASE BARPLOT
  ggplot(data = review, aes(x = stars)) + 
    geom_bar(aes(fill = ver.purchase)) +
    labs(x = "Star Rating", y = "Frequency", fill = "Verified Purchase")
} 

ggplot(data = stars_count(review), aes(x=row.names(stars_count(review)), y=count/sum(count),color=row.names(stars_count(review)))) + 
  geom_hline(yintercept = max(stars_count(review)$count/sum(stars_count(review)$count)),linetype="dashed", color ="black") +
  geom_bar(stat = "identity", fill="white") +
  scale_y_continuous(labels=scales::percent) +
  scale_color_brewer(palette="Dark2") +
  labs(title = "Stars", subtitle = "Bar Plot", x = "Stars", y = "Percentage",color='Star Rating', caption = "(based on entire dataset)")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggplot(data = stars_count(review), aes(x="", y=count, fill=row.names(stars_count(review)))) + 
  geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) +
  labs(title = "Stars", subtitle = "Pie Chart",x = NULL,y = NULL, fill='Star Rating', caption = "(based on entire dataset)") +
  scale_fill_grey() +
  blank_theme +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

quickplot(sentiment, data=emotions, weight=count, geom="bar",fill=sentiment, ylab="count")+
  scale_fill_hue(c = 45) +
  ggtitle("Survey sentiments")

ggplot(data = sentiment, aes(x=sentiment)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  labs(title = "Sentiment for each sentence", 
       subtitle = "Empirical distribution function",
       x = "Sentiment",
       y = "Density",
       caption = "(based on data from sentimentr library)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggplot(data = gruppi, aes(x=gruppi$ave_sentiment)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  labs(title = "Sentiment for each review", 
       subtitle = "Empirical distribution function",
       x = "Sentiment",
       y = "Density",
       caption = "(based on data from sentimentr library)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

par(mfrow = c(1, 3), mar = c(0, 0, 0, 0))
## Positive Words
with(
  sentiment_counts[polarity > 0,],
  wordcloud(words = words, freq = n, min.freq = 1,
            max.words = 200, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"), scale = c(4.5, .75)
  )
)
mtext("Positive Words", side = 3, padj = 5)

## Negative Words
with(
  sentiment_counts[polarity < 0,],
  wordcloud(words = words, freq = n, min.freq = 1,
            max.words = 200, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"), scale = c(4.5, 1)
  )
)
mtext("Negative Words", side = 3, padj = 5)

sentiment_counts[, 
                 color := ifelse(polarity > 0, 'red', 
                                 ifelse(polarity < 0, 'blue', 'gray70')
                 )]

## Positive & Negative Together
with(
  sentiment_counts[polarity != 0,],
  wordcloud(words = words, freq = n, min.freq = 1,
            max.words = 200, random.order = FALSE, rot.per = 0.35,
            colors = color, ordered.colors = TRUE, scale = c(5, .75)
  )
)
mtext("Positive(red) & Negative(blue) Words", side = 3, padj = 5)

plot(gruppi)

gruppi <- cbind(gruppi, review$stars)
names(gruppi)[names(gruppi) == "V2"] <- "stars"
ggplot(data = gruppi, aes(x=factor(stars), y=gruppi$ave_sentiment, fill=as.factor(stars))) +
  geom_boxplot(alpha=0.5) +
  scale_fill_brewer(name = "Stars", palette="Dark2") +
  labs(title = "Average Sentiment for each star", 
       subtitle = "Box Plot",
       x = "Stars",
       y = "Average Sentiment",
       caption = "(based on data from sentimentr library)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

gruppi %>% 
  mutate(quadrant = case_when(stars > 3.5 & ave_sentiment > 0   ~ "Positive Review/Postive Sentiment",
                              stars <= 3.5 & ave_sentiment > 0  ~ "Negative Review/Positive Sentiment",
                              stars <= 3.5 & ave_sentiment <= 0 ~ "Negative Review/Negative Sentiment",
                              TRUE                                      ~ "Positive Review/Negative Sentiment")) %>% 
  ggplot(aes(x = stars, y = ave_sentiment, color = quadrant)) + 
  geom_hline(yintercept=0, color = "black", size=.5) + 
  geom_vline(xintercept=3.5, color = "black", size=.5) +
  guides(color=FALSE) +
  scale_color_manual(values=c("lightgreen", "pink", "pink","lightgreen")) +
  ggtitle("Amazon Product Rating vs Sentiment Rating of Review") +
  ggplot2::annotate("text", x = 4.33, y=3.5,label="Positive Review/Postive Sentiment") +
  ggplot2::annotate("text", x = 2, y=3.5,label="Negative Review/Positive Sentiment") +
  ggplot2::annotate("text", x = 4.33, y=-2.5,label="Positive Review/Negative Sentiment") +
  ggplot2::annotate("text", x = 2, y=-2.5,label="Negative Review/Negative Sentiment") +
  geom_point()

ggplot(data = polarity(gruppi), 
       aes(
         x=c("Positive","Neutral","Negative"), 
         y=(c(polarity(gruppi)[1,"value"],polarity(gruppi)[2,"value"],polarity(gruppi)[3,"value"])/nrow(gruppi)),
         fill=c("Green","Red","Blue"))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels=scales::percent) +
  labs(title = "Sentiment Polarity", subtitle = "Bar Plot", x = "Polarity", y = "Percentage", fill = "Polarity") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  guides(fill=FALSE)


ggplot(year_comparison, aes(x = date, y = avg_sent)) +
  geom_smooth(method="loess", size=1, se=T, span = .6) +
  geom_vline(xintercept=as.Date("2020-10-22"), linetype="dashed", color = "black") +
  geom_text(aes(as.Date("2020-10-22") + 1, .4, label = "22-10-2020", hjust = "left"), size = 3, color = "black") +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_text(aes(max(date) - 5, -0.02, label = "Neutral Sentiment", vjust = 0), size = 3, color = "red") +
  geom_vline(xintercept=as.Date("2021-07-01"), linetype="dashed", color = "black") +
  geom_text(aes(as.Date("2021-07-01") - 26, .4, label = "01-07-2021", hjust = "left"), size = 3, color = "black") +
  ylab("Avg. Sentiment") +
  xlab("Review Date") +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  scale_x_date(date_labels="%b '%y",date_breaks  ="1 month") +
  ggtitle("Sentiment of Alexa Amazon Reviews, Over Time (10/2020 - 07/2021)")


ggplot(stars_over_time(review), aes(x = date, y = stars)) +
  geom_smooth(method="loess", size=1, se=T, span = .6) +
  geom_vline(xintercept=as.Date("2020-10-22"), linetype="dashed", color = "black") +
  geom_text(aes(as.Date("2020-10-22") + 1, 4.3, label = "22-10-2020", hjust = "left"), size = 3, color = "black") +
  geom_hline(yintercept=3, linetype="dashed", color = "red") +
  geom_text(aes(max(date) -15, 3.05, label = "Neutral Rating", vjust = 0), size = 3, color = "red") +
  geom_vline(xintercept=as.Date("2021-07-01"), linetype="dashed", color = "black") +
  geom_text(aes(as.Date("2021-07-01") -5, 4.3, label = "01-07-2021", hjust = "left"), size = 3, color = "black") +
  ylab("Stars Rating") +
  xlab("Review Date") +
  ylim(1,5) +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  scale_x_date(date_labels="%b '%y",date_breaks  ="1 month") +
  ggtitle("Stars rating of Alexa Amazon Reviews, Over Time (10/2020 - 07/2021)")


btm_model <- function(df, annotated_df){
  biterms <- as.data.table(annotated_df)
  biterms <- biterms[,cooccurrence(x=lemma, relevant=upos %in% c("NOUN","ADJ","VERB") & nchar(lemma)>2 & !lemma %in% stopwords("en"),skipgram=3),by = list(doc_id)]
  traindata <- subset(annotated_df, upos %in% c("NOUN","ADJ","VERB") & !lemma %in% stopwords("en") & nchar(lemma)>2)
  traindata <- traindata[, c("doc_id","lemma")]
  model <- BTM(traindata, biterms = biterms, k = 9, iter=2000, background = TRUE, trace=100)
  return(model)
}
set.seed(123456)
model <- btm_model(review, review_annotated)
plot(model,top_n=10,title ="BTM Model",subtitle ="Topic Classification")

stemmed_corpus <- function(df){
  sentences <- syuzhet::get_sentences(df$comments)
  corpus <- VCorpus(VectorSource(sentences))
  release_corpus <- tm_map(corpus, removeNumbers)
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    gsub(x, pattern = "[[:punct:]]", replacement = " ")))
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    tolower(x)))
  release_corpus <- tm_map(release_corpus, removeWords, words = stopwords("en"))
  release_corpus <- tm_map(release_corpus, stripWhitespace)
  release_corpus <- tm_map(release_corpus, stemDocument)
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    gsub(x, pattern = "[^0-9a-zA-Z]+",replacement= " ")))
  release_corpus <- tm_map(release_corpus, stripWhitespace)
  return(release_corpus)
}
cleaned_corpus <- function(df){
  sentences <- syuzhet::get_sentences(df$comments)
  corpus <- VCorpus(VectorSource(sentences))
  release_corpus <- tm_map(corpus, removeNumbers)
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    gsub(x, pattern = "[[:punct:]]", replacement = " ")))
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    tolower(x)))
  release_corpus <- tm_map(release_corpus, removeWords, words = stopwords("en"))
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    gsub(x, pattern = "[^0-9a-zA-Z]+",replacement= " ")))
  release_corpus <- tm_map(release_corpus, stripWhitespace)
  return(release_corpus)
}
release_corpus <- stemmed_corpus(review)
tdm_maker <- function(corpus){
  tdm <- TermDocumentMatrix(corpus)
  tdm <- removeSparseTerms(tdm, 1-(10/length(corpus)))
  return(tdm)
}
tdm <- tdm_maker(release_corpus)
sound_assoc <- findAssocs(tdm, "sound", corlimit = .1)
sound_assoc


aspect_analysis <- function(df, word=c("Sound","Cord","Music","Light","Voice")){
  word <- tolower(word)
  final_df <- NULL
  for (i in 1:length(word)){
    sound_vec <- df$text[as.vector(which(str_detect(df$text, word[i]) == TRUE))]
    sound_sentences <- sentimentr::get_sentences(sound_vec)
    sound_sentiment <- sentimentr::sentiment(sound_sentences)
    sound_sentiment <- as.data.frame(sound_sentiment)
    sound_sentiment["word"] <- word[i]
    final_df <- rbind(final_df, sound_sentiment)
  }
  return(final_df)
}

ggplot(data = aspect_analysis(cleaned_df), aes(x=factor(str_to_title(word)), y=sentiment, fill=as.factor(str_to_title(word)))) +
  geom_boxplot(alpha=0.5) +
  scale_fill_brewer(name = "Stars", palette="Dark2") +
  labs(title = "Sentiment for each aspect", 
       subtitle = "Box Plot",
       x = "Aspects",
       y = "Sentiment",
       caption = "(based on data from sentimentr library)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


################################################################################
################################OLD#############################################
################################################################################
if(!"pacman" %in% installed.packages()[,"Package"]) install.packages("pacman")
pacman::p_load(rvest, dplyr, tidyr, tidytext, stringr, stopwords, syuzhet, tm, ggplot2, hrbrthemes, wordcloud, xlsx, textdata)
################################################################################
stemmed_corpus <- function(df){
  sentences <- syuzhet::get_sentences(df$comments)
  corpus <- VCorpus(VectorSource(sentences))
  release_corpus <- tm_map(corpus, removeNumbers)
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    gsub(x, pattern = "[[:punct:]]", replacement = " ")))
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    tolower(x)))
  release_corpus <- tm_map(release_corpus, removeWords, words = stopwords("en"))
  release_corpus <- tm_map(release_corpus, stripWhitespace)
  release_corpus <- tm_map(release_corpus, stemDocument)
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    gsub(x, pattern = "[^0-9a-zA-Z]+",replacement= " ")))
  release_corpus <- tm_map(release_corpus, stripWhitespace)
  return(release_corpus)
}
cleaned_corpus <- function(df){
  sentences <- syuzhet::get_sentences(df$comments)
  corpus <- VCorpus(VectorSource(sentences))
  release_corpus <- tm_map(corpus, removeNumbers)
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    gsub(x, pattern = "[[:punct:]]", replacement = " ")))
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    tolower(x)))
  release_corpus <- tm_map(release_corpus, removeWords, words = stopwords("en"))
  release_corpus <- tm_map(release_corpus, content_transformer(function(x) 
    gsub(x, pattern = "[^0-9a-zA-Z]+",replacement= " ")))
  release_corpus <- tm_map(release_corpus, stripWhitespace)
  return(release_corpus)
}

release_corpus <- stemmed_corpus(review)

tdm_maker <- function(corpus){
  tdm <- TermDocumentMatrix(corpus)
  tdm <- removeSparseTerms(tdm, 1-(10/length(corpus)))
  return(tdm)
}
tdm <- tdm_maker(release_corpus)

tdm_bigram <- function(corpus){
  library(RWeka)
  BigramTokenizer <- function(x){
    NGramTokenizer(x, Weka_control(min=2, max=2))}
  tdm_bigram <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
  return(tdm_bigram)
}
tdm_bigram <- tdm_bigram(release_corpus)

ap_td <- tidy(tdm)
ap_td
ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments %>%
  count(sentiment, term, wt = count)%>%
  ungroup()%>%
  filter(n >= 100)%>%
  mutate(n = ifelse(sentiment == "negative", -n, n))%>%
  mutate(term = reorder(term,n))%>%
  ggplot(aes(term,n,fill=sentiment)) +
  geom_bar(stat="identity") +
  ylab("Contribution of Sentiment") +
  xlab("Word") +
  coord_flip()


library(RTextTools)
dtm <- DocumentTermMatrix(release_corpus)
dtm <- removeSparseTerms(dtm, 1-(10/length(release_corpus)))
dtm
container <- create_container(dtm,labels = recensioni$title, trainSize = 1:17900, virgin = TRUE)
slotNames(container)
svm_model <- train_model(container, "SVM")
tree_model <- train_model(container, "TREE")
maxent_model <- train_model(container, "MAXENT")


library(syuzhet)
syuzhet <- as.data.frame(syuzhet::get_sentiment(syuzhet::get_sentences(review$comments), method = "syuzhet", language = "english"))
bing <- as.data.frame(syuzhet::get_sentiment(syuzhet::get_sentences(review$comments), method = "bing", language = "english"))
afinn <- as.data.frame(syuzhet::get_sentiment(syuzhet::get_sentences(review$comments), method = "afinn", language = "english"))
nrc <- as.data.frame(syuzhet::get_sentiment(syuzhet::get_sentences(review$comments), method = "nrc", language = "english"))

ggplot(data = syuzhet, aes(x=syuzhet[,1])) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  labs(title = "Syuzhet's sentiment", 
       subtitle = "Empirical distribution function",
       x = "Sentiment",
       y = "Density",
       caption = "(based on data from syuzhet lexicon)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggplot(data = bing, aes(x=bing[,1])) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  labs(title = "Bing's sentiment", 
       subtitle = "Empirical distribution function",
       x = "Sentiment",
       y = "Density",
       caption = "(based on data from bing lexicon)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggplot(data = afinn, aes(x=afinn[,1])) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  labs(title = "Afinn's sentiment", 
       subtitle = "Empirical distribution function",
       x = "Sentiment",
       y = "Density",
       caption = "(based on data from afinn lexicon)") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


library(word2vec)
library(philentropy)
model <- word2vec(x = review$comments, type = "skip-gram",dim = 10, iter = 30)
vet <- predict(model,"echo",type = "embedding")
vet_2 <- predict(model,"dot",type = "embedding")
parolas <- rbind(vet, vet_2)
distance(parolas, method = "euclidean")

vet_parola <- predict(model,"echo",type = "nearest")
vet_parola
