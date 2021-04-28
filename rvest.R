################################################################################
install.packages(c("rvest","stringr","word2vec","udpipe","philentropy","stopwords","syuzhet","tm","textclean","wordcloud","SnowballC"))
################################################################################
library(rvest)
library(stringr)
################################################################################

one_page_scraper <- function(url, page = 1, throttle = 0)
{
  # FLOOD PREVENTION
  sec = 0
  if(throttle < 0) warning("throttle was less than 0: set to 0")
  if(throttle > 0) sec = max(0, throttle + runif(1, -1, 1))
  
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
  
  comments <- doc %>%
    html_nodes("#cm_cr-review_list .review-text") %>%
    html_text()  %>%
    str_replace(pattern = "Your browser does not support HTML5 video.", replacement = "") %>%
    str_trim()
  
  # Combine fields into data frame
  df <- data.frame(
    title, 
    author, 
    state, 
    date, 
    stars, 
    comments, stringsAsFactors = F)
  return(df)
}

cycle_scraper <- function(product_id, from_page = 1, to_page)
{
  # Get N (to_page) dozens of reviews from URL
  increment_page <- 1
  reviews_all <- NULL
  page_range <- from_page:to_page
  for(page_num in from_page:to_page){
    url <- paste("https://www.amazon.com/product-reviews/",as.character(product_id),"/?pageNumber=",as.character(page_num),sep = "")
    reviews <- one_page_scraper(url, throttle = 5)
    reviews_all <- rbind(reviews_all, cbind(reviews))
    message("Getting page ",increment_page, " of ",length(page_range), "; Actual: page ",page_num)
    increment_page <- increment_page + 1
  }
  return(reviews_all)
}

recensioni <- cycle_scraper(product_id = "B084J4MZK8",from_page = 1, to_page = 10)

##################################ANALIZE#######################################
library(word2vec)
library(udpipe)
library(philentropy)
library(stopwords)
library(syuzhet)
library(tm)
library(textclean)
library(ggplot2)

preprocessing <- function(df){
  recensioni_frasi <- get_sentences(df$comments)
  recensioni_corpus <- VCorpus(VectorSource(recensioni_frasi))
  recensioni_processed <- tm_map(recensioni_corpus, removeNumbers)
  recensioni_processed <- tm_map(recensioni_processed, removePunctuation)
  ndf <- data.frame(text=unlist(sapply(recensioni_processed,'[',"content")))
  ndf$text <- tolower(ndf$text)
  ndf$text <- str_trim(ndf$text)
  ndf_frasi <- get_sentences(ndf$text)
  ndf_corpus <- VCorpus(VectorSource(ndf_frasi))
  ndf_processed <- tm_map(ndf_corpus, removeWords, stopwords("english"))
  ndf_processed <- tm_map(ndf_processed, stemDocument)
  ndf_final <- tm_map(ndf_processed, stripWhitespace)
  complete_df <- data.frame(text=unlist(sapply(ndf_final,'[',"content"))) #RICONVERTIRE A DATA FRAME
  complete_df$text <- tolower(complete_df$text)
  complete_df$text <- str_trim(complete_df$text)
  return(complete_df)
}
recensioni_processed <- preprocessing(recensioni)

word_analytics <- function(df){
  parole <-strsplit(df$text, " ",fixed = TRUE)
  par <- unlist(parole)
  conti <- table(par)
  conti <- sort(conti, decreasing =TRUE)
  return(conti)
}
freq_word <- word_analytics(recensioni_processed)
parole <- as.data.frame(freq_word[1:20])

par(las=2)
barplot(freq_word[1:20], main = "Frequenza parole", xlab = "Parole")
ggplot(data = parole, aes(x=par, y=Freq)) + geom_bar(stat = "identity")

stars_count <- function(df){
  count <- tapply(recensioni$comments, recensioni$stars, length)
  return(count)
}
stars <- stars_count(recensioni)

par(las=1)
barplot(stars)
pie(stars)
ggplot(data = as.data.frame(stars), aes(x=row.names(as.data.frame(stars)), y=stars)) + geom_bar(stat = "identity")
ggplot(data = as.data.frame(stars), aes(x="", y=stars, fill=LETTERS[1:5])) + 
              geom_bar(stat="identity", width=1, color="white") + 
              coord_polar("y", start=0)
####################################WORDCLOUD###################################
library(wordcloud)

plot_cloud <- function(df){
  list_phrases <- get_sentences(df$text)
  word_corpus <- VCorpus(VectorSource(list_phrases))
  return(wordcloud(word_corpus, max.words = 200, min.freq = 1, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2")))
}
plot_cloud(recensioni_processed)

################################################################################
library(syuzhet)
sentences <- get_sentences(recensioni$comments)
syuzhet <- as.data.frame(get_sentiment(sentences, method = "syuzhet", language = "english"))
bing <- as.data.frame(get_sentiment(sentences, method = "bing", language = "english"))
afinn <- as.data.frame(get_sentiment(sentences, method = "afinn", language = "english"))
sentences[122] #min Bing
sentences[246] #min AFINN & Syuzhet
plot(density(x = syuzhet[,1], kernel= "gaussian"))

library(RSentiment)
calculate_score(sentences)
categorie <- as.matrix(calculate_sentiment(sentences))
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
categorie <- as.matrix(calculate_total_presence_sentiment(sentences))
barplot(sort(as.integer(categorie[2,]),decreasing = TRUE))

library(sentimentr)
sentences <- get_sentences(recensioni$comments)
replace_emoji(sentences)
pippo <- sentiment(sentences)
gruppi <- sentiment_by(sentences)
highlight(gruppi)
plot(gruppi)

library(SentimentAnalysis)
analyzeSentiment(x = recensioni$comments)
