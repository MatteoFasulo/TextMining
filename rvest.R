################################################################################
install.packages("rvest","stringr","word2vec","udpipe","philentropy","stopwords","syuzhet","tm","textclean","wordcloud")
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
    gsub(".*on ", "", .) %>%
    str_remove_all(pattern = "Recensito ") %>%
    str_remove_all(pattern = "in ") %>%
    str_remove_all(pattern = " il ") %>%
    str_remove_all(pattern = "nel ") %>%
    str_remove_all(pattern = "negli ") %>%
    str_remove_all(pattern = "[:digit:]") %>%
    str_trim() 
    state <- word(state, 1, -2)
  
  date <- doc %>%
    html_nodes("#cm_cr-review_list .review-date") %>%
    html_text() %>% 
    gsub(".*on ", "", .) 
    date <- word(date,5,-1) %>% 
    str_remove_all(pattern = "il ")
  
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
    url <- paste("https://www.amazon.it/product-reviews/",as.character(product_id),"/?pageNumber=",as.character(page_num),sep = "")
    reviews <- one_page_scraper(url, throttle = 5)
    reviews_all <- rbind(reviews_all, cbind(reviews))
    message("Getting page ",increment_page, " of ",length(page_range), "; Actual: page ",page_num)
    increment_page <- increment_page + 1
  }
  return(reviews_all)
}

recensioni <- cycle_scraper(product_id = "B07PHPXHQS",from_page = 1, to_page = 10)

##################################ANALIZE#######################################
library(word2vec)
library(udpipe)
library(philentropy)
library(stopwords)
library(syuzhet)
library(tm)
library(textclean)

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
  ndf_processed <- tm_map(ndf_corpus, removeWords, stopwords("italian"))
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

par(las=2)
barplot(freq_word[1:20], main = "Frequenza parole", xlab = "Parole")

####################################WORDCLOUD###################################
library(wordcloud)

plot_cloud <- function(df){
  list_phares <- get_sentences(df$text)
  word_corpus <- VCorpus(VectorSource(list_phares))
  return(wordcloud(word_corpus, max.words = 200, min.freq = 1, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2")))
}
plot_cloud(recensioni_processed)
