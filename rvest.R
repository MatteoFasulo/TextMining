library(rvest)
library(stringr)
--------------------------------------------------------------------------------

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
  df <- data.frame(title, author, state, date, stars, comments, stringsAsFactors = F)
  return(df)
}

cycle_scraper <- function(product_id, to_page)
{
  # Get N (to_page) dozens of reviews from URL
  reviews_all <- NULL
  for(page_num in 1:to_page){
    url <- paste("https://www.amazon.it/product-reviews/",as.character(product_id),"/?pageNumber=",as.character(page_num),sep = "")
    reviews <- one_page_scraper(url, throttle = 5)
    reviews_all <- rbind(reviews_all, cbind(reviews))
  }
  return(reviews_all)
}

product_id ="B00FOQZZ9G"
recensioni <- cycle_scraper(product_id, to_page = 11)
setwd("./")
write.csv(recensioni,paste("./",as.character(product_id),".csv",sep = ""), row.names = FALSE)
