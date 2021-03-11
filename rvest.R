library(rvest)
--------------------------------------------------------------------------------
product_info <- function(url)
{
  url <- "https://www.amazon.it/Smartphone-Fotocamere-Posteriori-Espandibili-Batteria/dp/B087XGFYDW/ref=sr_1_9?__mk_it_IT=%C3%85M%C3%85%C5%BD%C3%95%C3%91&dchild=1&keywords=samsung&qid=1615487745&sr=8-9"
  webdata <- read_html(url)
  #webdata %>%
  #html_nodes("p") %>%
  #html_text()
  title_html <- html_nodes(webdata, 'h1#title')
  title <- html_text(title_html)
  title <- str_replace_all(title, "[\r\n]" , "")
  
  price_html <- html_nodes(webdata, 'span#priceblock_ourprice')
  price <- html_text(price_html)
  price <- str_replace_all(price, "[\r\n]" , "")
  
  desc_html <- html_nodes(webdata, 'div#productDescription')
  prod_desc <- html_text(desc_html)
  prod_desc <- str_replace_all(prod_desc, "[\r\n\t]" , "")
  prod_desc <- str_trim(prod_desc)
  #head(prod_desc)
  
  color_html <- html_nodes(webdata, 'div#variation_color_name')
  color_html <- html_nodes(color_html, 'span.selection')
  color <- html_text(color_html)
  color <- str_trim(color)
  #head(color)
  
  rate_html <- html_nodes(webdata, 'span#acrPopover')
  rate <- html_text(rate_html)
  rate <- str_replace_all(rate, "[\r\n\t]" , "")
  rate <- str_trim(rate)
  rate <- rate[1]
  
  number_ratings <- html_nodes(webdata, 'span#acrCustomerReviewText')
  number_ratings <- html_text(number_ratings)
  number_ratings <- str_replace_all(number_ratings, "[\r\n\t]" , "")
  number_ratings <- str_trim(number_ratings)
  number_ratings <- number_ratings[1]
  
  result <- c(title, price, prod_desc, color, rate, number_ratings)
  
  return(result)
}

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
    html_text()
  
  author <- doc %>%
    html_nodes("#cm_cr-review_list .a-profile-name") %>%
    html_text()
  
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
    html_text() 
  
  # Combine fields into data frame
  df <- data.frame(title, author, date, stars, comments, stringsAsFactors = F)
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

recensioni <- cycle_scraper(product_id = "B086B5K94Y", to_page = 20)




