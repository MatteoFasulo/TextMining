library(tidyverse)
library(rvest)
  url <- "https://www.amazon.in/Samsung-Galaxy-Storage-Additional-Exchange/dp/B089MS6KLN/ref=dp_prsubs_1?pd_rd_i=B089MS6KLN&psc=1"

webdata = read_html(url)
webdata %>%
html_nodes("p") %>%
html_text()
title_html <- html_nodes(webdata, 'h1#title')

title <- html_text(title_html)

head(title)
str_replace_all(title, "[\r\n]" , "")
price_html <- html_nodes(webdata, 'span#priceblock_ourprice')

price <- html_text(price_html)

str_replace_all(price, "[\r\n\u20b9 ]" , "")
desc_html <- html_nodes(webdata, 'div#productDescription')
prod_desc <- html_text(desc_html)

prod_desc <- str_replace_all(prod_desc, "[\r\n\t]" , "")
prod_desc <- str_trim(prod_desc)
head(prod_desc)
color_html <- html_nodes(webdata, 'div#variation_color_name')
color_html <- html_nodes(color_html, 'span.selection')
color <- html_text(color_html)
color <- str_trim(color)
head(color)
rate_html <- html_nodes(webdata, 'span#acrPopover')

rate <- html_text(rate_html)

rate <- str_replace_all(rate, "[\r\n\t]" , "")
rate <- str_trim(rate)
rate[1]
number_ratings <- html_nodes(webdata, 'span#acrCustomerReviewText')
number_ratings <- html_text(number_ratings)
number_ratings <- str_replace_all(number_ratings, "[\r\n\t]" , "")
number_ratings <- str_trim(number_ratings)
number_ratings[1]
