library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)


url1 <- "http://skdlxm347811.blogspot.com/2016/07/"
page <- c(1:24)
url2 <- ".html"

url <- paste0(url1,page,url2)


for (i in 1:length(url)){
body <- url[i] %>% read_html()

img <- body %>% 
  html_nodes("div.separator") %>%
  html_nodes("img") %>% 
  html_attr("src")

for (j in 1:length(img)){
download.file(img[j], destfile= paste0(i,"image", j,".jpg"), method='curl')
}
}





