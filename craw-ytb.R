library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)
library(tidyverse)
library(XML)

library(rvest)
library(tuber)


# Start selenium server
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

# Navigate to the video page
remDr$navigate("https://www.youtube.com/watch?v=eap62CrRtgg")
Sys.sleep(10)
pause <- remDr$findElement("css","button.ytp-play-button") 
pause$clickElement()

# Scroll down to load all comments
last_height <- (remDr$executeScript("return document.documentElement.scrollHeight"))[[1]][1]
while (TRUE) {
  remDr$executeScript("window.scrollTo(0, document.documentElement.scrollHeight);")
  Sys.sleep(3)
  new_height <- (remDr$executeScript("return document.documentElement.scrollHeight"))[[1]][1]
  if (new_height == last_height) {
    break
  }
  last_height <- new_height
}

# all replies button
view_replies_buttons <- remDr$findElements(using = "css", "ytd-button-renderer.more-button")
for (button in view_replies_buttons) {
  button$clickElement()
}

view_replies_buttons <- remDr$findElements(using = "css selector", "#more-replies")
for (i in 1:length(view_replies_buttons)) {
  view_replies_buttons[[i]]$clickElement()
}

# Find all comment elements
comment_elems <- remDr$findElements("css", "#content-text")

# Extract comment text
comment_text <- lapply(comment_elems, function(x) {
  x$getElementText()[[1]]
})

# Create a data frame with the comments
comments_df <- tibble(comments = unlist(comment_text))
comments_df
