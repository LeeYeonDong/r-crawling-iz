library(tidyverse) 
library(stringr)
library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)
library(tidyverse)
library(XML)
library(openxlsx)

Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre-1.8')

##cmd 관리자권한으로 실행
# cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

# https://googlechromelabs.github.io/chrome-for-testing/#stable chromedriver downloads
# java -Dwebdriver.chrome.driver="C:\r_selenium\chromedriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

## 다수 유튜브 동영상 댓글 수집
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

start_time <- Sys.time()

# the+load+of+the+rings 검색 결과
Sys.sleep(time = 1)

remDr$navigate("https://www.youtube.com/results?search_query=the+load+of+the+rings")

Sys.sleep(time = 1)

frontpage <- remDr$getPageSource()[[1]]
body <- frontpage %>% read_html() 

Sys.sleep(time = 1)

# 링크 수집
링크_ytb <- body %>% 
  html_nodes("a#video-title") %>%
  html_attr("href")

링크_ytb <- paste0("https://www.youtube.com/",링크_ytb)

webElem <- remDr$findElement("css", "body") # 더 많은 링크를 얻을려고 할때
webElem$sendKeysToElement(list(key = "end"))
Sys.sleep(time = 1)

# 수집할 데이터 프레임 만들기
df_ytb_com <- tibble()
Sys.sleep(time = 1)

# df 만들기
df_ytb_com <- tibble()

# Navigate to the video page
for(i in 1:length(링크_ytb)){
  tryCatch({
  remDr$navigate(링크_ytb[i])
Sys.sleep(3)
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

## 현재페이지 정보 읽어오기
frontpage <- remDr$getPageSource()[[1]]
body <- frontpage %>% read_html() 

Sys.sleep(time = 1)

제목_ytb <- body %>% 
  html_nodes("h1.title") %>%
  html_nodes("yt-formatted-string.style-scope") %>%
  html_text() 
제목_ytb <- 제목_ytb[2]

글쓴이_ytb <- body %>% 
  html_nodes("a#author-text") %>%
  html_text()  

글쓴이_ytb <- gsub("\n","",글쓴이_ytb)
글쓴이_ytb <- gsub("@","",글쓴이_ytb)
글쓴이_ytb <- gsub(" ","",글쓴이_ytb)

댓글_ytb <- body %>% 
  html_nodes("yt-formatted-string#content-text") %>%
  html_text()  

날짜_ytb <-  body %>% 
  html_nodes("div#header-author") %>%
  html_nodes("yt-formatted-string.published-time-text") %>%
  html_nodes("a.yt-simple-endpoint") %>%
  html_text()

df_ytb <- tibble(글쓴이_ytb,제목_ytb,댓글_ytb,날짜_ytb)
df_ytb$line_number <- c(1:length(댓글_ytb))

df_ytb_com <- bind_rows(df_ytb_com,df_ytb)

cat(i,'번째 링크에서 페이지 정보 수집을 완료했습니다.\n')

Sys.sleep(time = 1)

}, error = function(e) cat("불러올 수 없습니다!\n"))

}

df_ytb_com %>% view()

end_time <- Sys.time()

end_time - start_time


write.xlsx(df_ytb_com, file = "D:/대학원/강의/2024-1 통계 프로그래밍 1/df_ytb_com.xlsx", rowNames=FALSE, fileEncoding = 'cp949')
