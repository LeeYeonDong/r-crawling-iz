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

#의료분쟁 뉴스, 의료사태 뉴스, 의대증원 뉴스 : 키워드
키워드_url <- c("https://www.youtube.com/results?search_query=%EC%9D%98%EB%A3%8C%EC%82%AC%ED%83%9C+%EB%89%B4%EC%8A%A4&sp=EgIIBQ%253D%253D", "https://www.youtube.com/results?search_query=%EC%9D%98%EB%A3%8C%EB%B6%84%EC%9F%81+%EB%89%B4%EC%8A%A4&sp=EgIIBQ%253D%253D",
  "https://www.youtube.com/results?search_query=%EC%9D%98%EB%8C%80+%EC%A6%9D%EC%9B%90+%EB%89%B4%EC%8A%A4&sp=EgIIBQ%253D%253D")

링크_ytb <- c()
제목_ytb <- c()
n=3

remDr$navigate(키워드_url[n])

times <- seq(from = 1, to = 3, by = 0.00001)

# Scroll down to load all comments
last_height <- (remDr$executeScript("return document.documentElement.scrollHeight"))[[1]][1]
while (TRUE) {
  remDr$executeScript("window.scrollTo(0, document.documentElement.scrollHeight);")
  Sys.sleep(time = sample(times,1))
  new_height <- (remDr$executeScript("return document.documentElement.scrollHeight"))[[1]][1]
  if (new_height == last_height) {
    break
  }
  last_height <- new_height
}

Sys.sleep(times)

frontpage <- remDr$getPageSource()[[1]]
body <- frontpage %>% read_html()

# 링크 수집
링크_ytb.tmp <- body %>%
  html_nodes("a#video-title") %>%
  html_attr("href")

링크_ytb.tmp <- paste0("https://www.youtube.com/",링크_ytb.tmp)

링크_ytb <- append(링크_ytb, 링크_ytb.tmp)

# 제목
제목_ytb.tmp <- body %>%
  html_nodes("a#video-title") %>%
  html_text()

제목_ytb <- append(제목_ytb, 제목_ytb.tmp)
Sys.sleep(times)

서치_페이지_tb <- tibble(제목_ytb,링크_ytb)

링크_ytb <- 서치_페이지_tb$링크_ytb

링크_ytb <- 링크_ytb %>% unique()

링크_ytb <- 링크_ytb[grep("watch", 링크_ytb)]



###
# library(readxl)
# df_ytb_com <- read.xlsx("D:/대학원/상담/커뮤니케이션학과/의료분쟁/df_ytb_com.xlsx")
# 
# 링크_ytb <- df_ytb_com %>% 
#   select(링크_ytb) %>% unlist() %>% as.vector()

링크_ytb <- 링크_ytb[grep("watch", 링크_ytb)]


# 수집할 데이터 프레임 만들기
## 다수 유튜브 동영상 댓글 수집
remDr <- remoteDriver(remoteServerAddr="localhost",
                      port=4445L,
                      browserName="chrome")
remDr$open()


df_ytb_com <- tibble()
times <- seq(from = 1, to = 3, by = 0.00001)

i = 1

# 수집한 링크를 navigate
for(i in 1:length(링크_ytb)){
  tryCatch({
    remDr$navigate(링크_ytb[i])
    Sys.sleep(3)
    pause <- remDr$findElement("css","button.ytp-play-button") 
    pause$clickElement()
    
    Sys.sleep(sample(times,1))
    
    webElem <- remDr$findElement("css", "body")
    webElem$sendKeysToElement(list(key = "end"))
    
    Sys.sleep(sample(times,1))
    
    # Scroll down to load all comments
    last_height <- (remDr$executeScript("return document.documentElement.scrollHeight"))[[1]][1]
    
    while (TRUE) {
      webElem <- remDr$findElement("css", "body")
      webElem$sendKeysToElement(list(key = "end"))
      Sys.sleep(sample(times,1))
      new_height <- (remDr$executeScript("return document.documentElement.scrollHeight"))[[1]][1]
      if (new_height == last_height) {
        cat(i,'번째 링크에서 페이지 스크롤 중 입니다.\n')
        break
      }
      last_height <- new_height
    }
    
    Sys.sleep(sample(times,1))
    
    # all replies button
    view_replies_buttons <- remDr$findElements(using = "css", "ytd-button-renderer.more-button")
    for (button in view_replies_buttons) {
      button$clickElement()
      Sys.sleep(sample(times,1))
    }
    
    # more button
    more_buttons <- remDr$findElements(using = "xpath", "//*[@id='button']/ytd-button-renderer")
    for (button in more_buttons) {
      button$clickElement()
      Sys.sleep(sample(times,1))
    }
    Sys.sleep(sample(times,1))

    
    ## 현재페이지 정보 읽어오기
    frontpage <- remDr$getPageSource()[[1]]
    body <- frontpage %>% read_html() 
    Sys.sleep(sample(times,1))
    
    제목_ytb <- body %>% 
      html_nodes("h1.title") %>%
      html_nodes("yt-formatted-string.style-scope") %>%
      html_text() 
    제목_ytb <- 제목_ytb[3]
    
    if(length(제목_ytb) == 0) {
      제목_ytb <- "없음"
    }
    Sys.sleep(sample(times,1))

    글쓴이_ytb <- body %>% 
      html_nodes("a#author-text") %>%
      html_text()  
    
    글쓴이_ytb <- gsub("\n","",글쓴이_ytb)
    글쓴이_ytb <- gsub("@","",글쓴이_ytb)
    글쓴이_ytb <- gsub(" ","",글쓴이_ytb)
    
    if(length(글쓴이_ytb) == 0) {
      글쓴이_ytb <- "없음"
    }
    Sys.sleep(sample(times,1))
    
    댓글_ytb <- body %>% 
      html_nodes(xpath = "//*[@id='content-text']/span") %>%
      html_text()  

    if(length(댓글_ytb) == 0) {
      댓글_ytb <- "없음"
    }
    Sys.sleep(sample(times,1))
    
    날짜_ytb <-  body %>% 
      html_nodes(xpath = "//*[@id='published-time-text']/a") %>%
      html_text()
    
    날짜_ytb <- gsub("\n","",날짜_ytb)
    날짜_ytb <- gsub("@","",날짜_ytb)
    날짜_ytb <- gsub(" ","",날짜_ytb)
    
    if(length(날짜_ytb) == 0) {
      날짜_ytb <- "없음"
    }

    cat(i,'번째 링크 정보 수집을 완료했습니다.\n')
    
    Sys.sleep(sample(times,1))
    
    df_ytb <- tibble(글쓴이_ytb,댓글_ytb,날짜_ytb)
    df_ytb$링크_ytb <- 링크_ytb[i]
    df_ytb$제목_ytb <- 제목_ytb
    
    Sys.sleep(sample(times,1))
    
    df_ytb_com <- bind_rows(df_ytb_com,df_ytb)
 
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
  Sys.sleep(10)
}

library(openxlsx)
write.xlsx(df_ytb_com, "D:/대학원/상담/커뮤니케이션학과/의료분쟁/df_ytb_com0605.xlsx")




           