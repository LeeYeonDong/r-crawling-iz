library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)
library(tidyverse)

## 조선일보-탈북자
#cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

### RSelenium 가동
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

# 키워드 탈북자
searchword <- "탈북자"

target <- paste0("https://www.chosun.com/nsearch/?query=",searchword,"&siteid=&sort=1&date_period=all&writer=&field=&emd_word=&expt_word=&opt_chk=false&app_check=0")

remDr$navigate(target)

body <- remDr$getPageSource()[[1]]
body <- body %>% read_html()

##검색결과 
검색결과_cho <- body %>% 
  html_nodes("div.flex") %>%
  html_nodes("p") %>%
  html_text() %>% 
  str_match('([0-9]+)')

기사수_cho <- 검색결과_cho[1] %>% as.integer()

# 더보기 클릭 후 링크 수집
n <- 기사수_cho / 10
n <- n %>% ceiling()

기사_cho <- c()

for(i in 1:n){
  tryCatch({
    
    cat(i, '번째 더 보기 클릭 후 페이지 수집 중 입니다.\n') 
    
    more_reply <- remDr$findElement("css", "button#load-more-stories")
    more_reply$clickElement()
    
    Sys.sleep(time = 5)
    
    body <- remDr$getPageSource()[[1]]
    body <- body %>% read_html()
    
    기사_cho.tmp <- body %>% 
      html_nodes("a.text__link") %>%
      html_attr("href")
    
    기사_cho <- append(기사_cho,기사_cho.tmp)
    
    if(i == n) break()
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}
# 기사_초 raw data
기사_cho <- 기사_초 

기사_cho %>% length()

기사_cho <- 기사_cho %>% unique()

기사_cho <- 기사_cho %>% as.data.frame()
names(기사_cho) <- c("링크_cho")

기사_cho %>% head()

# 기사 분류하기
기사_chosun.com <- grep("https://www.chosun.com/",기사_cho$링크_cho)
기사_biz <- grep("https://biz.chosun.com/",기사_cho$링크_cho)
기사_weekly <- grep("http://weekly.chosun.com/",기사_cho$링크_cho)

기사_chosun.com <- 기사_cho$링크_cho[기사_chosun.com]
기사_chosun.com <- 기사_chosun.com %>% unique()

기사_biz <- 기사_cho$링크_cho[기사_biz]
기사_biz <- 기사_biz %>% unique()

기사_weekly <- 기사_cho$링크_cho[기사_weekly]
기사_weekly <- 기사_weekly %>% unique()

length(기사_cho$링크_cho)
length(기사_chosun.com) + length(기사_biz) + length(기사_weekly)

# chosun.com 
날짜_chosun.com <- c()
제목_chosun.com <- c()
본문_chosun.com <- c()

for (i in 1:length(기사_chosun.com)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    remDr$navigate(기사_chosun.com[i])
    
    body <- remDr$getPageSource()[[1]]
    body <- body %>% read_html()
    
    #제목
    제목.tmp1 <- body %>% 
      html_nodes("div.article-header__headline-container") %>%
      html_nodes("h1") %>% 
      html_text()
    
    제목.tmp2 <- body %>% 
      html_nodes("div.article-header__headline-container") %>%
      html_nodes("h3") %>% 
      html_text()
    
    제목.tmp <- paste0(제목.tmp1," ",제목.tmp2)
  
    if (length(제목.tmp) != 0) {
      제목_chosun.com <- append(제목_chosun.com,제목.tmp)
    } else {
      제목_chosun.com <- append(제목_chosun.com,"수동확인")
    }    
    
    #날짜
    날짜.tmp <- body %>% 
      html_nodes("span.font--size-sm-14") %>%
      html_text()
    
    날짜.tmp <- 날짜.tmp[1]
    날짜.tmp <- substr(날짜.tmp,4,19)
      
    if (length(날짜.tmp) != 0) {
      날짜_chosun.com <- append(날짜_chosun.com,날짜.tmp)
    } else {
      날짜_chosun.com <- append(날짜_chosun.com,"수동확인")
    }    
    
    
    #본문
    본문.tmp <- body %>% 
      html_nodes("section.article-body") %>%
      html_text()

    if (length(본문.tmp) != 0) {
      본문_chosun.com <- append(본문_chosun.com,본문.tmp)
    } else {
      본문_chosun.com <- append(본문_chosun.com,"수동확인")
    }    
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

탈북_chosun.com <- data.frame(제목_chosun.com,
                           본문_chosun.com,
                           날짜_chosun.com,
                           기사_chosun.com)

names(탈북_chosun.com) <- c("제목_탈북","본문_탈북","날짜_탈북","링크_탈북")
탈북_chosun.com$언론사_탈북 <- c("조선.com")


# biz 
날짜_biz <- c()
제목_biz <- c()
본문_biz <- c()

for (i in 1:length(기사_biz)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    remDr$navigate(기사_biz[i])
    
    body <- remDr$getPageSource()[[1]]
    body <- body %>% read_html()
    
    #제목
    제목.tmp1 <- body %>% 
      html_nodes("h1#news_title_text_id") %>%
      html_text()
    
    제목.tmp2 <- body %>% 
      html_nodes("div.par") %>%
      html_text()
    
    제목.tmp <- append(제목.tmp1,제목.tmp2[1])
    
    if (length(제목.tmp1) != 0) {
      제목_biz <- append(제목_biz,제목.tmp)
    } else {
      제목_biz <- append(제목_biz,"수동확인")
    }    
    
    #날짜
    날짜.tmp <- body %>% 
      html_nodes("div.news_date") %>%
      html_text()
    
    날짜.tmp <- 날짜.tmp[1]
    날짜.tmp <- substr(날짜.tmp,4,19)
    
    if (length(날짜.tmp) != 0) {
      날짜_biz <- append(날짜_biz,날짜.tmp)
    } else {
      날짜_biz <- append(날짜_biz,"수동확인")
    }    
    
    #본문
    본문.tmp <- 제목.tmp2[2]
    
    if (length(본문.tmp) != 0) {
      본문_biz <- append(본문_biz,본문.tmp)
    } else {
      본문_biz <- append(본문_biz,"수동확인")
    }    
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

탈북_biz <- data.frame(제목_biz,
                       본문_biz,
                       날짜_biz,
                       기사_biz)

names(탈북_biz) <- c("제목_탈북","본문_탈북","날짜_탈북","링크_탈북")
탈북_biz$언론사_탈북 <- c("조선biz")


# weekly 
날짜_weekly <- c()
제목_weekly <- c()
본문_weekly <- c()

for (i in 1:length(기사_weekly)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    remDr$navigate(기사_weekly[1])
    
    body <- remDr$getPageSource()[[1]]
    body <- body %>% read_html()
    
    #제목
    제목.tmp <- body %>% 
      html_nodes("h2.txt") %>%
      html_text()
    
    제목.tmp <- gsub("커버스토리","",제목.tmp)
    
    if (length(제목.tmp) != 0) {
      제목_weekly <- append(제목_weekly,제목.tmp)
    } else {
      제목_weekly <- append(제목_weekly,"수동확인")
    }    
    
    #날짜
    날짜.tmp <- body %>% 
      html_nodes("div.edition") %>%
      html_text()

    날짜.tmp <- substr(날짜.tmp,9,18)
    
    if (length(날짜.tmp) != 0) {
      날짜_weekly <- append(날짜_weekly,날짜.tmp)
    } else {
      날짜_weekly <- append(날짜_weekly,"수동확인")
    }    
    
    #본문
    본문.tmp <- body %>% 
      html_nodes("div#articleBody") %>%
      html_text()
    
    if (length(본문.tmp) != 0) {
      본문_weekly <- append(본문_weekly,본문.tmp)
    } else {
      본문_weekly <- append(본문_weekly,"수동확인")
    }    
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

탈북_weekly <- data.frame(제목_weekly,
                        본문_weekly,
                        날짜_weekly,
                        기사_weekly)

names(탈북_weekly) <- c("제목_탈북","본문_탈북","날짜_탈북","링크_탈북")
탈북_weekly$언론사_탈북 <- c("조선weekly")

탈북_cho_com <- rbind(탈북_chosun.com,탈북_biz,탈북_weekly)
탈북_cho_com <- 탈북_cho_com %>% filter(탈북_cho_com$제목_탈북 != "수동확인")

write.csv(탈북_cho_com, file = "C:/대학원/탈북_cho_com.csv", row.names=FALSE)

#본문 제외
탈북_cho_com_본문제외 <- 탈북_cho_com %>% select("날짜_탈북","언론사_탈북","제목_탈북","링크_탈북")
write.csv(탈북_cho_com_본문제외, file = "C:/대학원/탈북_cho_com_본문제외.csv", row.names=FALSE)
