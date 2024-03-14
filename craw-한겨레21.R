library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)
library(tidyverse)

## 한겨레21-탈북자
## park
##검색결과 
body <- "http://search.hani.co.kr/Search?command=query&keyword=%ED%83%88%EB%B6%81%EC%9E%90&media=magazine&submedia=&sort=d&period=all&datefrom=2013.02.25&dateto=2017.03.10&pageseq=0" %>% read_html()

검색결과_han21 <- body %>% 
  html_nodes("div.search-title") %>%
  html_nodes("span") %>%
  html_text() %>% 
  str_match('([0-9]+)')

기사수_han21 <- 검색결과_han21[1] %>% as.integer()

n <- 기사수_han21 / 10
n <- n %>% ceiling()

# 키워드 탈북자
head <- "http://search.hani.co.kr/Search?command=query&keyword="
searchword <- "탈북자"
body1 <- "&media=magazine&submedia=&sort=d&period=all&datefrom="
from <- "2013.02.25"
body2 <- "&dateto="
to <- "2017.03.10"
body3 <- "&pageseq="
seq <- seq(from = 0, by = 1, length.out = n)

# 링크 수집
기사_park_han21 <- c()

for (i in 1:n){
  tryCatch({
    
    cat(i, '페이지 수집 중', '입니다.\n') 
    
    body <- paste0(head,searchword,body1,from,body2,to,body3,seq[i]) %>% read_html()
   
    #기사
    기사_탈북.tmp <- body %>% 
      html_nodes("ul.search-result-list") %>%
      html_nodes("a") %>%
      html_attr("href") # 링크등 해당 속성의 값을 추출함 
    
    if (length(기사_탈북.tmp) != 0) {
      기사_park_han21 <- append(기사_park_han21,기사_탈북.tmp)
    } else { 
      기사_park_han21 <- append(기사_park_han21,"수동확인")}    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

기사_park_han21 <- 기사_park_han21 %>% unique()
기사_park_han21 <- gsub("http:","",기사_park_han21)
기사_park_han21 <- gsub("//","http://",기사_park_han21)

## 최종 기사 링크 - 최신순 정렬
# 벡터 생성
제목_park_han21 <- c()
본문_park_han21 <- c()
날짜_park_han21 <- c()

for (i in 1:length(기사_park_han21)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- 기사_park_han21[i] %>% read_html()
    
    #제목
    제목_탈북.tmp1 <- body %>% 
      html_nodes("header.article_head") %>%
      html_nodes("h1") %>%
      html_text() # text 추출
    
    제목_탈북.tmp2 <- body %>% 
      html_nodes("header.article_head") %>%
      html_nodes("h3") %>%
      html_text() # text 추출
    
    제목_탈북.tmp <- paste0(제목_탈북.tmp1," ",제목_탈북.tmp2)
    
    if (length(제목_탈북.tmp) != 0) {
      제목_park_han21 <- append(제목_park_han21,제목_탈북.tmp)
    } else {
      제목_park_han21 <- append(제목_park_han21,"수동확인")
    }      
    
    #본문
    본문_탈북.tmp <- body %>%
      html_nodes("div.article_body") %>%
      html_text()
    
    if (length(본문_탈북.tmp) != 0) {
      본문_park_han21 <- append(본문_park_han21,본문_탈북.tmp)
    } else {
      본문_park_han21 <- append(본문_park_han21,"수동확인")
    }
    
    #날짜
    날짜_탈북.tmp <- body %>% 
      html_nodes("div.datebox") %>%
      html_text()

    날짜_탈북.tmp <- str_sub(날짜_탈북.tmp,7,22)
    
    if (length(날짜_탈북.tmp) != 0) {
      날짜_park_han21 <- append(날짜_park_han21,날짜_탈북.tmp)
    } else {
      날짜_park_han21 <- append(날짜_park_han21,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

탈북_park_han21 <- data.frame(제목_park_han21,
                               본문_park_han21,
                               날짜_park_han21,
                               기사_park_han21[1:length(기사_park_han21)-1])

names(탈북_park_han21) <- c("제목_탈북","본문_탈북","날짜_탈북","링크_탈북")
탈북_park_han21$언론사_탈북 <- c("한겨레21")

탈북_park_han21 <- 탈북_park_han21 %>% 
  select("날짜_탈북","언론사_탈북","제목_탈북","본문_탈북","링크_탈북")

write.csv(탈북_park_han21, file = "C:/대학원/탈북_park_han21.csv", row.names=FALSE)

탈북_park_han21_본문제외 <- 탈북_park_han21 %>% 
  select("날짜_탈북","언론사_탈북","제목_탈북","링크_탈북")

write.csv(탈북_park_han21_본문제외, file = "C:/대학원/탈북_park_han21_본문제외.csv", row.names=FALSE)


## 한겨레21-탈북자
## moon
##검색결과 
body <- "http://search.hani.co.kr/Search?command=query&keyword=%ED%83%88%EB%B6%81%EC%9E%90&media=magazine&submedia=&sort=d&period=all&datefrom=2017.05.10&dateto=2021.03.31&pageseq=0" %>% read_html()

검색결과_han21 <- body %>% 
  html_nodes("div.search-title") %>%
  html_nodes("span") %>%
  html_text() %>% 
  str_match('([0-9]+)')

기사수_han21 <- 검색결과_han21[1] %>% as.integer()

n <- 기사수_han21 / 10
n <- n %>% ceiling()

# 키워드 탈북자
head <- "http://search.hani.co.kr/Search?command=query&keyword="
searchword <- "탈북자"
body1 <- "&media=magazine&submedia=&sort=d&period=all&datefrom="
from <- "2017.05.10"
body2 <- "&dateto="
to <- "2021.03.31"
body3 <- "&pageseq="
seq <- seq(from = 0, by = 1, length.out = n)

# 링크 수집
기사_moon_han21 <- c()

for (i in 1:n){
  tryCatch({
    
    cat(i, '페이지 수집 중', '입니다.\n') 
    
    body <- paste0(head,searchword,body1,from,body2,to,body3,seq[i]) %>% read_html()
    
    #기사
    기사_탈북.tmp <- body %>% 
      html_nodes("ul.search-result-list") %>%
      html_nodes("a") %>%
      html_attr("href") # 링크등 해당 속성의 값을 추출함 
    
    if (length(기사_탈북.tmp) != 0) {
      기사_moon_han21 <- append(기사_moon_han21,기사_탈북.tmp)
    } else { 
      기사_moon_han21 <- append(기사_moon_han21,"수동확인")}    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

기사_moon_han21 <- 기사_moon_han21 %>% unique()
기사_moon_han21 <- gsub("http:","",기사_moon_han21)
기사_moon_han21 <- gsub("//","http://",기사_moon_han21)

## 최종 기사 링크 - 최신순 정렬
# 벡터 생성
제목_moon_han21 <- c()
본문_moon_han21 <- c()
날짜_moon_han21 <- c()

for (i in 1:length(기사_moon_han21)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- 기사_moon_han21[1] %>% read_html()
    
    #제목
    제목_탈북.tmp1 <- body %>% 
      html_nodes("header.article_head") %>%
      html_nodes("h1") %>%
      html_text() # text 추출
    
    제목_탈북.tmp2 <- body %>% 
      html_nodes("header.article_head") %>%
      html_nodes("h3") %>%
      html_text() # text 추출
    
    제목_탈북.tmp <- paste0(제목_탈북.tmp1," ",제목_탈북.tmp2)
    
    if (length(제목_탈북.tmp) != 0) {
      제목_moon_han21 <- append(제목_moon_han21,제목_탈북.tmp)
    } else {
      제목_moon_han21 <- append(제목_moon_han21,"수동확인")
    }      
    
    #본문
    본문_탈북.tmp <- body %>%
      html_nodes("div.article_body") %>%
      html_text()
    
    if (length(본문_탈북.tmp) != 0) {
      본문_moon_han21 <- append(본문_moon_han21,본문_탈북.tmp)
    } else {
      본문_moon_han21 <- append(본문_moon_han21,"수동확인")
    }
    
    #날짜
    날짜_탈북.tmp <- body %>% 
      html_nodes("div.datebox") %>%
      html_text()
    
    날짜_탈북.tmp <- str_sub(날짜_탈북.tmp,7,22)
    
    if (length(날짜_탈북.tmp) != 0) {
      날짜_moon_han21 <- append(날짜_moon_han21,날짜_탈북.tmp)
    } else {
      날짜_moon_han21 <- append(날짜_moon_han21,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

탈북_moon_han21 <- data.frame(제목_moon_han21,
                               본문_moon_han21,
                               날짜_moon_han21,
                               기사_moon_han21)

names(탈북_moon_han21) <- c("제목_탈북","본문_탈북","날짜_탈북","링크_탈북")
탈북_moon_han21$언론사_탈북 <- c("한겨레21")

탈북_moon_han21 <- 탈북_moon_han21 %>% 
  select("날짜_탈북","언론사_탈북","제목_탈북","본문_탈북","링크_탈북")

write.csv(탈북_moon_han21, file = "C:/대학원/탈북_moon_han21.csv", row.names=FALSE)

탈북_moon_han21_본문제외 <- 탈북_moon_han21 %>% 
  select("날짜_탈북","언론사_탈북","제목_탈북","링크_탈북")

write.csv(탈북_moon_han21_본문제외, file = "C:/대학원/탈북_moon_han21_본문제외.csv", row.names=FALSE)

## 한겨레(본지)는 동일한 기사가 두 군데 다른링크로 올려져 있어서 이를 전처리할 필요있음


