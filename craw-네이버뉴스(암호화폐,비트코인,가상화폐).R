library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)
library(tidyverse)

## 링크생성
ymd1220_dd <- c()

year1220_dd <- c("2012","2013","2014","2015","2016","2017","2018","2019","2020")
mon1220_dd <- c("01","02","03","04","05","06","07","08","09","10","11","12")
day1220_dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

for (i in 1:length(year1220_dd)){
  for (j in 1:length(mon1220_dd)){
    for (k in 1:length(day1220_dd)){
      ymd1220_dd.tmp <- paste0(year1220_dd[i],".",mon1220_dd[j],".",day1220_dd[k])
      ymd1220_dd <- append(ymd1220_dd,ymd1220_dd.tmp)
    }
  }
}

ymd21_dd <- c()

year21_dd <- c("2021")
mon21_dd <- c("01","02","03","04","05")
day21_dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

for (i in 1:length(year21_dd)){
  for (j in 1:length(mon21_dd)){
    for (k in 1:length(day21_dd)){
      ymd21_dd.tmp <- paste0(year21_dd[i],".",mon21_dd[j],".",day21_dd[k])
      ymd21_dd <- append(ymd21_dd,ymd21_dd.tmp)
    }
  }
}

코인_dd <- c()

코인_dd <- append(코인_dd,ymd1220_dd)
코인_dd <- append(코인_dd,ymd21_dd)

# from
코인_from <- gsub("\\.","",코인_dd)



## 암호화폐
# 링크에 dd from 붙이기

start_time_암호화폐 <- Sys.time()

head <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query="
searchword <- "암호화폐"
body1 <- "&sort=1&photo=0&field=0&pd=3&ds="
코인_dd  <- 코인_dd 
body2 <- "&de="
코인_dd  <- 코인_dd 
body3 <- "&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:dd,p:from"
코인_from <- 코인_from 
to <- "to"
코인_from <- 코인_from 
body4 <- ",a:all&start="
seq <- seq(from = 1, by = 10, length.out = 400)


## 네이버뉴스 링크 수집
# 기사링크 벡터 생성
기사_암호 <- c()

# 네이버뉴스 링크 수집
for (i in 1:length(암호_dd)){
  for (j in 1:400){
    tryCatch({
      
      cat(암호_dd[i], j, '페이지 수집 중', '입니다.\n') 
      
      body <- paste0(head,searchword,body1,암호_dd[i],body2,암호_dd[i],body3,암호_from[i],to,암호_from[i],body4,seq[j]) %>% read_html()
      
      #기사
      기사_암호.tmp <- body %>% 
        html_nodes("div.info_group") %>%
        html_nodes("a:nth-child(3)") %>%
        html_attr("href") # 링크등 해당 속성의 값을 추출함 
      
      if (length(기사_암호.tmp) != 0) {
        기사_암호 <- append(기사_암호,기사_암호.tmp)
      } else { break }    
      
      Sys.sleep(time = 0.01)
      
    }, error = function(e) cat("불러올 수 없습니다!\n"))
  }
}

기사_암호 <- 기사_암호%>% unique()
기사_암호 %>% length()

news.naver_기사_암호 <- grep("https://news.naver.com",기사_암호)
기사_암호_news <- 기사_암호[news.naver_기사_암호] # 최종 기사 링크
기사_암호_etc1 <- 기사_암호[-news.naver_기사_암호] ## sports인지 확인

## 최종 기사 링크 - 최신순 정렬
# 벡터 생성
언론사_암호 <- c()
제목_암호 <- c()
본문_암호 <- c()
날짜_암호 <- c()

for (i in 1:length(기사_암호_news)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- 기사_암호_news[i] %>% read_html()
    
    #언론사
    언론사_암호.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_암호.tmp) != 0) {
      언론사_암호 <- append(언론사_암호,언론사_암호.tmp)
    } else {
      언론사_암호 <- append(언론사_암호,"수동확인")
    }    
    
    #제목
    제목_암호.tmp <- body %>% 
      html_nodes("div.article_info") %>%
      html_nodes("h3") %>%
      html_text() # text 추출
    
    if (length(제목_암호.tmp) != 0) {
      제목_암호 <- append(제목_암호,제목_암호.tmp)
    } else {
      제목_암호 <- append(제목_암호,"수동확인")
    }      
    
    #본문
    본문_암호.tmp <- body %>%
      html_nodes("div#articleBodyContents") %>%
      html_text()
    
    if (length(본문_암호.tmp) != 0) {
      본문_암호 <- append(본문_암호,본문_암호.tmp)
    } else {
      본문_암호 <- append(본문_암호,"수동확인")
    }
    
    #날짜
    날짜_암호.tmp <- body %>% 
      html_nodes("span.t11") %>%
      html_text()
    
    날짜_암호.tmp <- 날짜_암호.tmp[1]
    
    if (length(날짜_암호.tmp) != 0) {
      날짜_암호 <- append(날짜_암호,날짜_암호.tmp)
    } else {
      날짜_암호 <- append(날짜_암호,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_암호 <- gsub("포토","",제목_암호)
제목_암호 <- gsub("\u5317","북한",제목_암호)
제목_암호 <- gsub("\u97D3","대한민국",제목_암호)
제목_암호 <- gsub("\u65E5","일본",제목_암호)
제목_암호 <- gsub("\u7F8E","미국",제목_암호)
제목_암호 <- gsub("\u4F5B","프랑스",제목_암호)
제목_암호 <- gsub("\u82F1","영국",제목_암호)

암호_df <- data.frame(언론사_암호,
                          제목_암호,
                          본문_암호,
                          날짜_암호,
                          기사_암호_news)

names(암호_df) <- c("언론사_암호","제목_암호","본문_암호","날짜_암호","링크_암호")


### sports.news 기사
## 페이지 정보 읽기 - 최신순 정렬
# 벡터 생성
언론사_암호_etc1 <- c()
제목_암호_etc1 <- c()
본문_암호_etc1 <- c()
날짜_암호_etc1 <- c()

for (i in 1:length(기사_암호_etc1)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- 기사_암호_etc1[i] %>% read_html()
    
    #언론사
    언론사_암호.tmp <- body %>% 
      html_nodes("a.link") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_암호.tmp) != 0) {
      언론사_암호_etc1 <- append(언론사_암호_etc1,언론사_암호.tmp)
    } else {
      언론사_암호_etc1 <- append(언론사_암호_etc1,"수동확인")
    }    
    
    #제목
    제목_암호.tmp <- body %>% 
      html_nodes("h4.title") %>%
      html_text()
    
    if (length(제목_암호.tmp) != 0) {
      제목_암호_etc1 <- append(제목_암호_etc1,제목_암호.tmp)
    } else {
      제목_암호_etc1 <- append(제목_암호_etc1,"수동확인")
    }    
    
    #본문
    본문_암호.tmp <- body %>%
      html_nodes("div#newsEndContents") %>%
      html_text()
    
    if (length(본문_암호.tmp) != 0) {
      본문_암호_etc1 <- append(본문_암호_etc1,본문_암호.tmp)
    } else {
      본문_암호_etc1 <- append(본문_암호_etc1,"수동확인")
    }
    
    #날짜
    날짜_암호.tmp <- body %>% 
      html_nodes("div.info") %>%
      html_nodes("span") %>%
      html_text()
    
    날짜_암호.tmp <- 날짜_암호.tmp[1]
    날짜_암호.tmp <- gsub("기사입력","",날짜_암호.tmp)
    
    if (length(날짜_암호.tmp) != 0) {
      날짜_암호_etc1 <- append(날짜_암호_etc1,날짜_암호.tmp)
    } else {
      날짜_암호_etc1 <- append(날짜_암호_etc1,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_암호_etc1 <- gsub("포토","",제목_암호_etc1)
제목_암호_etc1 <- gsub("\u5317","북한",제목_암호_etc1)
제목_암호_etc1 <- gsub("\u97D3","대한민국",제목_암호_etc1)
제목_암호_etc1 <- gsub("\u65E5","일본",제목_암호_etc1)
제목_암호_etc1 <- gsub("\u7F8E","미국",제목_암호_etc1)
제목_암호_etc1 <- gsub("\u4F5B","프랑스",제목_암호_etc1)
제목_암호_etc1 <- gsub("\u82F1","영국",제목_암호_etc1)

암호_df_etc1 <- data.frame(언론사_암호_etc1,
                                제목_암호_etc1,
                                본문_암호_etc1,
                                날짜_암호_etc1,
                                기사_암호_etc1)

names(암호_df_etc1) <- c("언론사_암호","제목_암호","본문_암호","날짜_암호","링크_암호")


## news.naver가 아닌 기사
기사_암호_etc2 <- 암호_df %>% 
  filter(제목_암호 == "수동확인") %>% 
  select("링크_암호")

기사_암호_etc2 <- 기사_암호_etc2$링크_암호


# 벡터 생성
언론사_암호_etc2 <- c()
제목_암호_etc2 <- c()
본문_암호_etc2 <- c()
날짜_암호_etc2 <- c()

for (i in 1:length(기사_암호_etc2)){
  tryCatch({
    
    cat(i, '페이지 수집 중입니다.\n') 
    
    body <- 기사_암호_etc2[i] %>% read_html()
    
    #언론사
    언론사_암호.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_암호.tmp) != 0) {
      언론사_암호_etc2 <- append(언론사_암호_etc2,언론사_암호.tmp)
    } else {
      언론사_암호_etc2 <- append(언론사_암호_etc2,"수동확인")
    }    
    
    #제목
    제목_암호.tmp <- body %>% 
      html_nodes("h2.end_tit") %>% 
      html_text() # text 추출
    
    if (length(제목_암호.tmp) != 0) {
      제목_암호_etc2 <- append(제목_암호_etc2,제목_암호.tmp)
    } else {
      제목_암호_etc2 <- append(제목_암호_etc2,"수동확인")
    }      
    
    #본문
    본문_암호.tmp <- body %>%
      html_nodes("div#articeBody") %>%
      html_text()
    
    if (length(본문_암호.tmp) != 0) {
      본문_암호_etc2 <- append(본문_암호_etc2,본문_암호.tmp)
    } else {
      본문_암호_etc2<- append(본문_암호_etc2,"수동확인")
    }
    
    #날짜
    날짜_암호.tmp <- body %>% 
      html_nodes("span.author") %>%
      html_nodes("em") %>%
      html_text()
    
    날짜_암호.tmp <- 날짜_암호.tmp[1]
    
    if (length(날짜_암호.tmp) != 0) {
      날짜_암호_etc2 <- append(날짜_암호_etc2,날짜_암호.tmp)
    } else {
      날짜_암호_etc2 <- append(날짜_암호_etc2,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_암호_etc2 <- gsub("포토","",제목_암호_etc2)
제목_암호_etc2 <- gsub("\u5317","북한",제목_암호_etc2)
제목_암호_etc2 <- gsub("\u97D3","대한민국",제목_암호_etc2)
제목_암호_etc2 <- gsub("\u65E5","일본",제목_암호_etc2)
제목_암호_etc2 <- gsub("\u7F8E","미국",제목_암호_etc2)
제목_암호_etc2 <- gsub("\u4F5B","프랑스",제목_암호_etc2)
제목_암호_etc2 <- gsub("\u82F1","영국",제목_암호_etc2)

암호_df_etc2 <- data.frame(언론사_암호_etc2,
                                제목_암호_etc2,
                                본문_암호_etc2,
                                날짜_암호_etc2,
                                기사_암호_etc2)

names(암호_df_etc2) <- c("언론사_암호","제목_암호","본문_암호","날짜_암호","링크_암호")

암호_df_com <- rbind(암호_df,암호_df_etc1,암호_df_etc2)
암호_df_com <- 암호_df_com %>% filter(암호_df_com$제목_암호 != "수동확인")

write.csv(암호_df_com, file = "C:/대학원/암호_df_com.csv", row.names=FALSE)

#본문 제외
암호_df_com_본문제외 <- 암호_df_com %>% select("언론사_암호","제목_암호","날짜_암호","링크_암호")
write.csv(암호_df_com_본문제외, file = "C:/대학원/암호_df_com_본문제외.csv", row.names=FALSE)

end_time_암호화폐 <- Sys.time()



## 비트코인
# 링크에 dd from 붙이기

start_time_비트코인 <- Sys.time()

head <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query="
searchword <- "비트코인"
body1 <- "&sort=1&photo=0&field=0&pd=3&ds="
코인_dd  <- 코인_dd 
body2 <- "&de="
코인_dd  <- 코인_dd 
body3 <- "&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:dd,p:from"
코인_from <- 코인_from 
to <- "to"
코인_from <- 코인_from 
body4 <- ",a:all&start="
seq <- seq(from = 1, by = 10, length.out = 400)


## 네이버뉴스 링크 수집
# 기사링크 벡터 생성
기사_비트 <- c()

# 네이버뉴스 링크 수집
for (i in 1:length(코인_dd)){
  for (j in 1:400){
    tryCatch({
      
      cat(코인_dd[i], j, '페이지 수집 중', '입니다.\n') 
      
      body <- paste0(head,searchword,body1,코인_dd[i],body2,코인_dd[i],body3,코인_from[i],to,코인_from[i],body4,seq[j]) %>% read_html()
      
      #기사
      기사_비트.tmp <- body %>% 
        html_nodes("div.info_group") %>%
        html_nodes("a:nth-child(3)") %>%
        html_attr("href") # 링크등 해당 속성의 값을 추출함 
      
      if (length(기사_비트.tmp) != 0) {
        기사_비트 <- append(기사_비트,기사_비트.tmp)
      } else { break }    
      
      Sys.sleep(time = 0.01)
      
    }, error = function(e) cat("불러올 수 없습니다!\n"))
  }
}

기사_비트 <- 기사_비트%>% unique()
기사_비트 %>% length()

news.naver_기사_비트 <- grep("https://news.naver.com",기사_비트)
기사_비트_news <- 기사_비트[news.naver_기사_비트] # 최종 기사 링크
기사_비트_etc1 <- 기사_비트[-news.naver_기사_비트] ## sports인지 확인

## 최종 기사 링크 - 최신순 정렬
# 벡터 생성
언론사_비트 <- c()
제목_비트 <- c()
본문_비트 <- c()
날짜_비트 <- c()

for (i in 1:length(기사_비트_news)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- 기사_비트_news[i] %>% read_html()
    
    #언론사
    언론사_비트.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_비트.tmp) != 0) {
      언론사_비트 <- append(언론사_비트,언론사_비트.tmp)
    } else {
      언론사_비트 <- append(언론사_비트,"수동확인")
    }    
    
    #제목
    제목_비트.tmp <- body %>% 
      html_nodes("div.article_info") %>%
      html_nodes("h3") %>%
      html_text() # text 추출
    
    if (length(제목_비트.tmp) != 0) {
      제목_비트 <- append(제목_비트,제목_비트.tmp)
    } else {
      제목_비트 <- append(제목_비트,"수동확인")
    }      
    
    #본문
    본문_비트.tmp <- body %>%
      html_nodes("div#articleBodyContents") %>%
      html_text()
    
    if (length(본문_비트.tmp) != 0) {
      본문_비트 <- append(본문_비트,본문_비트.tmp)
    } else {
      본문_비트 <- append(본문_비트,"수동확인")
    }
    
    #날짜
    날짜_비트.tmp <- body %>% 
      html_nodes("span.t11") %>%
      html_text()
    
    날짜_비트.tmp <- 날짜_비트.tmp[1]
    
    if (length(날짜_비트.tmp) != 0) {
      날짜_비트 <- append(날짜_비트,날짜_비트.tmp)
    } else {
      날짜_비트 <- append(날짜_비트,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_비트 <- gsub("포토","",제목_비트)
제목_비트 <- gsub("\u5317","북한",제목_비트)
제목_비트 <- gsub("\u97D3","대한민국",제목_비트)
제목_비트 <- gsub("\u65E5","일본",제목_비트)
제목_비트 <- gsub("\u7F8E","미국",제목_비트)
제목_비트 <- gsub("\u4F5B","프랑스",제목_비트)
제목_비트 <- gsub("\u82F1","영국",제목_비트)

비트_df <- data.frame(언론사_비트,
                          제목_비트,
                          본문_비트,
                          날짜_비트,
                          기사_비트_news)

names(비트_df) <- c("언론사_비트","제목_비트","본문_비트","날짜_비트","링크_비트")


### sports.news 기사
## 페이지 정보 읽기 - 최신순 정렬
# 벡터 생성
언론사_비트_etc1 <- c()
제목_비트_etc1 <- c()
본문_비트_etc1 <- c()
날짜_비트_etc1 <- c()

for (i in 1:length(기사_비트_etc1)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- 기사_비트_etc1[i] %>% read_html()
    
    #언론사
    언론사_비트.tmp <- body %>% 
      html_nodes("a.link") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_비트.tmp) != 0) {
      언론사_비트_etc1 <- append(언론사_비트_etc1,언론사_비트.tmp)
    } else {
      언론사_비트_etc1 <- append(언론사_비트_etc1,"수동확인")
    }    
    
    #제목
    제목_비트.tmp <- body %>% 
      html_nodes("h4.title") %>%
      html_text()
    
    if (length(제목_비트.tmp) != 0) {
      제목_비트_etc1 <- append(제목_비트_etc1,제목_비트.tmp)
    } else {
      제목_비트_etc1 <- append(제목_비트_etc1,"수동확인")
    }    
    
    #본문
    본문_비트.tmp <- body %>%
      html_nodes("div#newsEndContents") %>%
      html_text()
    
    if (length(본문_비트.tmp) != 0) {
      본문_비트_etc1 <- append(본문_비트_etc1,본문_비트.tmp)
    } else {
      본문_비트_etc1 <- append(본문_비트_etc1,"수동확인")
    }
    
    #날짜
    날짜_비트.tmp <- body %>% 
      html_nodes("div.info") %>%
      html_nodes("span") %>%
      html_text()
    
    날짜_비트.tmp <- 날짜_비트.tmp[1]
    날짜_비트.tmp <- gsub("기사입력","",날짜_비트.tmp)
    
    if (length(날짜_비트.tmp) != 0) {
      날짜_비트_etc1 <- append(날짜_비트_etc1,날짜_비트.tmp)
    } else {
      날짜_비트_etc1 <- append(날짜_비트_etc1,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_비트_etc1 <- gsub("포토","",제목_비트_etc1)
제목_비트_etc1 <- gsub("\u5317","북한",제목_비트_etc1)
제목_비트_etc1 <- gsub("\u97D3","대한민국",제목_비트_etc1)
제목_비트_etc1 <- gsub("\u65E5","일본",제목_비트_etc1)
제목_비트_etc1 <- gsub("\u7F8E","미국",제목_비트_etc1)
제목_비트_etc1 <- gsub("\u4F5B","프랑스",제목_비트_etc1)
제목_비트_etc1 <- gsub("\u82F1","영국",제목_비트_etc1)

비트_df_etc1 <- data.frame(언론사_비트_etc1,
                                제목_비트_etc1,
                                본문_비트_etc1,
                                날짜_비트_etc1,
                                기사_비트_etc1)

names(비트_df_etc1) <- c("언론사_비트","제목_비트","본문_비트","날짜_비트","링크_비트")


## news.naver가 아닌 기사
기사_비트_etc2 <- 비트_df %>% 
  filter(제목_비트 == "수동확인") %>% 
  select("링크_비트")

기사_비트_etc2 <- 기사_비트_etc2$링크_비트


# 벡터 생성
언론사_비트_etc2 <- c()
제목_비트_etc2 <- c()
본문_비트_etc2 <- c()
날짜_비트_etc2 <- c()

for (i in 1:length(기사_비트_etc2)){
  tryCatch({
    
    cat(i, '페이지 수집 중입니다.\n') 
    
    body <- 기사_비트_etc2[i] %>% read_html()
    
    #언론사
    언론사_비트.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_비트.tmp) != 0) {
      언론사_비트_etc2 <- append(언론사_비트_etc2,언론사_비트.tmp)
    } else {
      언론사_비트_etc2 <- append(언론사_비트_etc2,"수동확인")
    }    
    
    #제목
    제목_비트.tmp <- body %>% 
      html_nodes("h2.end_tit") %>% 
      html_text() # text 추출
    
    if (length(제목_비트.tmp) != 0) {
      제목_비트_etc2 <- append(제목_비트_etc2,제목_비트.tmp)
    } else {
      제목_비트_etc2 <- append(제목_비트_etc2,"수동확인")
    }      
    
    #본문
    본문_비트.tmp <- body %>%
      html_nodes("div#articeBody") %>%
      html_text()
    
    if (length(본문_비트.tmp) != 0) {
      본문_비트_etc2 <- append(본문_비트_etc2,본문_비트.tmp)
    } else {
      본문_비트_etc2<- append(본문_비트_etc2,"수동확인")
    }
    
    #날짜
    날짜_비트.tmp <- body %>% 
      html_nodes("span.author") %>%
      html_nodes("em") %>%
      html_text()
    
    날짜_비트.tmp <- 날짜_비트.tmp[1]
    
    if (length(날짜_비트.tmp) != 0) {
      날짜_비트_etc2 <- append(날짜_비트_etc2,날짜_비트.tmp)
    } else {
      날짜_비트_etc2 <- append(날짜_비트_etc2,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_비트_etc2 <- gsub("포토","",제목_비트_etc2)
제목_비트_etc2 <- gsub("\u5317","북한",제목_비트_etc2)
제목_비트_etc2 <- gsub("\u97D3","대한민국",제목_비트_etc2)
제목_비트_etc2 <- gsub("\u65E5","일본",제목_비트_etc2)
제목_비트_etc2 <- gsub("\u7F8E","미국",제목_비트_etc2)
제목_비트_etc2 <- gsub("\u4F5B","프랑스",제목_비트_etc2)
제목_비트_etc2 <- gsub("\u82F1","영국",제목_비트_etc2)

비트_df_etc2 <- data.frame(언론사_비트_etc2,
                                제목_비트_etc2,
                                본문_비트_etc2,
                                날짜_비트_etc2,
                                기사_비트_etc2)

names(비트_df_etc2) <- c("언론사_비트","제목_비트","본문_비트","날짜_비트","링크_비트")

비트_df_com <- rbind(비트_df,비트_df_etc1,비트_df_etc2)
비트_df_com <- 비트_df_com %>% filter(비트_df_com$제목_비트 != "수동확인")

write.csv(비트_df_com, file = "C:/대학원/비트_df_com.csv", row.names=FALSE)

#본문 제외
비트_df_com_본문제외 <- 비트_df_com %>% select("언론사_비트","제목_비트","날짜_비트","링크_비트")
write.csv(비트_df_com_본문제외, file = "C:/대학원/비트_df_com_본문제외.csv", row.names=FALSE)

end_time_비트코인 <- Sys.time()


## 가상화폐
# 링크에 dd from 붙이기

start_time_가상화폐 <- Sys.time()

head <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query="
searchword <- "가상화폐"
body1 <- "&sort=1&photo=0&field=0&pd=3&ds="
코인_dd  <- 코인_dd 
body2 <- "&de="
코인_dd  <- 코인_dd 
body3 <- "&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:dd,p:from"
코인_from <- 코인_from 
to <- "to"
코인_from <- 코인_from 
body4 <- ",a:all&start="
seq <- seq(from = 1, by = 10, length.out = 400)


## 네이버뉴스 링크 수집
# 기사링크 벡터 생성
기사_가상 <- c()

# 네이버뉴스 링크 수집
for (i in 1:length(코인_dd)){
  for (j in 1:400){
    tryCatch({
      
      cat(코인_dd[i], j, '페이지 수집 중', '입니다.\n') 
      
      body <- paste0(head,searchword,body1,코인_dd[i],body2,코인_dd[i],body3,코인_from[i],to,코인_from[i],body4,seq[j]) %>% read_html()
      
      #기사
      기사_가상.tmp <- body %>% 
        html_nodes("div.info_group") %>%
        html_nodes("a:nth-child(3)") %>%
        html_attr("href") # 링크등 해당 속성의 값을 추출함 
      
      if (length(기사_가상.tmp) != 0) {
        기사_가상 <- append(기사_가상,기사_가상.tmp)
      } else { break }    
      
      Sys.sleep(time = 0.01)
      
    }, error = function(e) cat("불러올 수 없습니다!\n"))
  }
}

기사_가상 <- 기사_가상%>% unique()
기사_가상 %>% length()

news.naver_기사_가상 <- grep("https://news.naver.com",기사_가상)
기사_가상_news <- 기사_가상[news.naver_기사_가상] # 최종 기사 링크
기사_가상_etc1 <- 기사_가상[-news.naver_기사_가상] ## sports인지 확인

## 최종 기사 링크 - 최신순 정렬
# 벡터 생성
언론사_가상 <- c()
제목_가상 <- c()
본문_가상 <- c()
날짜_가상 <- c()

for (i in 1:length(기사_가상_news)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- 기사_가상_news[i] %>% read_html()
    
    #언론사
    언론사_가상.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_가상.tmp) != 0) {
      언론사_가상 <- append(언론사_가상,언론사_가상.tmp)
    } else {
      언론사_가상 <- append(언론사_가상,"수동확인")
    }    
    
    #제목
    제목_가상.tmp <- body %>% 
      html_nodes("div.article_info") %>%
      html_nodes("h3") %>%
      html_text() # text 추출
    
    if (length(제목_가상.tmp) != 0) {
      제목_가상 <- append(제목_가상,제목_가상.tmp)
    } else {
      제목_가상 <- append(제목_가상,"수동확인")
    }      
    
    #본문
    본문_가상.tmp <- body %>%
      html_nodes("div#articleBodyContents") %>%
      html_text()
    
    if (length(본문_가상.tmp) != 0) {
      본문_가상 <- append(본문_가상,본문_가상.tmp)
    } else {
      본문_가상 <- append(본문_가상,"수동확인")
    }
    
    #날짜
    날짜_가상.tmp <- body %>% 
      html_nodes("span.t11") %>%
      html_text()
    
    날짜_가상.tmp <- 날짜_가상.tmp[1]
    
    if (length(날짜_가상.tmp) != 0) {
      날짜_가상 <- append(날짜_가상,날짜_가상.tmp)
    } else {
      날짜_가상 <- append(날짜_가상,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_가상 <- gsub("포토","",제목_가상)
제목_가상 <- gsub("\u5317","북한",제목_가상)
제목_가상 <- gsub("\u97D3","대한민국",제목_가상)
제목_가상 <- gsub("\u65E5","일본",제목_가상)
제목_가상 <- gsub("\u7F8E","미국",제목_가상)
제목_가상 <- gsub("\u4F5B","프랑스",제목_가상)
제목_가상 <- gsub("\u82F1","영국",제목_가상)
제목_가상 <- gsub("\u4e2d","중국",제목_가상)

가상_df <- data.frame(언론사_가상,
                          제목_가상,
                          본문_가상,
                          날짜_가상,
                          기사_가상_news)

names(가상_df) <- c("언론사_가상","제목_가상","본문_가상","날짜_가상","링크_가상")

언론사_가상 %>% length()
  제목_가상 %>% length()
  본문_가상%>% length()
  날짜_가상%>% length()
  기사_가상_news %>% length()

### sports.news 기사
## 페이지 정보 읽기 - 최신순 정렬
# 벡터 생성
언론사_가상_etc1 <- c()
제목_가상_etc1 <- c()
본문_가상_etc1 <- c()
날짜_가상_etc1 <- c()

for (i in 1:length(기사_가상_etc1)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- 기사_가상_etc1[i] %>% read_html()
    
    #언론사
    언론사_가상.tmp <- body %>% 
      html_nodes("a.link") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_가상.tmp) != 0) {
      언론사_가상_etc1 <- append(언론사_가상_etc1,언론사_가상.tmp)
    } else {
      언론사_가상_etc1 <- append(언론사_가상_etc1,"수동확인")
    }    
    
    #제목
    제목_가상.tmp <- body %>% 
      html_nodes("h4.title") %>%
      html_text()
    
    if (length(제목_가상.tmp) != 0) {
      제목_가상_etc1 <- append(제목_가상_etc1,제목_가상.tmp)
    } else {
      제목_가상_etc1 <- append(제목_가상_etc1,"수동확인")
    }    
    
    #본문
    본문_가상.tmp <- body %>%
      html_nodes("div#newsEndContents") %>%
      html_text()
    
    if (length(본문_가상.tmp) != 0) {
      본문_가상_etc1 <- append(본문_가상_etc1,본문_가상.tmp)
    } else {
      본문_가상_etc1 <- append(본문_가상_etc1,"수동확인")
    }
    
    #날짜
    날짜_가상.tmp <- body %>% 
      html_nodes("div.info") %>%
      html_nodes("span") %>%
      html_text()
    
    날짜_가상.tmp <- 날짜_가상.tmp[1]
    날짜_가상.tmp <- gsub("기사입력","",날짜_가상.tmp)
    
    if (length(날짜_가상.tmp) != 0) {
      날짜_가상_etc1 <- append(날짜_가상_etc1,날짜_가상.tmp)
    } else {
      날짜_가상_etc1 <- append(날짜_가상_etc1,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_가상_etc1 <- gsub("포토","",제목_가상_etc1)
제목_가상_etc1 <- gsub("\u5317","북한",제목_가상_etc1)
제목_가상_etc1 <- gsub("\u97D3","대한민국",제목_가상_etc1)
제목_가상_etc1 <- gsub("\u65E5","일본",제목_가상_etc1)
제목_가상_etc1 <- gsub("\u7F8E","미국",제목_가상_etc1)
제목_가상_etc1 <- gsub("\u4F5B","프랑스",제목_가상_etc1)
제목_가상_etc1 <- gsub("\u82F1","영국",제목_가상_etc1)

가상_df_etc1 <- data.frame(언론사_가상_etc1,
                                제목_가상_etc1,
                                본문_가상_etc1,
                                날짜_가상_etc1,
                                기사_가상_etc1)

names(가상_df_etc1) <- c("언론사_가상","제목_가상","본문_가상","날짜_가상","링크_가상")


## news.naver가 아닌 기사
기사_가상_etc2 <- 가상_df %>% 
  filter(제목_가상 == "수동확인") %>% 
  select("링크_가상")

기사_가상_etc2 <- 기사_가상_etc2$링크_가상


# 벡터 생성
언론사_가상_etc2 <- c()
제목_가상_etc2 <- c()
본문_가상_etc2 <- c()
날짜_가상_etc2 <- c()

for (i in 1:length(기사_가상_etc2)){
  tryCatch({
    
    cat(i, '페이지 수집 중입니다.\n') 
    
    body <- 기사_가상_etc2[i] %>% read_html()
    
    #언론사
    언론사_가상.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_가상.tmp) != 0) {
      언론사_가상_etc2 <- append(언론사_가상_etc2,언론사_가상.tmp)
    } else {
      언론사_가상_etc2 <- append(언론사_가상_etc2,"수동확인")
    }    
    
    #제목
    제목_가상.tmp <- body %>% 
      html_nodes("h2.end_tit") %>% 
      html_text() # text 추출
    
    if (length(제목_가상.tmp) != 0) {
      제목_가상_etc2 <- append(제목_가상_etc2,제목_가상.tmp)
    } else {
      제목_가상_etc2 <- append(제목_가상_etc2,"수동확인")
    }      
    
    #본문
    본문_가상.tmp <- body %>%
      html_nodes("div#articeBody") %>%
      html_text()
    
    if (length(본문_가상.tmp) != 0) {
      본문_가상_etc2 <- append(본문_가상_etc2,본문_가상.tmp)
    } else {
      본문_가상_etc2<- append(본문_가상_etc2,"수동확인")
    }
    
    #날짜
    날짜_가상.tmp <- body %>% 
      html_nodes("span.author") %>%
      html_nodes("em") %>%
      html_text()
    
    날짜_가상.tmp <- 날짜_가상.tmp[1]
    
    if (length(날짜_가상.tmp) != 0) {
      날짜_가상_etc2 <- append(날짜_가상_etc2,날짜_가상.tmp)
    } else {
      날짜_가상_etc2 <- append(날짜_가상_etc2,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_가상_etc2 <- gsub("포토","",제목_가상_etc2)
제목_가상_etc2 <- gsub("\u5317","북한",제목_가상_etc2)
제목_가상_etc2 <- gsub("\u97D3","대한민국",제목_가상_etc2)
제목_가상_etc2 <- gsub("\u65E5","일본",제목_가상_etc2)
제목_가상_etc2 <- gsub("\u7F8E","미국",제목_가상_etc2)
제목_가상_etc2 <- gsub("\u4F5B","프랑스",제목_가상_etc2)
제목_가상_etc2 <- gsub("\u82F1","영국",제목_가상_etc2)

가상_df_etc2 <- data.frame(언론사_가상_etc2,
                                제목_가상_etc2,
                                본문_가상_etc2,
                                날짜_가상_etc2,
                                기사_가상_etc2)

names(가상_df_etc2) <- c("언론사_가상","제목_가상","본문_가상","날짜_가상","링크_가상")

가상_df_com <- rbind(가상_df,가상_df_etc1,가상_df_etc2)
가상_df_com <- 가상_df_com %>% filter(가상_df_com$제목_가상 != "수동확인")

write.csv(가상_df_com, file = "C:/대학원/가상_df_com.csv", row.names=FALSE)

#본문 제외
가상_df_com_본문제외 <- 가상_df_com %>% select("언론사_가상","제목_가상","날짜_가상","링크_가상")
write.csv(가상_df_com_본문제외, file = "C:/대학원/가상_df_com_본문제외.csv", row.names=FALSE)

end_time_가상화폐 <- Sys.time()



end_time_암호화폐 - start_time_암호화폐
end_time_비트코인 - start_time_비트코인
end_time_가상화폐 - start_time_가상화폐 