library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)
library(tidyverse)

#### Park
start_time_park <- Sys.time()

## 링크생성
# 2013_dd
ymd13_2_dd <- c()

year13_dd <- c("2013")

mon13_2_dd <- c("02")
day13_2_dd <- c("25","26","27","28","29")

for (i in 1:length(year13_dd)){
  for (j in 1:length(mon13_2_dd)){
    for (k in 1:length(day13_2_dd)){
      ymd13_2_dd.tmp <- paste0(year13_dd[i],".",mon13_2_dd[j],".",day13_2_dd[k])
      ymd13_2_dd <- append(ymd13_2_dd,ymd13_2_dd.tmp)
    }
  }
}

ymd13_dd <- c()

mon13_dd <- c("03","04","05","06","07","08","09","10","11","12")
day13_dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

for (i in 1:length(year13_dd)){
  for (j in 1:length(mon13_dd)){
    for (k in 1:length(day13_dd)){
    ymd13_dd.tmp <- paste0(year13_dd[i],".",mon13_dd[j],".",day13_dd[k])
    ymd13_dd <- append(ymd13_dd,ymd13_dd.tmp)
    }
  }
}
  
# 20141516_dd
ymd1416_dd <- c()

year1416_dd <- c("2014","2015","2016")

mon1416_dd <- c("03","04","05","06","07","08","09","10","11","12")
day1416_dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

for (i in 1:length(year1416_dd)){
  for (j in 1:length(mon1416_dd)){
    for (k in 1:length(day1416_dd)){
    ymd1416_dd.tmp <- paste0(year1416_dd[i],".",mon1416_dd[j],".",day1416_dd[k])
    ymd1416_dd <- append(ymd1416_dd,ymd1416_dd.tmp)
    }
  }
}

# 2017_dd
ymd17_dd <- c()

year17_dd <- c("2017")

mon17_dd <- c("01","02")
day17_dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

for (i in 1:length(year17_dd)){
  for (j in 1:length(mon17_dd)){
    for (k in 1:length(day17_dd)){
      ymd17_dd.tmp <- paste0(year17_dd[i],".",mon17_dd[j],".",day17_dd[k])
      ymd17_dd <- append(ymd17_dd,ymd17_dd.tmp)
    }
  }
}

ymd17_3_dd <- c()

year17_dd <- c("2017")

mon17_3_dd <- c("03")
day17_3_dd <- c("01","02","03","04","05","06","07","08","09","10")

for (i in 1:length(year17_dd)){
  for (j in 1:length(mon17_3_dd)){
    for (k in 1:length(day17_3_dd)){
      ymd17_3_dd.tmp <- paste0(year17_dd[i],".",mon17_3_dd[j],".",day17_3_dd[k])
      ymd17_3_dd <- append(ymd17_3_dd,ymd17_3_dd.tmp)
    }
  }
}

dd_park <- c()

dd_park <- append(dd_park,ymd13_2_dd)
dd_park <- append(dd_park,ymd13_dd)
dd_park <- append(dd_park,ymd1416_dd)
dd_park <- append(dd_park,ymd17_dd)
dd_park <- append(dd_park,ymd17_3_dd)

# from
fm_park <- gsub("\\.","",dd_park)

# 링크에 dd from 붙이기
head <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query="
searchword <- "탈북자"
body1 <- "&sort=1&photo=0&field=0&pd=3&ds="
dd_park <- dd_park
body2 <- "&de="
dd_park <- dd_park
body3 <- "&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:dd,p:from"
fm_park <- fm_park 
to <- "to"
fm_park <- fm_park 
body4 <- ",a:all&start="
seq <- seq(from = 1, by = 10, length.out = 400)

## 네이버뉴스 링크 수집
# 기사링크 벡터 생성
기사_탈북_park <- c()

# 네이버뉴스 링크 수집
for (i in 1:365){
    for (j in 1:400){
      tryCatch({
        
        cat(dd_park[i], j, '페이지 수집 중', '입니다.\n') 
    
    body <- paste0(head,searchword,body1,dd_park[i],body2,dd_park[i],body3,fm_park[i],to,fm_park[i],body4,seq[j]) %>% read_html()
    
    #기사
    기사_탈북.tmp <- body %>% 
      html_nodes("div.info_group") %>%
      html_nodes("a:nth-child(3)") %>%
      html_attr("href") # 링크등 해당 속성의 값을 추출함 
    
    if (length(기사_탈북.tmp) != 0) {
      기사_탈북_park <- append(기사_탈북_park,기사_탈북.tmp)
    } else { break }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  }
}

기사_탈북_park <- 기사_탈북_park %>% unique()
기사_탈북_park %>% length()

news.naver_기사_탈북_park <- grep("https://news.naver.com",기사_탈북_park)
기사_탈북_park_news <- 기사_탈북_park[news.naver_기사_탈북_park] # 최종 기사 링크
기사_탈북_park_etc1 <- 기사_탈북_park[-news.naver_기사_탈북_park] ## sports인지 확인

# 링크 분할
기사_탈북_park_sp <- strsplit(기사_탈북_park_news, split="&")  

기사_탈북_park_aid <- c()
기사_탈북_park_oid <- c()

for (i in 1:length(기사_탈북_park_sp)){
  기사_탈북_park_sp_oid.tmp <- 기사_탈북_park_sp[[i]][4]
  기사_탈북_park_sp_aid.tmp <- 기사_탈북_park_sp[[i]][5]

  기사_탈북_park_oid <- append(기사_탈북_park_oid,기사_탈북_park_sp_oid.tmp)
  기사_탈북_park_aid <- append(기사_탈북_park_aid,기사_탈북_park_sp_aid.tmp)
}

기사_탈북_park_oid <- gsub("oid=","",기사_탈북_park_oid)
기사_탈북_park_aid <- gsub("aid=","",기사_탈북_park_aid)


## 페이지 정보 읽기 - 최신순 정렬
# 벡터 생성
언론사_탈북_park <- c()
제목_탈북_park <- c()
본문_탈북_park <- c()
날짜_탈북_park <- c()

for (i in 1:length(기사_탈북_park_news)){
  tryCatch({
    res_탈북_park <- GET(url = 'https://news.naver.com/main/read.nhn',
                               query = list(
                               oid = 기사_탈북_park_oid[i],
                               aid = 기사_탈북_park_aid[i]))
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_탈북_park), '입니다.\n') 
    
    body <- res_탈북_park %>% read_html()
    
    #언론사
    언론사_탈북.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_탈북.tmp) != 0) {
      언론사_탈북_park <- append(언론사_탈북_park,언론사_탈북.tmp)
    } else {
      언론사_탈북_park <- append(언론사_탈북_park,"수동확인")
    }    
    
    #제목
    제목_탈북.tmp <- body %>% 
      html_nodes("div.article_info") %>%
      html_nodes("h3") %>%
      html_text() # text 추출
    
    if (length(제목_탈북.tmp) != 0) {
      제목_탈북_park <- append(제목_탈북_park,제목_탈북.tmp)
    } else {
      제목_탈북_park <- append(제목_탈북_park,"수동확인")
    }      
    
    #본문
    # 본문_탈북.tmp <- body %>% 
    #   html_nodes("div#articleBodyContents") %>%
    #   html_text()
    # 
    # if (length(본문_탈북.tmp) != 0) {
    #   본문_탈북_park <- append(본문_탈북_park,본문_탈북.tmp)
    # } else {
    #   본문_탈북_park <- append(본문_탈북_park,"수동확인")
    # }   
    
    #날짜
    날짜_탈북.tmp <- body %>% 
      html_nodes("span.t11") %>%
      html_text()
    
    날짜_탈북.tmp <- 날짜_탈북.tmp[1]
    
    if (length(날짜_탈북.tmp) != 0) {
      날짜_탈북_park <- append(날짜_탈북_park,날짜_탈북.tmp)
    } else {
      날짜_탈북_park <- append(날짜_탈북_park,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_탈북_park <- gsub("포토","",제목_탈북_park)
제목_탈북_park <- gsub("\u5317","북한",제목_탈북_park)
제목_탈북_park <- gsub("\u97D3","대한민국",제목_탈북_park)
제목_탈북_park <- gsub("\u65E5","일본",제목_탈북_park)
제목_탈북_park <- gsub("\u7F8E","미국",제목_탈북_park)
제목_탈북_park <- gsub("\u4F5B","프랑스",제목_탈북_park)
제목_탈북_park <- gsub("\u82F1","영국",제목_탈북_park)

탈북_park_df <- data.frame(언론사_탈북_park[1:10000],
                           제목_탈북_park[1:10000],
                           날짜_탈북_park[1:10000],
                           기사_탈북_park_news[1:10000])

names(탈북_park_df) <- c("언론사_park","제목_park","날짜_park","링크_park")


### sports.news 기사
# 링크 분할
기사_탈북_park_etc1_sp <- strsplit(기사_탈북_park_etc1, split="=")  

기사_탈북_park_etc1_aid <- c()
기사_탈북_park_etc1_oid <- c()

for (i in 1:length(기사_탈북_park_etc1)){
  기사_탈북_park_sp_oid.tmp <- 기사_탈북_park_etc1_sp[[i]][2]
  기사_탈북_park_sp_aid.tmp <- 기사_탈북_park_etc1_sp[[i]][3]
  
  기사_탈북_park_etc1_oid <- append(기사_탈북_park_etc1_oid,기사_탈북_park_sp_oid.tmp)
  기사_탈북_park_etc1_aid <- append(기사_탈북_park_etc1_aid,기사_탈북_park_sp_aid.tmp)
}

기사_탈북_park_etc1_oid <- gsub("&aid","",기사_탈북_park_etc1_oid)


## 페이지 정보 읽기 - 최신순 정렬
# 벡터 생성
언론사_탈북_park_etc1 <- c()
제목_탈북_park_etc1 <- c()
본문_탈북_park_etc1 <- c()
날짜_탈북_park_etc1 <- c()

for (i in 1:length(기사_탈북_park_etc1)){
  tryCatch({
    res_탈북_park_etc1 <- GET(url = 'https://sports.news.naver.com/news.nhn',
                       query = list(
                         oid = 기사_탈북_park_etc1_oid[i],
                         aid = 기사_탈북_park_etc1_aid[i]))
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_탈북_park_etc1), '입니다.\n') 
    
    body <- res_탈북_park_etc1 %>% read_html()

    #언론사
    언론사_탈북.tmp <- body %>% 
      html_nodes("a.link") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_탈북.tmp) != 0) {
      언론사_탈북_park_etc1 <- append(언론사_탈북_park_etc1,언론사_탈북.tmp)
    } else {
      언론사_탈북_park_etc1 <- append(언론사_탈북_park_etc1,"수동확인")
    }    
    
    #제목
    제목_탈북.tmp <- body %>% 
      html_nodes("h4.title") %>%
      html_text()
    
    if (length(제목_탈북.tmp) != 0) {
      제목_탈북_park_etc1 <- append(제목_탈북_park_etc1,제목_탈북.tmp)
    } else {
      제목_탈북_park_etc1 <- append(제목_탈북_park_etc1,"수동확인")
    }    
    
    # #본문
    # 본문_탈북.tmp <- body %>%
    #   html_nodes("div#newsEndContents") %>%
    #   html_text()
    # 
    # if (length(본문_탈북.tmp) != 0) {
    #   본문_탈북_park_etc1 <- append(본문_탈북_park_etc1,본문_탈북.tmp)
    # } else {
    #   본문_탈북_park_etc1 <- append(본문_탈북_park_etc1,"수동확인")
    # }
    
    #날짜
    날짜_탈북.tmp <- body %>% 
      html_nodes("div.info") %>%
      html_nodes("span") %>%
      html_text()
    
    날짜_탈북.tmp <- 날짜_탈북.tmp[1]
    날짜_탈북.tmp <- gsub("기사입력","",날짜_탈북.tmp)
    
    if (length(날짜_탈북.tmp) != 0) {
      날짜_탈북_park_etc1 <- append(날짜_탈북_park_etc1,날짜_탈북.tmp)
    } else {
      날짜_탈북_park_etc1 <- append(날짜_탈북_park_etc1,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}


제목_탈북_park_etc1 <- gsub("포토","",제목_탈북_park_etc1)
제목_탈북_park_etc1 <- gsub("\u5317","북한",제목_탈북_park_etc1)
제목_탈북_park_etc1 <- gsub("\u97D3","대한민국",제목_탈북_park_etc1)
제목_탈북_park_etc1 <- gsub("\u65E5","일본",제목_탈북_park_etc1)
제목_탈북_park_etc1 <- gsub("\u7F8E","미국",제목_탈북_park_etc1)
제목_탈북_park_etc1 <- gsub("\u4F5B","프랑스",제목_탈북_park_etc1)
제목_탈북_park_etc1 <- gsub("\u82F1","영국",제목_탈북_park_etc1)

탈북_park_df_etc1 <- data.frame(언론사_탈북_park_etc1,
                                제목_탈북_park_etc1,
                                날짜_탈북_park_etc1,
                                기사_탈북_park_etc1)

names(탈북_park_df_etc1) <- c("언론사_park","제목_park","날짜_park","링크_park")


## news.naver가 아닌 기사
기사_탈북_park_etc2 <- 탈북_park_df %>% 
  filter(제목_park == "수동확인") %>% 
  select("링크_park")

기사_탈북_park_etc2 <- 기사_탈북_park_etc2$링크_park


# 벡터 생성
언론사_탈북_park_etc2 <- c()
제목_탈북_park_etc2 <- c()
본문_탈북_park_etc2 <- c()
날짜_탈북_park_etc2 <- c()

for (i in 1:length(기사_탈북_park_etc2)){
  tryCatch({

    cat(i, '페이지 수집 중입니다.\n') 
    
    body <- 기사_탈북_park_etc2[i] %>% read_html()
    
    #언론사
    언론사_탈북.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_탈북.tmp) != 0) {
      언론사_탈북_park_etc2 <- append(언론사_탈북_park_etc2,언론사_탈북.tmp)
    } else {
      언론사_탈북_park_etc2 <- append(언론사_탈북_park_etc2,"수동확인")
    }    
    
    #제목
    제목_탈북.tmp <- body %>% 
      html_nodes("h2.end_tit") %>% 
      html_text() # text 추출
    
    if (length(제목_탈북.tmp) != 0) {
      제목_탈북_park_etc2 <- append(제목_탈북_park_etc2,제목_탈북.tmp)
    } else {
      제목_탈북_park_etc2 <- append(제목_탈북_park_etc2,"수동확인")
    }      
    
    # 본문
    # 본문_탈북.tmp <- body %>%
    #   html_nodes("div#articeBody") %>%
    #   html_text()
    # 
    # if (length(본문_탈북.tmp) != 0) {
    #   본문_탈북 <- append(본문_탈북본문_탈북.tmp)
    # } else {
    #   본문_탈북<- append(본문_탈북,"수동확인")
    # }

    #날짜
    날짜_탈북.tmp <- body %>% 
      html_nodes("span.author") %>%
      html_nodes("em") %>%
      html_text()
    
    날짜_탈북.tmp <- 날짜_탈북.tmp[1]
    
    if (length(날짜_탈북.tmp) != 0) {
      날짜_탈북_park_etc2 <- append(날짜_탈북_park_etc2,날짜_탈북.tmp)
    } else {
      날짜_탈북_park_etc2 <- append(날짜_탈북_park_etc2,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_탈북_park_etc2 <- gsub("포토","",제목_탈북_park_etc2)
제목_탈북_park_etc2 <- gsub("\u5317","북한",제목_탈북_park_etc2)
제목_탈북_park_etc2 <- gsub("\u97D3","대한민국",제목_탈북_park_etc2)
제목_탈북_park_etc2 <- gsub("\u65E5","일본",제목_탈북_park_etc2)
제목_탈북_park_etc2 <- gsub("\u7F8E","미국",제목_탈북_park_etc2)
제목_탈북_park_etc2 <- gsub("\u4F5B","프랑스",제목_탈북_park_etc2)
제목_탈북_park_etc2 <- gsub("\u82F1","영국",제목_탈북_park_etc2)

탈북_park_df_etc2 <- data.frame(언론사_탈북_park_etc2[1:100],
                                제목_탈북_park_etc2[1:100],
                                날짜_탈북_park_etc2[1:100],
                                기사_탈북_park_etc2[1:100])

names(탈북_park_df_etc2) <- c("언론사_park","제목_park","날짜_park","링크_park")

탈북_park_df_com <- rbind(탈북_park_df,탈북_park_df_etc1,탈북_park_df_etc2)

write.csv(탈북_park_df_com, file = "C:/대학원/논문/탈북_park_df_com.csv", row.names=FALSE)

news.naver_기사_탈북_moon <- grep("https://news.naver.com",기사_탈북_moon)

end_time_park <- Sys.time()

end_time_park - start_time_park 

