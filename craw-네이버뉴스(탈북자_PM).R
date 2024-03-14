library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)
library(tidyverse)

#### Park
start_park_moon_탈북 <- Sys.time()

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
for (i in 1:length(dd_park)){
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

## 최종 기사 링크 - 최신순 정렬
# 벡터 생성
언론사_탈북_park <- c()
제목_탈북_park <- c()
본문_탈북_park <- c()
날짜_탈북_park <- c()

for (i in 1:length(기사_탈북_park_news)){
  tryCatch({

    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- 기사_탈북_park_news[i] %>% read_html()
    
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
    본문_탈북.tmp <- body %>%
      html_nodes("div#articleBodyContents") %>%
      html_text()

    if (length(본문_탈북.tmp) != 0) {
      본문_탈북_park <- append(본문_탈북_park,본문_탈북.tmp)
    } else {
      본문_탈북_park <- append(본문_탈북_park,"수동확인")
    }
    
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

탈북_park_df <- data.frame(언론사_탈북_park,
                                제목_탈북_park,
                                본문_탈북_park,
                                날짜_탈북_park,
                                기사_탈북_park_news)

names(탈북_park_df) <- c("언론사_park","제목_park","본문_park","날짜_park","링크_park")


### sports.news 기사
## 페이지 정보 읽기 - 최신순 정렬
# 벡터 생성
언론사_탈북_park_etc1 <- c()
제목_탈북_park_etc1 <- c()
본문_탈북_park_etc1 <- c()
날짜_탈북_park_etc1 <- c()

for (i in 1:length(기사_탈북_park_etc1)){
  tryCatch({

    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- 기사_탈북_park_etc1[i] %>% read_html()
    
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
    
    #본문
    본문_탈북.tmp <- body %>%
      html_nodes("div#newsEndContents") %>%
      html_text()

    if (length(본문_탈북.tmp) != 0) {
      본문_탈북_park_etc1 <- append(본문_탈북_park_etc1,본문_탈북.tmp)
    } else {
      본문_탈북_park_etc1 <- append(본문_탈북_park_etc1,"수동확인")
    }
    
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
                                     본문_탈북_park_etc1,
                                     날짜_탈북_park_etc1,
                                     기사_탈북_park_etc1)

names(탈북_park_df_etc1) <- c("언론사_park","제목_park","본문_park","날짜_park","링크_park")


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
    
    #본문
    본문_탈북.tmp <- body %>%
      html_nodes("div#articeBody") %>%
      html_text()

    if (length(본문_탈북.tmp) != 0) {
      본문_탈북_park_etc2 <- append(본문_탈북_park_etc2,본문_탈북.tmp)
    } else {
      본문_탈북_park_etc2<- append(본문_탈북_park_etc2,"수동확인")
    }
    
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

탈북_park_df_etc2 <- data.frame(언론사_탈북_park_etc2,
                                     제목_탈북_park_etc2,
                                     본문_탈북_park_etc2,
                                     날짜_탈북_park_etc2,
                                     기사_탈북_park_etc2)
본문_탈북_park_etc2 %>% length()

names(탈북_park_df_etc2) <- c("언론사_park","제목_park","본문_park","날짜_park","링크_park")

탈북_park_df_com <- rbind(탈북_park_df,탈북_park_df_etc1,탈북_park_df_etc2)
탈북_park_df_com <- 탈북_park_df_com %>% filter(탈북_park_df_com$제목_park != "수동확인")

write.csv(탈북_park_df_com, file = "C:/대학원/탈북_park_df_com.csv", row.names=FALSE)

#본문 제외
탈북_park_df_com_본문제외 <- 탈북_park_df_com %>% select("언론사_park","제목_park","날짜_park","링크_park")
write.csv(탈북_park_df_com_본문제외, file = "C:/대학원/탈북_park_df_com_본문제외.csv", row.names=FALSE)

end_time_park_탈북 <- Sys.time()



#### moon
start_time_moon_탈북 <- Sys.time()

## 링크생성
# 2017_dd
ymd17_5_dd <- c()

year17_dd <- c("2017")

mon17_5_dd <- c("05")
day17_5_dd <- c("10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

for (i in 1:length(year17_dd)){
  for (j in 1:length(mon17_5_dd)){
    for (k in 1:length(day17_5_dd)){
      ymd17_5_dd.tmp <- paste0(year17_dd[i],".",mon17_5_dd[j],".",day17_5_dd[k])
      ymd17_5_dd <- append(ymd17_5_dd,ymd17_5_dd.tmp)
    }
  }
}

ymd17_dd <- c()

mon17_dd <- c("06","07","08","09","10","11","12")
day17_dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

for (i in 1:length(year17_dd)){
  for (j in 1:length(mon17_dd)){
    for (k in 1:length(day17_dd)){
      ymd17_dd.tmp <- paste0(year17_dd[i],".",mon17_dd[j],".",day17_dd[k])
      ymd17_dd <- append(ymd17_dd,ymd17_dd.tmp)
    }
  }
}

# 20181920_dd
ymd1820_dd <- c()

year1820_dd <- c("2018","2019","2020")

mon1820_dd <- c("01","02","03","04","05","06","07","08","09","10","11","12")
day1820_dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

for (i in 1:length(year1820_dd)){
  for (j in 1:length(mon1820_dd)){
    for (k in 1:length(day1820_dd)){
      ymd1820_dd.tmp <- paste0(year1820_dd[i],".",mon1820_dd[j],".",day1820_dd[k])
      ymd1820_dd <- append(ymd1820_dd,ymd1820_dd.tmp)
    }
  }
}

# 2021_dd
ymd21_dd <- c()

year21_dd <- c("2021")

mon21_dd <- c("01","02","03")
day21_dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

for (i in 1:length(year21_dd)){
  for (j in 1:length(mon21_dd)){
    for (k in 1:length(day21_dd)){
      ymd21_dd.tmp <- paste0(year21_dd[i],".",mon21_dd[j],".",day21_dd[k])
      ymd21_dd <- append(ymd21_dd,ymd21_dd.tmp)
    }
  }
}

dd_moon <- c()

dd_moon <- append(dd_moon,ymd17_5_dd)
dd_moon <- append(dd_moon,ymd17_dd)
dd_moon <- append(dd_moon,ymd1820_dd)
dd_moon <- append(dd_moon,ymd21_dd)

# from
fm_moon <- gsub("\\.","",dd_moon)

# 링크에 dd from 붙이기
head <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query="
searchword <- "탈북자"
body1 <- "&sort=1&photo=0&field=0&pd=3&ds="
dd_moon <- dd_moon
body2 <- "&de="
dd_moon <- dd_moon
body3 <- "&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:dd,p:from"
fm_moon <- fm_moon 
to <- "to"
fm_moon <- fm_moon 
body4 <- ",a:all&start="
seq <- seq(from = 1, by = 10, length.out = 400)

## 네이버뉴스 링크 수집
# 기사링크 벡터 생성
기사_탈북_moon <- c()

# 네이버뉴스 링크 수집
for (i in 1:length(dd_moon)){
  for (j in 1:400){
    tryCatch({
      
      cat(dd_moon[i], j, '페이지 수집 중', '입니다.\n') 
      
      body <- paste0(head,searchword,body1,dd_moon[i],body2,dd_moon[i],body3,fm_moon[i],to,fm_moon[i],body4,seq[j]) %>% read_html()
      
      #기사
      기사_탈북.tmp <- body %>% 
        html_nodes("div.info_group") %>%
        html_nodes("a:nth-child(3)") %>%
        html_attr("href") # 링크등 해당 속성의 값을 추출함 
      
      if (length(기사_탈북.tmp) != 0) {
        기사_탈북_moon <- append(기사_탈북_moon,기사_탈북.tmp)
      } else { break }    
      
      Sys.sleep(time = 0.01)
      
    }, error = function(e) cat("불러올 수 없습니다!\n"))
  }
}

기사_탈북_moon <- 기사_탈북_moon %>% unique()
기사_탈북_moon %>% length()

news.naver_기사_탈북_moon <- grep("https://news.naver.com",기사_탈북_moon)
기사_탈북_moon_news <- 기사_탈북_moon[news.naver_기사_탈북_moon] # 최종 기사 링크
기사_탈북_moon_etc1 <- 기사_탈북_moon[-news.naver_기사_탈북_moon] ## sports인지 확인

## 최종 기사 링크 - 최신순 정렬
# 벡터 생성
언론사_탈북_moon <- c()
제목_탈북_moon <- c()
본문_탈북_moon <- c()
날짜_탈북_moon <- c()

for (i in 1:length(기사_탈북_moon_news)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- 기사_탈북_moon_news[i] %>% read_html()
    
    #언론사
    언론사_탈북.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_탈북.tmp) != 0) {
      언론사_탈북_moon <- append(언론사_탈북_moon,언론사_탈북.tmp)
    } else {
      언론사_탈북_moon <- append(언론사_탈북_moon,"수동확인")
    }    
    
    #제목
    제목_탈북.tmp <- body %>% 
      html_nodes("div.article_info") %>%
      html_nodes("h3") %>%
      html_text() # text 추출
    
    if (length(제목_탈북.tmp) != 0) {
      제목_탈북_moon <- append(제목_탈북_moon,제목_탈북.tmp)
    } else {
      제목_탈북_moon <- append(제목_탈북_moon,"수동확인")
    }      
    
    #본문
    본문_탈북.tmp <- body %>%
      html_nodes("div#articleBodyContents") %>%
      html_text()
    
    if (length(본문_탈북.tmp) != 0) {
      본문_탈북_moon <- append(본문_탈북_moon,본문_탈북.tmp)
    } else {
      본문_탈북_moon <- append(본문_탈북_moon,"수동확인")
    }
    
    #날짜
    날짜_탈북.tmp <- body %>% 
      html_nodes("span.t11") %>%
      html_text()
    
    날짜_탈북.tmp <- 날짜_탈북.tmp[1]
    
    if (length(날짜_탈북.tmp) != 0) {
      날짜_탈북_moon <- append(날짜_탈북_moon,날짜_탈북.tmp)
    } else {
      날짜_탈북_moon <- append(날짜_탈북_moon,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_탈북_moon <- gsub("포토","",제목_탈북_moon)
제목_탈북_moon <- gsub("\u5317","북한",제목_탈북_moon)
제목_탈북_moon <- gsub("\u97D3","대한민국",제목_탈북_moon)
제목_탈북_moon <- gsub("\u65E5","일본",제목_탈북_moon)
제목_탈북_moon <- gsub("\u7F8E","미국",제목_탈북_moon)
제목_탈북_moon <- gsub("\u4F5B","프랑스",제목_탈북_moon)
제목_탈북_moon <- gsub("\u82F1","영국",제목_탈북_moon)

탈북_moon_df <- data.frame(언론사_탈북_moon,
                                제목_탈북_moon,
                                본문_탈북_moon,
                                날짜_탈북_moon,
                                기사_탈북_moon_news)

names(탈북_moon_df) <- c("언론사_moon","제목_moon","본문_moon","날짜_moon","링크_moon")


### sports.news 기사
## 페이지 정보 읽기 - 최신순 정렬
# 벡터 생성
언론사_탈북_moon_etc1 <- c()
제목_탈북_moon_etc1 <- c()
본문_탈북_moon_etc1 <- c()
날짜_탈북_moon_etc1 <- c()

for (i in 1:length(기사_탈북_moon_etc1)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- 기사_탈북_moon_etc1[i] %>% read_html()
    
    #언론사
    언론사_탈북.tmp <- body %>% 
      html_nodes("a.link") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_탈북.tmp) != 0) {
      언론사_탈북_moon_etc1 <- append(언론사_탈북_moon_etc1,언론사_탈북.tmp)
    } else {
      언론사_탈북_moon_etc1 <- append(언론사_탈북_moon_etc1,"수동확인")
    }    
    
    #제목
    제목_탈북.tmp <- body %>% 
      html_nodes("h4.title") %>%
      html_text()
    
    if (length(제목_탈북.tmp) != 0) {
      제목_탈북_moon_etc1 <- append(제목_탈북_moon_etc1,제목_탈북.tmp)
    } else {
      제목_탈북_moon_etc1 <- append(제목_탈북_moon_etc1,"수동확인")
    }    
    
    #본문
    본문_탈북.tmp <- body %>%
      html_nodes("div#newsEndContents") %>%
      html_text()
    
    if (length(본문_탈북.tmp) != 0) {
      본문_탈북_moon_etc1 <- append(본문_탈북_moon_etc1,본문_탈북.tmp)
    } else {
      본문_탈북_moon_etc1 <- append(본문_탈북_moon_etc1,"수동확인")
    }
    
    #날짜
    날짜_탈북.tmp <- body %>% 
      html_nodes("div.info") %>%
      html_nodes("span") %>%
      html_text()
    
    날짜_탈북.tmp <- 날짜_탈북.tmp[1]
    날짜_탈북.tmp <- gsub("기사입력","",날짜_탈북.tmp)
    
    if (length(날짜_탈북.tmp) != 0) {
      날짜_탈북_moon_etc1 <- append(날짜_탈북_moon_etc1,날짜_탈북.tmp)
    } else {
      날짜_탈북_moon_etc1 <- append(날짜_탈북_moon_etc1,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_탈북_moon_etc1 <- gsub("포토","",제목_탈북_moon_etc1)
제목_탈북_moon_etc1 <- gsub("\u5317","북한",제목_탈북_moon_etc1)
제목_탈북_moon_etc1 <- gsub("\u97D3","대한민국",제목_탈북_moon_etc1)
제목_탈북_moon_etc1 <- gsub("\u65E5","일본",제목_탈북_moon_etc1)
제목_탈북_moon_etc1 <- gsub("\u7F8E","미국",제목_탈북_moon_etc1)
제목_탈북_moon_etc1 <- gsub("\u4F5B","프랑스",제목_탈북_moon_etc1)
제목_탈북_moon_etc1 <- gsub("\u82F1","영국",제목_탈북_moon_etc1)

탈북_moon_df_etc1 <- data.frame(언론사_탈북_moon_etc1,
                                     제목_탈북_moon_etc1,
                                     본문_탈북_moon_etc1,
                                     날짜_탈북_moon_etc1,
                                     기사_탈북_moon_etc1)

names(탈북_moon_df_etc1) <- c("언론사_moon","제목_moon","본문_moon","날짜_moon","링크_moon")


## news.naver가 아닌 기사
기사_탈북_moon_etc2 <- 탈북_moon_df %>% 
  filter(제목_moon == "수동확인") %>% 
  select("링크_moon")

기사_탈북_moon_etc2 <- 기사_탈북_moon_etc2$링크_moon


# 벡터 생성
언론사_탈북_moon_etc2 <- c()
제목_탈북_moon_etc2 <- c()
본문_탈북_moon_etc2 <- c()
날짜_탈북_moon_etc2 <- c()

for (i in 1:length(기사_탈북_moon_etc2)){
  tryCatch({
    
    cat(i, '페이지 수집 중입니다.\n') 
    
    body <- 기사_탈북_moon_etc2[i] %>% read_html()
    
    #언론사
    언론사_탈북.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_탈북.tmp) != 0) {
      언론사_탈북_moon_etc2 <- append(언론사_탈북_moon_etc2,언론사_탈북.tmp)
    } else {
      언론사_탈북_moon_etc2 <- append(언론사_탈북_moon_etc2,"수동확인")
    }    
    
    #제목
    제목_탈북.tmp <- body %>% 
      html_nodes("h2.end_tit") %>% 
      html_text() # text 추출
    
    if (length(제목_탈북.tmp) != 0) {
      제목_탈북_moon_etc2 <- append(제목_탈북_moon_etc2,제목_탈북.tmp)
    } else {
      제목_탈북_moon_etc2 <- append(제목_탈북_moon_etc2,"수동확인")
    }      
    
    #본문
    본문_탈북.tmp <- body %>%
      html_nodes("div#articeBody") %>%
      html_text()
    
    if (length(본문_탈북.tmp) != 0) {
      본문_탈북_moon_etc2 <- append(본문_탈북_moon_etc2,본문_탈북.tmp)
    } else {
      본문_탈북_moon_etc2<- append(본문_탈북_moon_etc2,"수동확인")
    }
    
    #날짜
    날짜_탈북.tmp <- body %>% 
      html_nodes("span.author") %>%
      html_nodes("em") %>%
      html_text()
    
    날짜_탈북.tmp <- 날짜_탈북.tmp[1]
    
    if (length(날짜_탈북.tmp) != 0) {
      날짜_탈북_moon_etc2 <- append(날짜_탈북_moon_etc2,날짜_탈북.tmp)
    } else {
      날짜_탈북_moon_etc2 <- append(날짜_탈북_moon_etc2,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_탈북_moon_etc2 <- gsub("포토","",제목_탈북_moon_etc2)
제목_탈북_moon_etc2 <- gsub("\u5317","북한",제목_탈북_moon_etc2)
제목_탈북_moon_etc2 <- gsub("\u97D3","대한민국",제목_탈북_moon_etc2)
제목_탈북_moon_etc2 <- gsub("\u65E5","일본",제목_탈북_moon_etc2)
제목_탈북_moon_etc2 <- gsub("\u7F8E","미국",제목_탈북_moon_etc2)
제목_탈북_moon_etc2 <- gsub("\u4F5B","프랑스",제목_탈북_moon_etc2)
제목_탈북_moon_etc2 <- gsub("\u82F1","영국",제목_탈북_moon_etc2)

탈북_moon_df_etc2 <- data.frame(언론사_탈북_moon_etc2,
                                     제목_탈북_moon_etc2,
                                     본문_탈북_moon_etc2,
                                     날짜_탈북_moon_etc2,
                                     기사_탈북_moon_etc2)

names(탈북_moon_df_etc2) <- c("언론사_moon","제목_moon","본문_moon","날짜_moon","링크_moon")

탈북_moon_df_com <- rbind(탈북_moon_df,탈북_moon_df_etc1,탈북_moon_df_etc2)
탈북_moon_df_com <- 탈북_moon_df_com %>% filter(탈북_moon_df_com$제목_moon != "수동확인")

write.csv(탈북_moon_df_com, file = "C:/대학원/탈북_moon_df_com.csv", row.names=FALSE)

#본문 제외
탈북_moon_df_com_본문제외 <- 탈북_moon_df_com %>% select("언론사_moon","제목_moon","날짜_moon","링크_moon")
write.csv(탈북_moon_df_com_본문제외, file = "C:/대학원/탈북_moon_df_com_본문제외.csv", row.names=FALSE)

end_time_moon_탈북 <- Sys.time()


#### Park
start_park_park_새터 <- Sys.time()

## 링크생성
# 링크에 dd from 붙이기
head <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query="
searchword <- "새터민"
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
기사_새터_park <- c()

# 네이버뉴스 링크 수집
for (i in 1:length(dd_park)){
  for (j in 1:400){
    tryCatch({
      
      cat(dd_park[i], j, '페이지 수집 중', '입니다.\n') 
      
      body <- paste0(head,searchword,body1,dd_park[i],body2,dd_park[i],body3,fm_park[i],to,fm_park[i],body4,seq[j]) %>% read_html()
      
      #기사
      기사_새터.tmp <- body %>% 
        html_nodes("div.info_group") %>%
        html_nodes("a:nth-child(3)") %>%
        html_attr("href") # 링크등 해당 속성의 값을 추출함 
      
      if (length(기사_새터.tmp) != 0) {
        기사_새터_park <- append(기사_새터_park,기사_새터.tmp)
      } else { break }    
      
      Sys.sleep(time = 0.01)
      
    }, error = function(e) cat("불러올 수 없습니다!\n"))
  }
}

기사_새터_park <- 기사_새터_park %>% unique()
기사_새터_park %>% length()

news.naver_기사_새터_park <- grep("https://news.naver.com",기사_새터_park)
기사_새터_park_news <- 기사_새터_park[news.naver_기사_새터_park] # 최종 기사 링크
기사_새터_park_etc1 <- 기사_새터_park[-news.naver_기사_새터_park] ## sports인지 확인

## 최종 기사 링크 - 최신순 정렬
# 벡터 생성
언론사_새터_park <- c()
제목_새터_park <- c()
본문_새터_park <- c()
날짜_새터_park <- c()

for (i in 1:length(기사_새터_park_news)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- 기사_새터_park_news[i] %>% read_html()
    
    #언론사
    언론사_새터.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_새터.tmp) != 0) {
      언론사_새터_park <- append(언론사_새터_park,언론사_새터.tmp)
    } else {
      언론사_새터_park <- append(언론사_새터_park,"수동확인")
    }    
    
    #제목
    제목_새터.tmp <- body %>% 
      html_nodes("div.article_info") %>%
      html_nodes("h3") %>%
      html_text() # text 추출
    
    if (length(제목_새터.tmp) != 0) {
      제목_새터_park <- append(제목_새터_park,제목_새터.tmp)
    } else {
      제목_새터_park <- append(제목_새터_park,"수동확인")
    }      
    
    #본문
    본문_새터.tmp <- body %>%
      html_nodes("div#articleBodyContents") %>%
      html_text()
    
    if (length(본문_새터.tmp) != 0) {
      본문_새터_park <- append(본문_새터_park,본문_새터.tmp)
    } else {
      본문_새터_park <- append(본문_새터_park,"수동확인")
    }
    
    #날짜
    날짜_새터.tmp <- body %>% 
      html_nodes("span.t11") %>%
      html_text()
    
    날짜_새터.tmp <- 날짜_새터.tmp[1]
    
    if (length(날짜_새터.tmp) != 0) {
      날짜_새터_park <- append(날짜_새터_park,날짜_새터.tmp)
    } else {
      날짜_새터_park <- append(날짜_새터_park,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_새터_park <- gsub("포토","",제목_새터_park)
제목_새터_park <- gsub("\u5317","북한",제목_새터_park)
제목_새터_park <- gsub("\u97D3","대한민국",제목_새터_park)
제목_새터_park <- gsub("\u65E5","일본",제목_새터_park)
제목_새터_park <- gsub("\u7F8E","미국",제목_새터_park)
제목_새터_park <- gsub("\u4F5B","프랑스",제목_새터_park)
제목_새터_park <- gsub("\u82F1","영국",제목_새터_park)

새터_park_df <- data.frame(언론사_새터_park,
                                제목_새터_park,
                                본문_새터_park,
                                날짜_새터_park,
                                기사_새터_park_news)

names(새터_park_df) <- c("언론사_park","제목_park","본문_park","날짜_park","링크_park")


### sports.news 기사
## 페이지 정보 읽기 - 최신순 정렬
# 벡터 생성
언론사_새터_park_etc1 <- c()
제목_새터_park_etc1 <- c()
본문_새터_park_etc1 <- c()
날짜_새터_park_etc1 <- c()

for (i in 1:length(기사_새터_park_etc1)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- 기사_새터_park_etc1[i] %>% read_html()
    
    #언론사
    언론사_새터.tmp <- body %>% 
      html_nodes("a.link") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_새터.tmp) != 0) {
      언론사_새터_park_etc1 <- append(언론사_새터_park_etc1,언론사_새터.tmp)
    } else {
      언론사_새터_park_etc1 <- append(언론사_새터_park_etc1,"수동확인")
    }    
    
    #제목
    제목_새터.tmp <- body %>% 
      html_nodes("h4.title") %>%
      html_text()
    
    if (length(제목_새터.tmp) != 0) {
      제목_새터_park_etc1 <- append(제목_새터_park_etc1,제목_새터.tmp)
    } else {
      제목_새터_park_etc1 <- append(제목_새터_park_etc1,"수동확인")
    }    
    
    #본문
    본문_새터.tmp <- body %>%
      html_nodes("div#newsEndContents") %>%
      html_text()
    
    if (length(본문_새터.tmp) != 0) {
      본문_새터_park_etc1 <- append(본문_새터_park_etc1,본문_새터.tmp)
    } else {
      본문_새터_park_etc1 <- append(본문_새터_park_etc1,"수동확인")
    }
    
    #날짜
    날짜_새터.tmp <- body %>% 
      html_nodes("div.info") %>%
      html_nodes("span") %>%
      html_text()
    
    날짜_새터.tmp <- 날짜_새터.tmp[1]
    날짜_새터.tmp <- gsub("기사입력","",날짜_새터.tmp)
    
    if (length(날짜_새터.tmp) != 0) {
      날짜_새터_park_etc1 <- append(날짜_새터_park_etc1,날짜_새터.tmp)
    } else {
      날짜_새터_park_etc1 <- append(날짜_새터_park_etc1,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_새터_park_etc1 <- gsub("포토","",제목_새터_park_etc1)
제목_새터_park_etc1 <- gsub("\u5317","북한",제목_새터_park_etc1)
제목_새터_park_etc1 <- gsub("\u97D3","대한민국",제목_새터_park_etc1)
제목_새터_park_etc1 <- gsub("\u65E5","일본",제목_새터_park_etc1)
제목_새터_park_etc1 <- gsub("\u7F8E","미국",제목_새터_park_etc1)
제목_새터_park_etc1 <- gsub("\u4F5B","프랑스",제목_새터_park_etc1)
제목_새터_park_etc1 <- gsub("\u82F1","영국",제목_새터_park_etc1)

새터_park_df_etc1 <- data.frame(언론사_새터_park_etc1,
                                     제목_새터_park_etc1,
                                     본문_새터_park_etc1,
                                     날짜_새터_park_etc1,
                                     기사_새터_park_etc1)

names(새터_park_df_etc1) <- c("언론사_park","제목_park","본문_park","날짜_park","링크_park")


## news.naver가 아닌 기사
기사_새터_park_etc2 <- 새터_park_df %>% 
  filter(제목_park == "수동확인") %>% 
  select("링크_park")

기사_새터_park_etc2 <- 기사_새터_park_etc2$링크_park


# 벡터 생성
언론사_새터_park_etc2 <- c()
제목_새터_park_etc2 <- c()
본문_새터_park_etc2 <- c()
날짜_새터_park_etc2 <- c()

for (i in 1:length(기사_새터_park_etc2)){
  tryCatch({
    
    cat(i, '페이지 수집 중입니다.\n') 
    
    body <- 기사_새터_park_etc2[i] %>% read_html()
    
    #언론사
    언론사_새터.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_새터.tmp) != 0) {
      언론사_새터_park_etc2 <- append(언론사_새터_park_etc2,언론사_새터.tmp)
    } else {
      언론사_새터_park_etc2 <- append(언론사_새터_park_etc2,"수동확인")
    }    
    
    #제목
    제목_새터.tmp <- body %>% 
      html_nodes("h2.end_tit") %>% 
      html_text() # text 추출
    
    if (length(제목_새터.tmp) != 0) {
      제목_새터_park_etc2 <- append(제목_새터_park_etc2,제목_새터.tmp)
    } else {
      제목_새터_park_etc2 <- append(제목_새터_park_etc2,"수동확인")
    }      
    
    #본문
    본문_새터.tmp <- body %>%
      html_nodes("div#articeBody") %>%
      html_text()
    
    if (length(본문_새터.tmp) != 0) {
      본문_새터_park_etc2 <- append(본문_새터_park_etc2,본문_새터.tmp)
    } else {
      본문_새터_park_etc2<- append(본문_새터_park_etc2,"수동확인")
    }
    
    #날짜
    날짜_새터.tmp <- body %>% 
      html_nodes("span.author") %>%
      html_nodes("em") %>%
      html_text()
    
    날짜_새터.tmp <- 날짜_새터.tmp[1]
    
    if (length(날짜_새터.tmp) != 0) {
      날짜_새터_park_etc2 <- append(날짜_새터_park_etc2,날짜_새터.tmp)
    } else {
      날짜_새터_park_etc2 <- append(날짜_새터_park_etc2,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_새터_park_etc2 <- gsub("포토","",제목_새터_park_etc2)
제목_새터_park_etc2 <- gsub("\u5317","북한",제목_새터_park_etc2)
제목_새터_park_etc2 <- gsub("\u97D3","대한민국",제목_새터_park_etc2)
제목_새터_park_etc2 <- gsub("\u65E5","일본",제목_새터_park_etc2)
제목_새터_park_etc2 <- gsub("\u7F8E","미국",제목_새터_park_etc2)
제목_새터_park_etc2 <- gsub("\u4F5B","프랑스",제목_새터_park_etc2)
제목_새터_park_etc2 <- gsub("\u82F1","영국",제목_새터_park_etc2)

새터_park_df_etc2 <- data.frame(언론사_새터_park_etc2,
                                     제목_새터_park_etc2,
                                     본문_새터_park_etc2,
                                     날짜_새터_park_etc2,
                                     기사_새터_park_etc2)

names(새터_park_df_etc2) <- c("언론사_park","제목_park","본문_park","날짜_park","링크_park")

새터_park_df_com <- rbind(새터_park_df,새터_park_df_etc1,새터_park_df_etc2)
새터_park_df_com <- 새터_park_df_com %>% filter(새터_park_df_com$제목_park != "수동확인")

write.csv(새터_park_df_com, file = "C:/대학원/새터_park_df_com.csv", row.names=FALSE)

#본문 제외
새터_park_df_com_본문제외 <- 새터_park_df_com %>% select("언론사_park","제목_park","날짜_park","링크_park")
write.csv(새터_park_df_com_본문제외, file = "C:/대학원/새터_park_df_com_본문제외.csv", row.names=FALSE)

end_time_park_새터 <- Sys.time()



#### moon
start_time_moon_새터 <- Sys.time()

## 링크생성
# 링크에 dd from 붙이기
head <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query="
searchword <- "새터민"
body1 <- "&sort=1&photo=0&field=0&pd=3&ds="
dd_moon <- dd_moon
body2 <- "&de="
dd_moon <- dd_moon
body3 <- "&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:dd,p:from"
fm_moon <- fm_moon 
to <- "to"
fm_moon <- fm_moon 
body4 <- ",a:all&start="
seq <- seq(from = 1, by = 10, length.out = 400)

## 네이버뉴스 링크 수집
# 기사링크 벡터 생성
기사_새터_moon <- c()

# 네이버뉴스 링크 수집
for (i in 1:length(dd_moon)){
  for (j in 1:400){
    tryCatch({
      
      cat(dd_moon[i], j, '페이지 수집 중', '입니다.\n') 
      
      body <- paste0(head,searchword,body1,dd_moon[i],body2,dd_moon[i],body3,fm_moon[i],to,fm_moon[i],body4,seq[j]) %>% read_html()
      
      #기사
      기사_새터.tmp <- body %>% 
        html_nodes("div.info_group") %>%
        html_nodes("a:nth-child(3)") %>%
        html_attr("href") # 링크등 해당 속성의 값을 추출함 
      
      if (length(기사_새터.tmp) != 0) {
        기사_새터_moon <- append(기사_새터_moon,기사_새터.tmp)
      } else { break }    
      
      Sys.sleep(time = 0.01)
      
    }, error = function(e) cat("불러올 수 없습니다!\n"))
  }
}

기사_새터_moon <- 기사_새터_moon %>% unique()
기사_새터_moon %>% length()

news.naver_기사_새터_moon <- grep("https://news.naver.com",기사_새터_moon)
기사_새터_moon_news <- 기사_새터_moon[news.naver_기사_새터_moon] # 최종 기사 링크
기사_새터_moon_etc1 <- 기사_새터_moon[-news.naver_기사_새터_moon] ## sports인지 확인

## 최종 기사 링크 - 최신순 정렬
# 벡터 생성
언론사_새터_moon <- c()
제목_새터_moon <- c()
본문_새터_moon <- c()
날짜_새터_moon <- c()

for (i in 1:length(기사_새터_moon_news)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- 기사_새터_moon_news[i] %>% read_html()
    
    #언론사
    언론사_새터.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_새터.tmp) != 0) {
      언론사_새터_moon <- append(언론사_새터_moon,언론사_새터.tmp)
    } else {
      언론사_새터_moon <- append(언론사_새터_moon,"수동확인")
    }    
    
    #제목
    제목_새터.tmp <- body %>% 
      html_nodes("div.article_info") %>%
      html_nodes("h3") %>%
      html_text() # text 추출
    
    if (length(제목_새터.tmp) != 0) {
      제목_새터_moon <- append(제목_새터_moon,제목_새터.tmp)
    } else {
      제목_새터_moon <- append(제목_새터_moon,"수동확인")
    }      
    
    #본문
    본문_새터.tmp <- body %>%
      html_nodes("div#articleBodyContents") %>%
      html_text()
    
    if (length(본문_새터.tmp) != 0) {
      본문_새터_moon <- append(본문_새터_moon,본문_새터.tmp)
    } else {
      본문_새터_moon <- append(본문_새터_moon,"수동확인")
    }
    
    #날짜
    날짜_새터.tmp <- body %>% 
      html_nodes("span.t11") %>%
      html_text()
    
    날짜_새터.tmp <- 날짜_새터.tmp[1]
    
    if (length(날짜_새터.tmp) != 0) {
      날짜_새터_moon <- append(날짜_새터_moon,날짜_새터.tmp)
    } else {
      날짜_새터_moon <- append(날짜_새터_moon,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_새터_moon <- gsub("포토","",제목_새터_moon)
제목_새터_moon <- gsub("\u5317","북한",제목_새터_moon)
제목_새터_moon <- gsub("\u97D3","대한민국",제목_새터_moon)
제목_새터_moon <- gsub("\u65E5","일본",제목_새터_moon)
제목_새터_moon <- gsub("\u7F8E","미국",제목_새터_moon)
제목_새터_moon <- gsub("\u4F5B","프랑스",제목_새터_moon)
제목_새터_moon <- gsub("\u82F1","영국",제목_새터_moon)

새터_moon_df <- data.frame(언론사_새터_moon,
                                제목_새터_moon,
                                본문_새터_moon,
                                날짜_새터_moon,
                                기사_새터_moon_news)

names(새터_moon_df) <- c("언론사_moon","제목_moon","본문_moon","날짜_moon","링크_moon")


### sports.news 기사
## 페이지 정보 읽기 - 최신순 정렬
# 벡터 생성
언론사_새터_moon_etc1 <- c()
제목_새터_moon_etc1 <- c()
본문_새터_moon_etc1 <- c()
날짜_새터_moon_etc1 <- c()

for (i in 1:length(기사_새터_moon_etc1)){
  tryCatch({
    
    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- 기사_새터_moon_etc1[i] %>% read_html()
    
    #언론사
    언론사_새터.tmp <- body %>% 
      html_nodes("a.link") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_새터.tmp) != 0) {
      언론사_새터_moon_etc1 <- append(언론사_새터_moon_etc1,언론사_새터.tmp)
    } else {
      언론사_새터_moon_etc1 <- append(언론사_새터_moon_etc1,"수동확인")
    }    
    
    #제목
    제목_새터.tmp <- body %>% 
      html_nodes("h4.title") %>%
      html_text()
    
    if (length(제목_새터.tmp) != 0) {
      제목_새터_moon_etc1 <- append(제목_새터_moon_etc1,제목_새터.tmp)
    } else {
      제목_새터_moon_etc1 <- append(제목_새터_moon_etc1,"수동확인")
    }    
    
    #본문
    본문_새터.tmp <- body %>%
      html_nodes("div#newsEndContents") %>%
      html_text()
    
    if (length(본문_새터.tmp) != 0) {
      본문_새터_moon_etc1 <- append(본문_새터_moon_etc1,본문_새터.tmp)
    } else {
      본문_새터_moon_etc1 <- append(본문_새터_moon_etc1,"수동확인")
    }
    
    #날짜
    날짜_새터.tmp <- body %>% 
      html_nodes("div.info") %>%
      html_nodes("span") %>%
      html_text()
    
    날짜_새터.tmp <- 날짜_새터.tmp[1]
    날짜_새터.tmp <- gsub("기사입력","",날짜_새터.tmp)
    
    if (length(날짜_새터.tmp) != 0) {
      날짜_새터_moon_etc1 <- append(날짜_새터_moon_etc1,날짜_새터.tmp)
    } else {
      날짜_새터_moon_etc1 <- append(날짜_새터_moon_etc1,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_새터_moon_etc1 <- gsub("포토","",제목_새터_moon_etc1)
제목_새터_moon_etc1 <- gsub("\u5317","북한",제목_새터_moon_etc1)
제목_새터_moon_etc1 <- gsub("\u97D3","대한민국",제목_새터_moon_etc1)
제목_새터_moon_etc1 <- gsub("\u65E5","일본",제목_새터_moon_etc1)
제목_새터_moon_etc1 <- gsub("\u7F8E","미국",제목_새터_moon_etc1)
제목_새터_moon_etc1 <- gsub("\u4F5B","프랑스",제목_새터_moon_etc1)
제목_새터_moon_etc1 <- gsub("\u82F1","영국",제목_새터_moon_etc1)

새터_moon_df_etc1 <- data.frame(언론사_새터_moon_etc1,
                                     제목_새터_moon_etc1,
                                     본문_새터_moon_etc1,
                                     날짜_새터_moon_etc1,
                                     기사_새터_moon_etc1)

names(새터_moon_df_etc1) <- c("언론사_moon","제목_moon","본문_moon","날짜_moon","링크_moon")


## news.naver가 아닌 기사
기사_새터_moon_etc2 <- 새터_moon_df %>% 
  filter(제목_moon == "수동확인") %>% 
  select("링크_moon")

기사_새터_moon_etc2 <- 기사_새터_moon_etc2$링크_moon


# 벡터 생성
언론사_새터_moon_etc2 <- c()
제목_새터_moon_etc2 <- c()
본문_새터_moon_etc2 <- c()
날짜_새터_moon_etc2 <- c()

for (i in 1:length(기사_새터_moon_etc2)){
  tryCatch({
    
    cat(i, '페이지 수집 중입니다.\n') 
    
    body <- 기사_새터_moon_etc2[i] %>% read_html()
    
    #언론사
    언론사_새터.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_새터.tmp) != 0) {
      언론사_새터_moon_etc2 <- append(언론사_새터_moon_etc2,언론사_새터.tmp)
    } else {
      언론사_새터_moon_etc2 <- append(언론사_새터_moon_etc2,"수동확인")
    }    
    
    #제목
    제목_새터.tmp <- body %>% 
      html_nodes("h2.end_tit") %>% 
      html_text() # text 추출
    
    if (length(제목_새터.tmp) != 0) {
      제목_새터_moon_etc2 <- append(제목_새터_moon_etc2,제목_새터.tmp)
    } else {
      제목_새터_moon_etc2 <- append(제목_새터_moon_etc2,"수동확인")
    }      
    
    #본문
    본문_새터.tmp <- body %>%
      html_nodes("div#articeBody") %>%
      html_text()
    
    if (length(본문_새터.tmp) != 0) {
      본문_새터_moon_etc2 <- append(본문_새터_moon_etc2,본문_새터.tmp)
    } else {
      본문_새터_moon_etc2<- append(본문_새터_moon_etc2,"수동확인")
    }
    
    #날짜
    날짜_새터.tmp <- body %>% 
      html_nodes("span.author") %>%
      html_nodes("em") %>%
      html_text()
    
    날짜_새터.tmp <- 날짜_새터.tmp[1]
    
    if (length(날짜_새터.tmp) != 0) {
      날짜_새터_moon_etc2 <- append(날짜_새터_moon_etc2,날짜_새터.tmp)
    } else {
      날짜_새터_moon_etc2 <- append(날짜_새터_moon_etc2,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

제목_새터_moon_etc2 <- gsub("포토","",제목_새터_moon_etc2)
제목_새터_moon_etc2 <- gsub("\u5317","북한",제목_새터_moon_etc2)
제목_새터_moon_etc2 <- gsub("\u97D3","대한민국",제목_새터_moon_etc2)
제목_새터_moon_etc2 <- gsub("\u65E5","일본",제목_새터_moon_etc2)
제목_새터_moon_etc2 <- gsub("\u7F8E","미국",제목_새터_moon_etc2)
제목_새터_moon_etc2 <- gsub("\u4F5B","프랑스",제목_새터_moon_etc2)
제목_새터_moon_etc2 <- gsub("\u82F1","영국",제목_새터_moon_etc2)

새터_moon_df_etc2 <- data.frame(언론사_새터_moon_etc2,
                                     제목_새터_moon_etc2,
                                     본문_새터_moon_etc2,
                                     날짜_새터_moon_etc2,
                                     기사_새터_moon_etc2)

names(새터_moon_df_etc2) <- c("언론사_moon","제목_moon","본문_moon","날짜_moon","링크_moon")

새터_moon_df_com <- rbind(새터_moon_df,새터_moon_df_etc1,새터_moon_df_etc2)
새터_moon_df_com <- 새터_moon_df_com %>% filter(새터_moon_df_com$제목_moon != "수동확인")

write.csv(새터_moon_df_com, file = "C:/대학원/새터_moon_df_com.csv", row.names=FALSE)

#본문 제외
새터_moon_df_com_본문제외 <- 새터_moon_df_com %>% select("언론사_moon","제목_moon","날짜_moon","링크_moon")
write.csv(새터_moon_df_com_본문제외, file = "C:/대학원/새터_moon_df_com_본문제외.csv", row.names=FALSE)

end_time_moon_새터 <- Sys.time()



## 소요시간
end_time_park_탈북 - start_park_moon_탈북
end_time_moon_탈북 - start_time_moon_탈북
end_time_park_새터 - start_time_park_새터
end_time_moon_새터 - start_time_moon_새터


