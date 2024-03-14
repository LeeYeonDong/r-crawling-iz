
library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)
library(tidyverse)

### NC
## 링크생성
# dd생성
start_time1 <- Sys.time()

yyyy20 <- "2020"
yyyy21 <- "2021"

mm20 <- c("05","06","07","08","09","10","11","12")
mm21 <- c("01","02","03")

dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14",
        "15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

dd20 <- c()

for (j in 1:length(dd)){
  for (i in 1:length(mm20)){
    dd20.tmp <- paste0(yyyy20,".",mm20[i],".",dd[j])
    dd20 <- append(dd20,dd20.tmp)
  }
}

dd21 <- c()

for (j in 1:length(dd)){
  for (i in 1:length(mm21)){
    dd21.tmp <- paste0(yyyy21,".",mm21[i],".",dd[j])
    dd21 <- append(dd21,dd21.tmp)
  }
}

dd2021 <- append(dd20,dd21)


# from생성
from2021 <- gsub("\\.","",dd2021)

head <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query="
searchword <- "NC"
body1 <- "&sort=1&photo=0&field=0&pd=3&ds="
dd2021 <- dd2021
body2 <- "&de="
dd2021 <- dd2021
body3 <- "&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:dd,p:from"
from2021 <- from2021
to <- "to"
from2021 <- from2021
body4 <- ",a:all&start="
seq <- seq(from = 1, by = 10, length.out = 400)

### 네이버뉴스링크-독자반응포함
### 기사링크 벡터 생성
링크_NC <- c()

## 네이버뉴스 링크 수집
for (i in 1:length(dd2021)){
  for (j in 1:400){
    tryCatch({
      
      cat(dd2021[i], j, '페이지 수집 중', '입니다.\n') 
    
    body <- paste0(head,searchword,body1,dd2021[i],body2,dd2021[i],body3,from2021[i],to,from2021[i],body4,seq[j]) %>% read_html()
    
    #링크
    링크_NC.tmp <- body %>% 
      html_nodes("div.info_group") %>%
      html_nodes("a:nth-child(3)") %>%
      html_attr("href")
    
    if (length(링크_NC.tmp) != 0) {
      링크_NC <- append(링크_NC,링크_NC.tmp)
    } else { break }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  }
}

링크_NC <- 링크_NC %>% unique()


### sports.news 벡터 생성
언론사_sports.news_NC <- c()
제목_sports.news_NC <- c()
본문_sports.news_NC <- c()
날짜_sports.news_NC <- c()

좋아_sports.news_NC <- c()
훈훈_sports.news_NC <- c()
슬퍼_sports.news_NC <- c()
화나_sports.news_NC <- c()
후속_sports.news_NC <- c()


### sports.news 기사
## 페이지 정보 읽기 - 최신순 정렬
sports.news_NC <- grep("https://sports.news",링크_NC)
기사_sports.news_NC <- 링크_NC[sports.news_NC]

# 링크 분할
기사_sports.news_NC_sp <- strsplit(기사_sports.news_NC, split="=")  

기사_sports.news_NC_aid <- c()
기사_sports.news_NC_oid <- c()

for (i in 1:length(기사_sports.news_NC)){
  기사_NC_park_sp_oid.tmp <- 기사_sports.news_NC_sp[[i]][2]
  기사_NC_park_sp_aid.tmp <- 기사_sports.news_NC_sp[[i]][3]
  
  기사_sports.news_NC_oid <- append(기사_sports.news_NC_oid,기사_NC_park_sp_oid.tmp)
  기사_sports.news_NC_aid <- append(기사_sports.news_NC_aid,기사_NC_park_sp_aid.tmp)
}

기사_sports.news_NC_oid <- gsub("&aid","",기사_sports.news_NC_oid)


## 페이지 정보 읽기 - 최신순 정렬

for (i in 1:length(기사_sports.news_NC)){
  tryCatch({
    res_sports.news_NC <- GET(url = 'https://sports.news.naver.com/news.nhn',
                              query = list(
                                oid = 기사_sports.news_NC_oid[i],
                                aid = 기사_sports.news_NC_aid[i]))
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_sports.news_NC), '입니다.\n') 
    
    body <- res_sports.news_NC %>% read_html()
    
    #언론사
    언론사_NC.tmp <- body %>% 
      html_nodes("a.link") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_NC.tmp) != 0) {
      언론사_sports.news_NC <- append(언론사_sports.news_NC,언론사_NC.tmp)
    } else {
      언론사_sports.news_NC <- append(언론사_sports.news_NC,"수동확인")
    }    
    
    #제목
    제목_NC.tmp <- body %>% 
      html_nodes("h4.title") %>%
      html_text()
    
    if (length(제목_NC.tmp) != 0) {
      제목_sports.news_NC <- append(제목_sports.news_NC,제목_NC.tmp)
    } else {
      제목_sports.news_NC <- append(제목_sports.news_NC,"수동확인")
    }    
    
    # #본문
    # 본문_NC.tmp <- body %>%
    #   html_nodes("div#newsEndContents") %>%
    #   html_text()
    # 
    # if (length(본문_NC.tmp) != 0) {
    #   본문_sports.news_NC <- append(본문_sports.news_NC,본문_NC.tmp)
    # } else {
    #   본문_sports.news_NC <- append(본문_sports.news_NC,"수동확인")
    # }
    
    #날짜
    날짜_NC.tmp <- body %>% 
      html_nodes("div.info") %>%
      html_nodes("span") %>%
      html_text()
    
    날짜_NC.tmp <- 날짜_NC.tmp[1]
    날짜_NC.tmp <- gsub("기사입력","",날짜_NC.tmp)
    
    if (length(날짜_NC.tmp) != 0) {
      날짜_sports.news_NC <- append(날짜_sports.news_NC,날짜_NC.tmp)
    } else {
      날짜_sports.news_NC <- append(날짜_sports.news_NC,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

end_time1 <- Sys.time()

## 반응_sports.news

##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

# RSelenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

start_time2 <- Sys.time()

# RSelenium
for (i in 1:length(링크_sports.news_NC)){
  tryCatch({
    remDr$navigate(링크_sports.news_NC[i])
    body <- remDr$getPageSource()[[1]]
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    body <- body %>% read_html()
    
    반응_NC.tmp <- body %>%  
      html_nodes("a.u_likeit_list_button") %>%
      html_nodes("span:nth-child(2)")%>%
      html_text()
    
    좋아_sports.news_NC <- append(좋아_sports.news_NC, 반응_NC.tmp[1])
    훈훈_sports.news_NC <- append(훈훈_sports.news_NC, 반응_NC.tmp[2])
    슬퍼_sports.news_NC <- append(슬퍼_sports.news_NC, 반응_NC.tmp[3])
    화나_sports.news_NC <- append(화나_sports.news_NC, 반응_NC.tmp[4])
    후속_sports.news_NC <- append(후속_sports.news_NC, 반응_NC.tmp[5])
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

뉴스_sports.news_NC <- data.frame(언론사_sports.news_NC,
                                 제목_sports.news_NC,
                                 날짜_sports.news_NC,
                                 링크_sports.news_NC,
                                 좋아_sports.news_NC,
                                 훈훈_sports.news_NC,
                                 슬퍼_sports.news_NC,
                                 화나_sports.news_NC,
                                 후속_sports.news_NC)

end_time2 <- Sys.time()

end_time1 - start_time1
end_time2 - start_time2

## 데이터 프레임 저장
write.csv(뉴스_sports.news_NC, file = "C:/대학원/논문/소논문/뉴스_sports.news_NC.csv", row.names=FALSE)




### LG
## 링크생성
# dd생성
start_time1 <- Sys.time()

yyyy20 <- "2020"
yyyy21 <- "2021"

mm20 <- c("05","06","07","08","09","10","11","12")
mm21 <- c("01","02","03")

dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

dd20 <- c()

for (j in 1:length(dd)){
  for (i in 1:length(mm20)){
    dd20.tmp <- paste0(yyyy20,".",mm20[i],".",dd[j])
    dd20 <- append(dd20,dd20.tmp)
  }
}

dd21 <- c()

for (j in 1:length(dd)){
  for (i in 1:length(mm21)){
    dd21.tmp <- paste0(yyyy21,".",mm21[i],".",dd[j])
    dd21 <- append(dd21,dd21.tmp)
  }
}

dd2021 <- append(dd20,dd21)

# from생성
from2021 <- gsub("\\.","",dd2021)

head <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query="
searchword <- "LG"
body1 <- "&sort=1&photo=0&field=0&pd=3&ds="
dd2021 <- dd2021
body2 <- "&de="
dd2021 <- dd2021
body3 <- "&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:dd,p:from"
from2021 <- from2021
to <- "to"
from2021 <- from2021
body4 <- ",a:all&start="
seq <- seq(from = 1, by = 10, length.out = 400)

### 네이버뉴스링크-독자반응포함
### 기사링크 벡터 생성
링크_LG <- c()

## 네이버뉴스 링크 수집
for (i in 1:length(dd2021)){
  for (j in 1:400){
    tryCatch({
      
      cat(dd2021[i], j, '페이지 수집 중', '입니다.\n') 
      
      body <- paste0(head,searchword,body0,body1,dd2021[i],body2,dd2021[i],body3,from2021[i],to,from2021[i],body5,seq[j],body6) %>% read_html()
      
      #링크
      링크_LG.tmp <- body %>% 
        html_nodes("div.info_group") %>%
        html_nodes("a:nth-child(3)") %>%
        html_attr("href")
      
      if (length(링크_LG.tmp) != 0) {
        링크_LG <- append(링크_LG,링크_LG.tmp)
      } else { break }    
      
      Sys.sleep(time = 0.01)
      
    }, error = function(e) cat("불러올 수 없습니다!\n"))
  }
}

링크_LG <- 링크_LG %>% unique()


### sports.news 벡터 생성
언론사_sports.news_LG <- c()
제목_sports.news_LG <- c()
날짜_sports.news_LG <- c()
본문_sports.news_LG <- c()

좋아_sports.news_LG <- c()
훈훈_sports.news_LG <- c()
슬퍼_sports.news_LG <- c()
화나_sports.news_LG <- c()
후속_sports.news_LG <- c()

## 페이지 정보 읽기 - 최신순 정렬

for (i in 1:length(기사_sports.news_LG)){
  tryCatch({
    res_sports.news_LG <- GET(url = 'https://sports.news.naver.com/news.nhn',
                              query = list(
                                oid = 기사_sports.news_LG_oid[i],
                                aid = 기사_sports.news_LG_aid[i]))
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_sports.news_LG), '입니다.\n') 
    
    body <- res_sports.news_LG %>% read_html()
    
    #언론사
    언론사_LG.tmp <- body %>% 
      html_nodes("a.link") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_LG.tmp) != 0) {
      언론사_sports.news_LG <- append(언론사_sports.news_LG,언론사_LG.tmp)
    } else {
      언론사_sports.news_LG <- append(언론사_sports.news_LG,"수동확인")
    }    
    
    #제목
    제목_LG.tmp <- body %>% 
      html_nodes("h4.title") %>%
      html_text()
    
    if (length(제목_LG.tmp) != 0) {
      제목_sports.news_LG <- append(제목_sports.news_LG,제목_LG.tmp)
    } else {
      제목_sports.news_LG <- append(제목_sports.news_LG,"수동확인")
    }    
    
    # #본문
    # 본문_LG.tmp <- body %>%
    #   html_nodes("div#newsEndContents") %>%
    #   html_text()
    # 
    # if (length(본문_LG.tmp) != 0) {
    #   본문_sports.news_LG <- append(본문_sports.news_LG,본문_LG.tmp)
    # } else {
    #   본문_sports.news_LG <- append(본문_sports.news_LG,"수동확인")
    # }
    
    #날짜
    날짜_LG.tmp <- body %>% 
      html_nodes("div.info") %>%
      html_nodes("span") %>%
      html_text()
    
    날짜_LG.tmp <- 날짜_LG.tmp[1]
    날짜_LG.tmp <- gsub("기사입력","",날짜_LG.tmp)
    
    if (length(날짜_LG.tmp) != 0) {
      날짜_sports.news_LG <- append(날짜_sports.news_LG,날짜_LG.tmp)
    } else {
      날짜_sports.news_LG <- append(날짜_sports.news_LG,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}




end_time1 <- Sys.time()

## 반응_sports.news

##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

# RSelenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

# RSelenium
for (i in 1:length(링크_sports.news_LG)){
  tryCatch({
    remDr$navigate(링크_sports.news_LG[i])
    body <- remDr$getPageSource()[[1]]
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    body <- body %>% read_html()
    
    반응_LG.tmp <- body %>%  
      html_nodes("a.u_likeit_list_button") %>%
      html_nodes("span:nth-child(2)")%>%
      html_text()
    
    좋아_sports.news_LG <- append(좋아_sports.news_LG, 반응_LG.tmp[1])
    훈훈_sports.news_LG <- append(훈훈_sports.news_LG, 반응_LG.tmp[2])
    슬퍼_sports.news_LG <- append(슬퍼_sports.news_LG, 반응_LG.tmp[3])
    화나_sports.news_LG <- append(화나_sports.news_LG, 반응_LG.tmp[4])
    후속_sports.news_LG <- append(후속_sports.news_LG, 반응_LG.tmp[5])
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

뉴스_sports.news_LG <- data.frame(언론사_sports.news_LG,
                                    제목_sports.news_LG,
                                    날짜_sports.news_LG,
                                    링크_sports.news_LG,
                                    좋아_sports.news_LG,
                                    훈훈_sports.news_LG,
                                    슬퍼_sports.news_LG,
                                    화나_sports.news_LG,
                                    후속_sports.news_LG)

end_time2 <- Sys.time()

end_time1 - start_time1
end_time2 - start_time1 

## 데이터 프레임 저장
write.csv(뉴스_sports.news_LG, file = "C:/대학원/논문/소논문/뉴스_sports.news_LG.csv", row.names=FALSE)



### 두산
## 링크생성
# dd생성
start_time1 <- Sys.time()

yyyy20 <- "2020"
yyyy21 <- "2021"

mm20 <- c("05","06","07","08","09","10","11","12")
mm21 <- c("01","02","03")

dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

dd20 <- c()

for (j in 1:length(dd)){
  for (i in 1:length(mm20)){
    dd20.tmp <- paste0(yyyy20,".",mm20[i],".",dd[j])
    dd20 <- append(dd20,dd20.tmp)
  }
}

dd21 <- c()

for (j in 1:length(dd)){
  for (i in 1:length(mm21)){
    dd21.tmp <- paste0(yyyy21,".",mm21[i],".",dd[j])
    dd21 <- append(dd21,dd21.tmp)
  }
}

dd2021 <- append(dd20,dd21)


# from생성
mm20 <- c("05","06","07","08","09","10","11","12")
mm21 <- c("01","02","03")

dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

from20 <- c()

for (j in 1:length(dd)){
  for (i in 1:length(mm20)){
    from20.tmp <- paste0(yyyy20,mm20[i],dd[j])
    from20 <- append(from20,from20.tmp)
  }
}

from21 <- c()

for (j in 1:length(dd)){
  for (i in 1:length(mm21)){
    from21.tmp <- paste0(yyyy21,mm21[i],dd[j])
    from21 <- append(from21,from21.tmp)
  }
}

from2021 <- append(from20,from21)

head <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query="
searchword <- "두산"
body1 <- "&sort=1&photo=0&field=0&pd=3&ds="
dd2021 <- dd2021
body2 <- "&de="
dd2021 <- dd2021
body3 <- "&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:dd,p:from"
from2021 <- from2021
to <- "to"
from2021 <- from2021
body4 <- ",a:all&start="
seq <- seq(from = 1, by = 10, length.out = 400)

### 네이버뉴스링크-독자반응포함
### 기사링크 벡터 생성
링크_두산 <- c()

## 네이버뉴스 링크 수집
for (i in 1:length(dd2021)){
  for (j in 1:400){
    tryCatch({
      
      cat(dd2021[i], j, '페이지 수집 중', '입니다.\n') 
      
      body <- paste0(head,searchword,body0,body1,dd2021[i],body2,dd2021[i],body3,from2021[i],to,from2021[i],body5,seq[j],body6) %>% read_html()
      
      #링크
      링크_두산.tmp <- body %>% 
        html_nodes("div.info_group") %>%
        html_nodes("a:nth-child(3)") %>%
        html_attr("href")
      
      if (length(링크_두산.tmp) != 0) {
        링크_두산 <- append(링크_두산,링크_두산.tmp)
      } else { break }    
      
      Sys.sleep(time = 0.01)
      
    }, error = function(e) cat("불러올 수 없습니다!\n"))
  }
}

링크_두산 <- 링크_두산 %>% unique()


### sports.news 벡터 생성
언론사_sports.news_두산 <- c()
제목_sports.news_두산 <- c()
날짜_sports.news_두산 <- c()
본문_sports.news_두산 <- c()

좋아_sports.news_두산 <- c()
훈훈_sports.news_두산 <- c()
슬퍼_sports.news_두산 <- c()
화나_sports.news_두산 <- c()
후속_sports.news_두산 <- c()


## 페이지 정보 읽기 - 최신순 정렬
sports.news_두산 <- grep("https://sports.news",링크_두산)
링크_sports.news_두산 <- 링크_두산[sports.news_두산]


## 페이지 정보 읽기 - 최신순 정렬

for (i in 1:length(기사_sports.news_두산)){
  tryCatch({
    res_sports.news_두산 <- GET(url = 'https://sports.news.naver.com/news.nhn',
                              query = list(
                                oid = 기사_sports.news_두산_oid[i],
                                aid = 기사_sports.news_두산_aid[i]))
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_sports.news_두산), '입니다.\n') 
    
    body <- res_sports.news_두산 %>% read_html()
    
    #언론사
    언론사_두산.tmp <- body %>% 
      html_nodes("a.link") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_두산.tmp) != 0) {
      언론사_sports.news_두산 <- append(언론사_sports.news_두산,언론사_두산.tmp)
    } else {
      언론사_sports.news_두산 <- append(언론사_sports.news_두산,"수동확인")
    }    
    
    #제목
    제목_두산.tmp <- body %>% 
      html_nodes("h4.title") %>%
      html_text()
    
    if (length(제목_두산.tmp) != 0) {
      제목_sports.news_두산 <- append(제목_sports.news_두산,제목_두산.tmp)
    } else {
      제목_sports.news_두산 <- append(제목_sports.news_두산,"수동확인")
    }    
    
    # #본문
    # 본문_두산.tmp <- body %>%
    #   html_nodes("div#newsEndContents") %>%
    #   html_text()
    # 
    # if (length(본문_두산.tmp) != 0) {
    #   본문_sports.news_두산 <- append(본문_sports.news_두산,본문_두산.tmp)
    # } else {
    #   본문_sports.news_두산 <- append(본문_sports.news_두산,"수동확인")
    # }
    
    #날짜
    날짜_두산.tmp <- body %>% 
      html_nodes("div.info") %>%
      html_nodes("span") %>%
      html_text()
    
    날짜_두산.tmp <- 날짜_두산.tmp[1]
    날짜_두산.tmp <- gsub("기사입력","",날짜_두산.tmp)
    
    if (length(날짜_두산.tmp) != 0) {
      날짜_sports.news_두산 <- append(날짜_sports.news_두산,날짜_두산.tmp)
    } else {
      날짜_sports.news_두산 <- append(날짜_sports.news_두산,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

end_time1 <- Sys.time()

## 반응_sports.news

##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

# RSelenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

# RSelenium
for (i in 1:length(링크_sports.news_두산)){
  tryCatch({
    remDr$navigate(링크_sports.news_두산[i])
    body <- remDr$getPageSource()[[1]]
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    body <- body %>% read_html()
    
    반응_두산.tmp <- body %>%  
      html_nodes("a.u_likeit_list_button") %>%
      html_nodes("span:nth-child(2)")%>%
      html_text()
    
    좋아_sports.news_두산 <- append(좋아_sports.news_두산, 반응_두산.tmp[1])
    훈훈_sports.news_두산 <- append(훈훈_sports.news_두산, 반응_두산.tmp[2])
    슬퍼_sports.news_두산 <- append(슬퍼_sports.news_두산, 반응_두산.tmp[3])
    화나_sports.news_두산 <- append(화나_sports.news_두산, 반응_두산.tmp[4])
    후속_sports.news_두산 <- append(후속_sports.news_두산, 반응_두산.tmp[5])
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

뉴스_sports.news_두산 <- data.frame(언론사_sports.news_두산,
                                    제목_sports.news_두산,
                                    날짜_sports.news_두산,
                                    링크_sports.news_두산,
                                    좋아_sports.news_두산,
                                    훈훈_sports.news_두산,
                                    슬퍼_sports.news_두산,
                                    화나_sports.news_두산,
                                    후속_sports.news_두산)

end_time2 <- Sys.time()

end_time1 - start_time1
end_time2 - start_time1 

## 데이터 프레임 저장
write.csv(뉴스_sports.news_두산, file = "C:/대학원/논문/소논문/뉴스_sports.news_두산.csv", row.names=FALSE)




### 롯데
## 링크생성
# dd생성
start_time1 <- Sys.time()

yyyy20 <- "2020"
yyyy21 <- "2021"

mm20 <- c("05","06","07","08","09","10","11","12")
mm21 <- c("01","02","03")

dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

dd20 <- c()

for (j in 1:length(dd)){
  for (i in 1:length(mm20)){
    dd20.tmp <- paste0(yyyy20,".",mm20[i],".",dd[j])
    dd20 <- append(dd20,dd20.tmp)
  }
}

dd21 <- c()

for (j in 1:length(dd)){
  for (i in 1:length(mm21)){
    dd21.tmp <- paste0(yyyy21,".",mm21[i],".",dd[j])
    dd21 <- append(dd21,dd21.tmp)
  }
}

dd2021 <- append(dd20,dd21)


# from생성
mm20 <- c("05","06","07","08","09","10","11","12")
mm21 <- c("01","02","03")

dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

from20 <- c()

for (j in 1:length(dd)){
  for (i in 1:length(mm20)){
    from20.tmp <- paste0(yyyy20,mm20[i],dd[j])
    from20 <- append(from20,from20.tmp)
  }
}

from21 <- c()

for (j in 1:length(dd)){
  for (i in 1:length(mm21)){
    from21.tmp <- paste0(yyyy21,mm21[i],dd[j])
    from21 <- append(from21,from21.tmp)
  }
}

from2021 <- append(from20,from21)

head <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query="
searchword <- "롯데"
body1 <- "&sort=1&photo=0&field=0&pd=3&ds="
dd2021 <- dd2021
body2 <- "&de="
dd2021 <- dd2021
body3 <- "&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:dd,p:from"
from2021 <- from2021
to <- "to"
from2021 <- from2021
body4 <- ",a:all&start="
seq <- seq(from = 1, by = 10, length.out = 400)

### 네이버뉴스링크-독자반응포함
### 기사링크 벡터 생성
링크_롯데 <- c()

## 네이버뉴스 링크 수집
for (i in 1:length(dd2021)){
  for (j in 1:400){
    tryCatch({
      
      cat(dd2021[i], j, '페이지 수집 중', '입니다.\n') 
      
      body <- paste0(head,searchword,body0,body1,dd2021[i],body2,dd2021[i],body3,from2021[i],to,from2021[i],body5,seq[j],body6) %>% read_html()
      
      #링크
      링크_롯데.tmp <- body %>% 
        html_nodes("div.info_group") %>%
        html_nodes("a:nth-child(3)") %>%
        html_attr("href")
      
      if (length(링크_롯데.tmp) != 0) {
        링크_롯데 <- append(링크_롯데,링크_롯데.tmp)
      } else { break }    
      
      Sys.sleep(time = 0.01)
      
    }, error = function(e) cat("불러올 수 없습니다!\n"))
  }
}

링크_롯데 <- 링크_롯데 %>% unique()


### sports.news 벡터 생성
언론사_sports.news_롯데 <- c()
제목_sports.news_롯데 <- c()
날짜_sports.news_롯데 <- c()
본문_sports.news_롯데 <- c()

좋아_sports.news_롯데 <- c()
훈훈_sports.news_롯데 <- c()
슬퍼_sports.news_롯데 <- c()
화나_sports.news_롯데 <- c()
후속_sports.news_롯데 <- c()


## 페이지 정보 읽기 - 최신순 정렬
sports.news_롯데 <- grep("https://sports.news",링크_롯데)
링크_sports.news_롯데 <- 링크_롯데[sports.news_롯데]


## 페이지 정보 읽기 - 최신순 정렬

for (i in 1:length(기사_sports.news_롯데)){
  tryCatch({
    res_sports.news_롯데 <- GET(url = 'https://sports.news.naver.com/news.nhn',
                              query = list(
                                oid = 기사_sports.news_롯데_oid[i],
                                aid = 기사_sports.news_롯데_aid[i]))
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_sports.news_롯데), '입니다.\n') 
    
    body <- res_sports.news_롯데 %>% read_html()
    
    #언론사
    언론사_롯데.tmp <- body %>% 
      html_nodes("a.link") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_롯데.tmp) != 0) {
      언론사_sports.news_롯데 <- append(언론사_sports.news_롯데,언론사_롯데.tmp)
    } else {
      언론사_sports.news_롯데 <- append(언론사_sports.news_롯데,"수동확인")
    }    
    
    #제목
    제목_롯데.tmp <- body %>% 
      html_nodes("h4.title") %>%
      html_text()
    
    if (length(제목_롯데.tmp) != 0) {
      제목_sports.news_롯데 <- append(제목_sports.news_롯데,제목_롯데.tmp)
    } else {
      제목_sports.news_롯데 <- append(제목_sports.news_롯데,"수동확인")
    }    
    
    # #본문
    # 본문_롯데.tmp <- body %>%
    #   html_nodes("div#newsEndContents") %>%
    #   html_text()
    # 
    # if (length(본문_롯데.tmp) != 0) {
    #   본문_sports.news_롯데 <- append(본문_sports.news_롯데,본문_롯데.tmp)
    # } else {
    #   본문_sports.news_롯데 <- append(본문_sports.news_롯데,"수동확인")
    # }
    
    #날짜
    날짜_롯데.tmp <- body %>% 
      html_nodes("div.info") %>%
      html_nodes("span") %>%
      html_text()
    
    날짜_롯데.tmp <- 날짜_롯데.tmp[1]
    날짜_롯데.tmp <- gsub("기사입력","",날짜_롯데.tmp)
    
    if (length(날짜_롯데.tmp) != 0) {
      날짜_sports.news_롯데 <- append(날짜_sports.news_롯데,날짜_롯데.tmp)
    } else {
      날짜_sports.news_롯데 <- append(날짜_sports.news_롯데,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}


end_time1 <- Sys.time()

## 반응_sports.news

##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

# RSelenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

# RSelenium
for (i in 1:length(링크_sports.news_롯데)){
  tryCatch({
    remDr$navigate(링크_sports.news_롯데[i])
    body <- remDr$getPageSource()[[1]]
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    body <- body %>% read_html()
    
    반응_롯데.tmp <- body %>%  
      html_nodes("a.u_likeit_list_button") %>%
      html_nodes("span:nth-child(2)")%>%
      html_text()
    
    좋아_sports.news_롯데 <- append(좋아_sports.news_롯데, 반응_롯데.tmp[1])
    훈훈_sports.news_롯데 <- append(훈훈_sports.news_롯데, 반응_롯데.tmp[2])
    슬퍼_sports.news_롯데 <- append(슬퍼_sports.news_롯데, 반응_롯데.tmp[3])
    화나_sports.news_롯데 <- append(화나_sports.news_롯데, 반응_롯데.tmp[4])
    후속_sports.news_롯데 <- append(후속_sports.news_롯데, 반응_롯데.tmp[5])
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

뉴스_sports.news_롯데 <- data.frame(언론사_sports.news_롯데,
                                    제목_sports.news_롯데,
                                    날짜_sports.news_롯데,
                                    링크_sports.news_롯데,
                                    좋아_sports.news_롯데,
                                    훈훈_sports.news_롯데,
                                    슬퍼_sports.news_롯데,
                                    화나_sports.news_롯데,
                                    후속_sports.news_롯데)

end_time2 <- Sys.time()

end_time1 - start_time1
end_time2 - start_time1 

## 데이터 프레임 저장
write.csv(뉴스_sports.news_롯데, file = "C:/대학원/논문/소논문/뉴스_sports.news_롯데.csv", row.names=FALSE)