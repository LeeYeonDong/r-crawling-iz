library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)

start_time1 <- Sys.time()

## 링크생성
# dd생성
yyyy20 <- "2020"
yyyy21 <- "2021"

mm20 <- c("01","02","03","04","05","06","07","08","09","10","11","12")
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


# mm생성
mm20 <- c("01","02","03","04","05","06","07","08","09","10","11","12")
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


head <- "https://search.naver.com/search.naver?where=news&query="
searchword <- "축구"
body0 <- "&sm=tab_srt&sort=0&photo=0&field=0&reporter_article=&pd=3"
body1 <- "&ds="
dd2021 <- dd2021
body2 <- "&de="
dd2021 <- dd2021
body3 <- "&docid=&nso=so%3Ar%2Cp%3Afrom"
from2021 <- from2021
to <- "to"
from2021 <- from2021
body4 <- ",a:all&mynews=0&cluster_rank=20"
body5 <- "&start="
seq <- seq(from = 1, by = 10, length.out = n)
body6 <- "&refresh_start=0"


navi_축구 <- c()

for (i in 1:length(dd2021)){
  for (j in 1:length(seq)){
    navi_축구.tmp <- paste0(head,searchword,body0,body1,dd2021[i],body2,dd2021[i],body3,from2021[i],to,from2021[i],body5,seq[j],body6)
    navi_축구 <- append(navi_축구,navi_축구.tmp)
  }
}

### 네이버뉴스링크-독자반응포함
### 기사링크 벡터 생성
링크_축구 <- c()

## 네이버뉴스 링크 수집
for (i in 1:length(navi_축구)){
  tryCatch({
    cat(i, '페이지 수집 중', '입니다.\n') 
    
    body <- navi_축구[i] %>% read_html()
    
    #링크
    링크_축구.tmp <- body %>% 
      html_nodes("div.info_group") %>%
      html_nodes("a:nth-child(3)") %>%
      html_attr("href")
    
    if (length(링크_축구.tmp) != 0) {
      링크_축구 <- append(링크_축구,링크_축구.tmp)
    } else {
      링크_축구 <- append(링크_축구,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}

링크_축구 <- 링크_축구 %>% unique()

### news.naver 링크
news.naver_축구 <- grep("https://news.naver.com",링크_축구)
링크_news.naver_축구 <- 링크_축구[news.naver_축구]

### news.naver 벡터 생성
언론사_news.naver_축구 <- c()
제목_news.naver_축구 <- c()
날짜_news.naver_축구 <- c()

좋아_news.naver_축구 <- c()
훈훈_news.naver_축구 <- c()
슬퍼_news.naver_축구 <- c()
화나_news.naver_축구 <- c()
후속_news.naver_축구 <- c()

## 링크 분할
링크_news.naver_sp_축구 <- strsplit(링크_news.naver_축구, split="&")  

링크_news.naver.oid_축구 <- c()
링크_news.naver.aid_축구 <- c()

for (i in 1:length(링크_news.naver_축구)){
  링크_news.naver_sp1_축구.tmp <- 링크_news.naver_sp_축구[[i]][4]
  링크_news.naver_sp2_축구.tmp <- 링크_news.naver_sp_축구[[i]][5]
  
  링크_news.naver.oid_축구 <- append(링크_news.naver.oid_축구,링크_news.naver_sp1_축구.tmp)
  링크_news.naver.aid_축구 <- append(링크_news.naver.aid_축구,링크_news.naver_sp2_축구.tmp)
}

링크_news.naver.oid_축구 <- str_replace_all(링크_news.naver.oid_축구,"oid=","")
링크_news.naver.aid_축구 <- str_replace_all(링크_news.naver.aid_축구,"aid=","")

## 페이지 정보 읽기 - 최신순 정렬
for (i in 1:length(링크_news.naver_축구)){
  tryCatch({
    res_news.naver_축구 <- GET(url = 'https://news.naver.com/main/read.nhn',
                             query = list(
                               oid = 링크_news.naver.oid_축구[i],
                               aid = 링크_news.naver.aid_축구[i]))
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_news.naver_축구), '입니다.\n') 
    
    body <- res_news.naver_축구 %>% read_html()
    
    #언론사
    언론사_축구.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_축구.tmp) != 0) {
      언론사_news.naver_축구 <- append(언론사_news.naver_축구,언론사_축구.tmp)
    } else {
      언론사_news.naver_축구 <- append(언론사_news.naver_축구,"수동확인")
    }    
    
    #제목
    제목_축구.tmp <- body %>% 
      html_nodes("div.article_info") %>%
      html_nodes("h3") %>%
      html_text()
    
    if (length(제목_축구.tmp) != 0) {
      제목_news.naver_축구 <- append(제목_news.naver_축구,제목_축구.tmp)
    } else {
      제목_news.naver_축구 <- append(제목_news.naver_축구,"수동확인")
    }    
    
    #날짜
    날짜_축구.tmp <- body %>% 
      html_nodes("span.t11") %>%
      html_text()
    
    날짜_축구.tmp <- 날짜_축구.tmp[1]
    
    if (length(날짜.tmp) != 0) {
      날짜_news.naver_축구 <- append(날짜_news.naver_축구,날짜_축구.tmp)
    } else {
      날짜_news.naver_축구 <- append(날짜_news.naver_축구,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}


### sports.news 링크
sports.news_축구 <- grep("https://sports.news",링크_축구)
링크_sports.news_축구 <- 링크_축구[sports.news_축구]

### sports.news 벡터 생성
언론사_sports.news_축구 <- c()
제목_sports.news_축구 <- c()
날짜_sports.news_축구 <- c()

좋아_sports.news_축구 <- c()
훈훈_sports.news_축구 <- c()
슬퍼_sports.news_축구 <- c()
화나_sports.news_축구 <- c()
후속_sports.news_축구 <- c()

## 링크 분할
링크_sports.news_sp_축구 <- strsplit(링크_sports.news_축구, split="=")  
링크_sports.news.oid_축구 <- c()
링크_sports.news.aid_축구 <- c()

for (i in 1:length(링크_sports.news_축구)){
  링크_sports.news_sp1_축구.tmp <- 링크_sports.news_sp_축구[[i]][2]
  링크_sports.news_sp2_축구.tmp <- 링크_sports.news_sp_축구[[i]][3]
  
  링크_sports.news.oid_축구 <- append(링크_sports.news.oid_축구,링크_sports.news_sp1_축구.tmp)
  링크_sports.news.aid_축구 <- append(링크_sports.news.aid_축구,링크_sports.news_sp2_축구.tmp)
}

링크_sports.news.oid_축구 <- str_replace_all(링크_sports.news.oid_축구,"&aid","")


## 페이지 정보 읽기 - 최신순 정렬
for (i in 1:length(링크_sports.news_축구)){
  tryCatch({
    res_sports.news_축구 <- GET(url = 'https://sports.news.naver.com/news.nhn',
                              query = list(
                                oid = 링크_sports.news.oid_축구[i],
                                aid = 링크_sports.news.aid_축구[i]))
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_sports.news_축구), '입니다.\n') 
    
    body <- res_sports.news_축구 %>% read_html()
    
    #언론사
    언론사_축구.tmp <- body %>% 
      html_nodes("a.link") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_축구.tmp) != 0) {
      언론사_sports.news_축구 <- append(언론사_sports.news_축구,언론사_축구.tmp)
    } else {
      언론사_sports.news_축구 <- append(언론사_sports.news_축구,"수동확인")
    }    
    
    #제목
    제목_축구.tmp <- body %>% 
      html_nodes("h4.title") %>%
      html_text()
    
    if (length(제목_축구.tmp) != 0) {
      제목_sports.news_축구 <- append(제목_sports.news_축구,제목_축구.tmp)
    } else {
      제목_sports.news_축구 <- append(제목_sports.news_축구,"수동확인")
    }    
    
    
    #날짜
    날짜_축구.tmp <- body %>% 
      html_nodes("div.info") %>%
      html_nodes("span") %>%
      html_text()
    
    날짜_축구.tmp <- 날짜_축구.tmp[1]
    날짜_축구.tmp <- gsub("기사입력","",날짜_축구.tmp)
    
    if (length(날짜.tmp) != 0) {
      날짜_sports.news_축구 <- append(날짜_sports.news_축구,날짜_축구.tmp)
    } else {
      날짜_sports.news_축구 <- append(날짜_sports.news_축구,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

end_time1 <- Sys.time()


##########################################


## 반응_news.naver

##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

# RSelenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

start_time2 <- Sys.time()

for (i in 1:length(링크_news.naver_축구)){
  tryCatch({
    remDr$navigate(링크_news.naver_축구[i])
    body <- remDr$getPageSource()[[1]]
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    body <- body %>% read_html()
    
    반응_축구.tmp <- body %>%  
      html_nodes("div#spiLayer") %>%
      html_nodes("li.u_likeit_list") %>%
      html_nodes("a.u_likeit_list_button") %>%
      html_nodes("span:nth-child(2)")%>%
      html_text()
    
    좋아_news.naver_축구 <- append(좋아_news.naver_축구, 반응_축구.tmp[1])
    훈훈_news.naver_축구 <- append(훈훈_news.naver_축구, 반응_축구.tmp[2])
    슬퍼_news.naver_축구 <- append(슬퍼_news.naver_축구, 반응_축구.tmp[3])
    화나_news.naver_축구 <- append(화나_news.naver_축구, 반응_축구.tmp[4])
    후속_news.naver_축구 <- append(후속_news.naver_축구, 반응_축구.tmp[5])
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}


뉴스_news.naver_축구 <- data.frame(언론사_news.naver_축구,
                                   제목_news.naver_축구,
                                   날짜_news.naver_축구,
                                   링크_news.naver_축구,
                                   좋아_news.naver_축구,
                                   훈훈_news.naver_축구,
                                   슬퍼_news.naver_축구,
                                   화나_news.naver_축구,
                                   후속_news.naver_축구)

names(뉴스_news.naver_축구) <- c("언론사_축구","제목_축구","날짜_축구","링크_축구","좋아_축구","훈훈_축구","슬퍼_축구","화나_축구","후속_축구")

write.csv(뉴스_news.naver, file = "C:/뉴스_df.csv", row.names=FALSE)


## 반응_sports.news

##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

# RSelenium
for (i in 1:length(링크_sports.news_축구)){
  tryCatch({
    remDr$navigate(링크_sports.news_축구[i])
    body <- remDr$getPageSource()[[1]]
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    body <- body %>% read_html()
    
    반응_축구.tmp <- body %>%  
      html_nodes("a.u_likeit_list_button") %>%
      html_nodes("span:nth-child(2)")%>%
      html_text()
    
    좋아_sports.news_축구 <- append(좋아_sports.news_축구, 반응_축구.tmp[1])
    훈훈_sports.news_축구 <- append(훈훈_sports.news_축구, 반응_축구.tmp[2])
    슬퍼_sports.news_축구 <- append(슬퍼_sports.news_축구, 반응_축구.tmp[3])
    화나_sports.news_축구 <- append(화나_sports.news_축구, 반응_축구.tmp[4])
    후속_sports.news_축구 <- append(후속_sports.news_축구, 반응_축구.tmp[5])
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

뉴스_sports.news_축구 <- data.frame(언론사_sports.news_축구,
                                    제목_sports.news_축구,
                                    날짜_sports.news_축구,
                                    링크_sports.news_축구,
                                    좋아_sports.news_축구,
                                    훈훈_sports.news_축구,
                                    슬퍼_sports.news_축구,
                                    화나_sports.news_축구,
                                    후속_sports.news_축구)

names(뉴스_sports.news_축구) <- c("언론사_축구","제목_축구","날짜_축구","링크_축구","좋아_축구","슬퍼_축구","화나_축구","팬이_축구","후속_축구")

end_time2 <- Sys.time()

start_time1 - end_time1
start_time2 - end_time2

## 데이터 프레임 저장
뉴스_df_축구 <- rbind(뉴스_news.naver_축구,뉴스_sports.news_축구)

write.csv(뉴스_df_축구, file = "C:/대학원/논문/소논문/뉴스_df_축구.csv", row.names=FALSE)
