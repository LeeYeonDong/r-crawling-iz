library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)

start_time <- Sys.time()

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
searchword <- "야구"
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


링크_news_야구 <- c()

for (i in 1:length(dd2021)){
  for (j in 1:length(seq)){
    링크_news_야구.tmp <- paste0(head,searchword,body0,body1,dd2021[i],body2,dd2021[i],body3,from2021[i],to,from2021[i],body5,seq[j],body6)
    링크_news_야구 <- append(링크_news_야구,링크_news_야구.tmp)
  }
}

### 네이버뉴스링크-독자반응포함
### news.naver 벡터 생성
언론사_news.naver_야구 <- c()
제목_news.naver_야구 <- c()
날짜_news.naver_야구 <- c()

좋아_news.naver_야구 <- c()
훈훈_news.naver_야구 <- c()
슬퍼_news.naver_야구 <- c()
화나_news.naver_야구 <- c()
후속_news.naver_야구 <- c()

length(링크_news_야구)
## 네이버뉴스 링크 수집
for (i in 1:1000){
  tryCatch({
    cat(i, '페이지 수집 중', '입니다.\n') 
    
    body <- 링크_news_야구[i] %>% read_html()
    
    #링크
    링크_야구.tmp <- body %>% 
      html_nodes("div.info_group") %>%
      html_nodes("a:nth-child(3)") %>%
      html_attr("href")
    
    if (length(링크_야구.tmp) != 0) {
      링크_news_야구 <- append(링크_news_야구,링크_news_야구.tmp)
    } else {
      링크_news_야구 <- append(링크_news_야구,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}

링크_news_야구 <- 링크_news_야구 %>% unique()

news.naver_야구 <- grep("https://news.naver.com",링크_news_야구)
링크_news.naver_야구 <- 링크_news_야구[news.naver_야구]


## 링크 분할
링크_news.naver_sp_야구 <- strsplit(링크_news.naver_야구, split="&")  

링크_news.naver.oid_야구 <- c()
링크_news.naver.aid_야구 <- c()

for (i in 1:length(링크_news.naver_야구)){
  링크_news.naver_sp1_야구.tmp <- 링크_news.naver_sp_야구[[i]][4]
  링크_news.naver_sp2_야구.tmp <- 링크_news.naver_sp_야구[[i]][5]
  
  링크_news.naver.oid_야구 <- append(링크_news.naver.oid_야구,링크_news.naver_sp1_야구.tmp)
  링크_news.naver.aid_야구 <- append(링크_news.naver.aid_야구,링크_news.naver_sp2_야구.tmp)
}

링크_news.naver.oid_야구 <- str_replace_all(링크_news.naver.oid_야구,"oid=","")
링크_news.naver.aid_야구 <- str_replace_all(링크_news.naver.aid_야구,"aid=","")


## 페이지 정보 읽기 - 최신순 정렬
for (i in 1:length(링크_news.naver_야구)){
  tryCatch({
    res_news.naver_야구 <- GET(url = 'https://news.naver.com/main/read.nhn',
                          query = list(
                            oid = 링크_news.naver.oid_야구[i],
                            aid = 링크_news.naver.aid_야구[i]))
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_news.naver_야구), '입니다.\n') 
    
    body <- res_news.naver_야구 %>% read_html()
    
    #언론사
    언론사_야구.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_야구.tmp) != 0) {
      언론사_news.naver_야구 <- append(언론사_news.naver_야구,언론사_야구.tmp)
    } else {
      언론사_news.naver_야구 <- append(언론사_news.naver_야구,"수동확인")
    }    
    
    #제목
    제목_야구.tmp <- body %>% 
      html_nodes("div.article_info") %>%
      html_nodes("h3") %>%
      html_text()
    
    if (length(제목_야구.tmp) != 0) {
      제목_news.naver_야구 <- append(제목_news.naver_야구,제목_야구.tmp)
    } else {
      제목_news.naver_야구 <- append(제목_news.naver_야구,"수동확인")
    }    
    
    #날짜
    날짜_야구.tmp <- body %>% 
      html_nodes("span.t11") %>%
      html_text()
    
    날짜_야구.tmp <- 날짜_야구.tmp[1]
    
    if (length(날짜.tmp) != 0) {
      날짜_news.naver_야구 <- append(날짜_news.naver_야구,날짜_야구.tmp)
    } else {
      날짜_news.naver_야구 <- append(날짜_news.naver_야구,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

뉴스_news.naver_야구 <- data.frame(언론사_news.naver_야구,
                                   제목_news.naver_야구,
                                   날짜_news.naver_야구,
                                   링크_news.naver_야구)

names(뉴스_news.naver_야구) <- c("언론사_야구","제목_야구","날짜_야구","링크_야구")



### sports.news 벡터 생성
언론사_sports.news_야구 <- c()
제목_sports.news_야구 <- c()
날짜_sports.news_야구 <- c()

좋아_sports.news_야구 <- c()
훈훈_sports.news_야구 <- c()
슬퍼_sports.news_야구 <- c()
화나_sports.news_야구 <- c()
후속_sports.news_야구 <- c()

sports.news_야구 <- grep("https://sports.news",링크_news_야구)
링크_sports.news_야구 <- 링크_news[sports.news_야구]

## 링크 분할
링크_sports.news_sp_야구 <- strsplit(링크_sports.news_야구, split="=")  
링크_sports.news.oid_야구 <- c()
링크_sports.news.aid_야구 <- c()

for (i in 1:length(링크_sports.news_야구)){
  링크_sports.news_sp1_야구.tmp <- 링크_sports.news_sp_야구[[i]][2]
  링크_sports.news_sp2_야구.tmp <- 링크_sports.news_sp_야구[[i]][3]
  
  링크_sports.news.oid_야구 <- append(링크_sports.news.oid_야구,링크_sports.news_sp1_야구.tmp)
  링크_sports.news.aid_야구 <- append(링크_sports.news.aid_야구,링크_sports.news_sp2_야구.tmp)
}

링크_sports.news.oid_야구 <- str_replace_all(링크_sports.news.oid_야구,"&aid","")


## 페이지 정보 읽기 - 최신순 정렬
for (i in 1:length(링크_sports.news_야구)){
  tryCatch({
    res_sports.news_야구 <- GET(url = 'https://sports.news.naver.com/news.nhn',
                           query = list(
                             oid = 링크_sports.news.oid_야구[i],
                             aid = 링크_sports.news.aid_야구[i]))
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_sports.news_야구), '입니다.\n') 
    
    body <- res_sports.news_야구 %>% read_html()
    
    #언론사
    언론사_야구.tmp <- body %>% 
      html_nodes("a.link") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사_야구.tmp) != 0) {
      언론사_sports.news_야구 <- append(언론사_sports.news_야구,언론사_야구.tmp)
    } else {
      언론사_sports.news_야구 <- append(언론사_sports.news_야구,"수동확인")
    }    
    
    #제목
    제목_야구.tmp <- body %>% 
      html_nodes("h4.title") %>%
      html_text()
    
    if (length(제목_야구.tmp) != 0) {
      제목_sports.news_야구 <- append(제목_sports.news_야구,제목_야구.tmp)
    } else {
      제목_sports.news_야구 <- append(제목_sports.news_야구,"수동확인")
    }    
    
    
    #날짜
    날짜_야구.tmp <- body %>% 
      html_nodes("div.info") %>%
      html_nodes("span") %>%
      html_text()
    
    날짜_야구.tmp <- 날짜_야구.tmp[1]
    
    if (length(날짜.tmp) != 0) {
      날짜_sports.news_야구 <- append(날짜_sports.news_야구,날짜_야구.tmp)
    } else {
      날짜_sports.news_야구 <- append(날짜_sports.news_야구,"수동확인")
    }    
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

뉴스_sports.news_야구 <- data.frame(언론사_sports.news_야구,
                                   제목_sports.news_야구,
                                   날짜_sports.news_야구,
                                   링크_sports.news_야구)

names(뉴스_sports.news_야구) <- c("언론사_야구","제목_야구","날짜_야구","링크_야구")
뉴스_야구 <- rbind(뉴스_news.naver_야구,뉴스_sports.news_야구)

write.csv(뉴스_야구, file = "C:/뉴스_야구.csv", row.names=FALSE)

end_time <- Sys.time()








## 반응

##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

# RSelenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

for (i in 1:length(링크_news.naver)){
  
  tryCatch({
    remDr$navigate(링크_news.naver[i])
    body <- remDr$getPageSource()[[1]]
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    body <- body %>% read_html()
    
    반응.tmp <- body %>%  
      html_nodes("div#spiLayer") %>%
      html_nodes("li.u_likeit_list") %>%
      html_nodes("a.u_likeit_list_button") %>%
      html_nodes("span:nth-child(2)")%>%
      html_text()
    
    좋아_news.naver <- append(좋아_news.naver, 반응.tmp[1])
    훈훈_news.naver <- append(훈훈_news.naver, 반응.tmp[2])
    슬퍼_news.naver <- append(슬퍼_news.naver, 반응.tmp[3])
    화나_news.naver <- append(화나_news.naver, 반응.tmp[4])
    후속_news.naver <- append(후속_news.naver, 반응.tmp[5])
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}


뉴스_news.naver <- data.frame(언론사_news.naver,
                                제목_news.naver,
                                날짜_news.naver,
                                링크_news.naver,
                                좋아_news.naver,
                                훈훈_news.naver,
                                슬퍼_news.naver,
                                화나_news.naver,
                                후속_news.naver)

names(뉴스_news.naver) <- c("언론사","제목","날짜","링크","좋아","훈훈","슬퍼","화나","후속")

write.csv(뉴스_news.naver, file = "C:/뉴스_df.csv", row.names=FALSE)








## 반응

##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

# RSelenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

for (i in 1:length(링크_sports.news)){
  
  tryCatch({
    remDr$navigate(링크_sports.news[i])
    body <- remDr$getPageSource()[[1]]
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    body <- body %>% read_html()
    
    반응.tmp <- body %>%  
      html_nodes("a.u_likeit_list_button") %>%
      html_nodes("span:nth-child(2)")%>%
      html_text()
    
    좋아_sports.news <- append(좋아_sports.news, 반응.tmp[1])
    훈훈_sports.news <- append(훈훈_sports.news, 반응.tmp[2])
    슬퍼_sports.news <- append(슬퍼_sports.news, 반응.tmp[3])
    화나_sports.news <- append(화나_sports.news, 반응.tmp[4])
    후속_sports.news <- append(후속_sports.news, 반응.tmp[5])
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

뉴스_sports.news <- data.frame(언론사_sports.news,
                                 제목_sports.news,
                                 날짜_sports.news,
                                 링크_sports.news,
                                 좋아_sports.news,
                                 훈훈_sports.news,
                                 슬퍼_sports.news,
                                 화나_sports.news,
                                 후속_sports.news)

names(뉴스_sports.news) <- c("언론사","제목","날짜","링크","좋아","훈훈","슬퍼","화나","후속")

뉴스_df <- rbind(뉴스_news.naver,뉴스_sports.news)

write.csv(뉴스_spo1, file = "G:/대학원/논문/소논문/뉴스_df.csv", row.names=FALSE)