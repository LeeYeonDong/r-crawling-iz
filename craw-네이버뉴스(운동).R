install.packages("torch")
library(torch)
torch_tensor(1, device = "cuda")

library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)

### 뉴스
## 벡터생성
링크_spo1 <- c()
언론사_spo1 <- c()
제목_spo1 <- c()
날짜_spo1 <- c()

## 기본 설정
n <- 100000

searchword <- "야구"

seq <- seq(from = 1, by = 10, length.out = n)

sd <- "2020.02.23"
ed <- "2021.03.15"

nav %>% length()

## 페이지 정보 읽기 - 최신순 정렬
for (i in 1:n){
  tryCatch({
    nav
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_spo1), '입니다.\n') 
    
    body <- nav %>% read_html()
    
    #링크
    링크.tmp <- body %>% 
      html_nodes("a.news_tit") %>%
      html_attr("href")

    if (length(링크.tmp) != 0) {
      링크_spo1 <- append(링크_spo1,링크.tmp)
    } else {
      링크_spo1 <- append(링크_spo1,"수동확인")
    }
    
    
    #언론사
    언론사.tmp <- body %>% 
      html_nodes("a.info") %>% 
      html_text()
    
    if (length(언론사.tmp) != 0) {
      언론사_spo1 <- append(언론사_spo1,언론사.tmp)
    } else {
      언론사_spo1 <- append(언론사_spo1,"수동확인")
    }

    
    #제목
    제목.tmp <- body %>% 
      html_nodes("a.news_tit") %>%
      html_text()

    if (length(제목.tmp) != 0) {
      제목_spo1 <- append(제목_spo1,제목.tmp)
    } else {
      제목_spo1 <- append(제목_spo1,"수동확인")
    }    
 
    
    #날짜
    날짜.tmp <- body %>% 
      html_nodes("span.info") %>%
      html_text()
   
    if (length(날짜.tmp) != 0) {
      날짜_spo1 <- append(날짜_spo1,날짜.tmp)
    } else {
      날짜_spo1 <- append(날짜_spo1,"수동확인")
    }    
    
    Sys.sleep(time = 0.1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

전 <- grep("전",날짜_spo1)
날짜_spo1 <- 날짜_spo1[전]

네이버뉴스 <- grep("네이버뉴스",언론사_spo1)
언론사_spo1 <- 언론사_spo1[-네이버뉴스]

뉴스_spo1 <- data.frame(언론사_spo1,제목_spo1,날짜_spo1,링크_spo1)

언론사_spo1 %>% length()
제목_spo1 %>% length()
날짜_spo1 %>% length()
링크_spo1 %>% length()

names(뉴스_spo1) <- c("언론사","제목","날짜","링크")

write.csv(뉴스_spo1, file = "G:/대학원/논문/소논문/뉴스_spo1.csv", row.names=FALSE)

#####################################








### 네이버뉴스링크-독자반응포함

## 기본 설정
n <- 400

searchword <- "야구"

seq <- seq(from = 1, by = 10, length.out = n)

d20 <- c()

m <- c(1:12)
d1 <- c(1:31)

for (j in 1:length(m)){
for (i in 1:length(d1)){
  d20.tmp <- paste0("2020",".",m[j],".",d1[i])
  d20 <- append(d20,d20.tmp)
  }
}

d21 <- c()

m <- c(1:3)
d1 <- c(1:31)

for (i in 1:length(d1)){
  for (j in 1:length(m)){
    d21.tmp <- paste0("2021",".",m[j],".",d1[i])
    d21 <- append(sd21,sd21.tmp)
  }
}

d <- append(d20,d21)


### news.naver 벡터 생성
링크_news <- c()

언론사_news.naver <- c()
제목_news.naver <- c()
날짜_news.naver <- c()

좋아_news.naver <- c()
훈훈_news.naver <- c()
슬퍼_news.naver <- c()
화나_news.naver <- c()
후속_news.naver <- c()


## 네이버뉴스 링크 수집
  for (i in 1:length(nav)){
    tryCatch({

    cat(i, '페이지 수집 중. 상태코드는', '입니다.\n') 
    
    body <- nav[20] %>% read_html()
    
    #링크
    링크.tmp <- body %>% 
      html_nodes("div.info_group") %>%
      html_nodes("a:nth-child(3)") %>%
      html_attr("href")
    
    if (length(링크.tmp) != 0) {
      링크_news <- append(링크_news,링크.tmp)
    } else {
      링크_news <- append(링크_news,"수동확인")
    }    
    
    Sys.sleep(time = 0.01)
    
    }, error = function(e) cat("불러올 수 없습니다!\n"))
    
  }


링크_news <- 링크_news %>% unique()

news.naver <- grep("https://news.naver.com",링크_news)
링크_news.naver <- 링크_news[news.naver]


## 링크 분할
링크_news.naver_sp <- strsplit(링크_news.naver, split="&")  

링크_news.naver.oid <- c()
링크_news.naver.aid <- c()

for (i in 1:length(링크_news.naver)){
  링크_news.naver_sp1.tmp <- 링크_news.naver_sp[[i]][4]
  링크_news.naver_sp2.tmp <- 링크_news.naver_sp[[i]][5]
  
  링크_news.naver.oid <- append(링크_news.naver.oid,링크_news.naver_sp1.tmp)
  링크_news.naver.aid <- append(링크_news.naver.aid,링크_news.naver_sp2.tmp)
}

링크_news.naver.oid <- str_replace_all(링크_news.naver.oid,"oid=","")
링크_news.naver.aid <- str_replace_all(링크_news.naver.aid,"aid=","")


## 페이지 정보 읽기 - 최신순 정렬
for (i in 1:length(링크_news.naver)){
  tryCatch({
    res_news.naver <- GET(url = 'https://news.naver.com/main/read.nhn',
                          query = list(
                            oid = 링크_news.naver.oid[i],
                            aid = 링크_news.naver.aid[i]))

    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_news.naver), '입니다.\n') 

    body <- res_news.naver %>% read_html()
    
    #언론사
    언론사.tmp <- body %>% 
      html_nodes("div.press_logo") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사.tmp) != 0) {
      언론사_news.naver <- append(언론사_news.naver,언론사.tmp)
    } else {
      언론사_news.naver <- append(언론사_news.naver,"수동확인")
    }    
    
    #제목
    제목.tmp <- body %>% 
      html_nodes("div.article_info") %>%
      html_nodes("h3") %>%
      html_text()
    
    if (length(제목.tmp) != 0) {
      제목_news.naver <- append(제목_news.naver,제목.tmp)
    } else {
      제목_news.naver <- append(제목_news.naver,"수동확인")
    }    
    
    #날짜
    날짜.tmp <- body %>% 
      html_nodes("span.t11") %>%
      html_text()
    
    날짜.tmp <- 날짜.tmp[1]
    
    if (length(날짜.tmp) != 0) {
      날짜_news.naver <- append(날짜_news.naver,날짜.tmp)
    } else {
      날짜_news.naver <- append(날짜_news.naver,"수동확인")
    }    

    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}


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

### sports.news 벡터 생성
언론사_sports.news <- c()
제목_sports.news <- c()
날짜_sports.news <- c()

좋아_sports.news <- c()
훈훈_sports.news <- c()
슬퍼_sports.news <- c()
화나_sports.news <- c()
후속_sports.news <- c()

sports.news <- grep("https://sports.news",링크_news)
링크_sports.news <- 링크_news[sports.news]


## 링크 분할
링크_sports.news_sp <- strsplit(링크_sports.news, split="=")  
링크_sports.news.oid <- c()
링크_sports.news.aid <- c()

for (i in 1:length(링크_sports.news)){
  링크_sports.news_sp1.tmp <- 링크_sports.news_sp[[i]][2]
  링크_sports.news_sp2.tmp <- 링크_sports.news_sp[[i]][3]
  
  링크_sports.news.oid <- append(링크_sports.news.oid,링크_sports.news_sp1.tmp)
  링크_sports.news.aid <- append(링크_sports.news.aid,링크_sports.news_sp2.tmp)
}

링크_sports.news.oid <- str_replace_all(링크_sports.news.oid,"&aid","")


## 페이지 정보 읽기 - 최신순 정렬
for (i in 1:length(링크_sports.news)){
  tryCatch({
    res_sports.news <- GET(url = 'https://sports.news.naver.com/news.nhn',
                          query = list(
                            oid = 링크_sports.news.oid[i],
                            aid = 링크_sports.news.aid[i]))
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_sports.news), '입니다.\n') 
    
    body <- res_sports.news %>% read_html()
    
    #언론사
    언론사.tmp <- body %>% 
      html_nodes("a.link") %>%
      html_nodes("img") %>% 
      html_attr("alt")
    
    if (length(언론사.tmp) != 0) {
      언론사_sports.news <- append(언론사_sports.news,언론사.tmp)
    } else {
      언론사_sports.news <- append(언론사_sports.news,"수동확인")
    }    
    
    
    #제목
    제목.tmp <- body %>% 
      html_nodes("h4.title") %>%
      html_text()
    
    if (length(제목.tmp) != 0) {
      제목_sports.news <- append(제목_sports.news,제목.tmp)
    } else {
      제목_sports.news <- append(제목_sports.news,"수동확인")
    }    
    
    
    #날짜
    날짜.tmp <- body %>% 
      html_nodes("div.info") %>%
      html_nodes("span") %>%
      html_text()
    
    날짜.tmp <- 날짜.tmp[1]
    
    if (length(날짜.tmp) != 0) {
      날짜_sports.news <- append(날짜_sports.news,날짜.tmp)
    } else {
      날짜_sports.news <- append(날짜_sports.news,"수동확인")
    }    
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

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
