library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)
library(tidyverse)
library(XML)

Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk-18.0.1.1')

##cmd 관리자권한으로 실행
# cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

# RSelenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()


## n4g - dark_souls
head <- "https://n4g.com/channel/"
keyword <- "dark-souls"
end <- "?load=3"

target1 <- paste0(head,keyword,end)

remDr$navigate(target1)

Sys.sleep(time = 10)

n <- 200

body <- remDr$getPageSource()[[1]]
body <- body %>% read_html()


# lead more n차례 실행 - 구글 광고 닫고 실행
for(i in 1:n){
  tryCatch({
    
    more_reply <- remDr$findElement("css", "a.f-load-more-link")
    more_reply$clickElement()
    
    if(i==n) break()
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}


# collect element
# head
body <- remDr$getPageSource()[[1]]
body <- body %>% read_html()

head_링크_ds <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_attr("href")

head_링크_ds <- paste0("https://n4g.com/",head_링크_ds)

# 본문
링크_ds <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_attr("href")
링크_ds <- paste0("https://n4g.com/",링크_ds)

링크_ds_all <- append(head_링크_ds,링크_ds) %>% unique()


# RSelenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

## n4g - the-last-of-us
head <- "https://n4g.com/channel/"
keyword <- "the-last-of-us"
end <- "?load=3"

target1 <- paste0(head,keyword,end)

remDr$navigate(target1)

Sys.sleep(time = 10)

n <- 200

body <- remDr$getPageSource()[[1]]
body <- body %>% read_html()


# lead more n차례 실행 - 구글 광고 닫고 실행
for(i in 1:n){
  tryCatch({
    
    more_reply <- remDr$findElement("css", "a.f-load-more-link")
    more_reply$clickElement()
    
    if(i==n) break()
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}


# collect element
# head
body <- remDr$getPageSource()[[1]]
body <- body %>% read_html()

head_링크_lou <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_attr("href")

head_링크_lou <- paste0("https://n4g.com/",head_링크_lou)

# 본문
링크_lou <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_attr("href")
링크_lou <- paste0("https://n4g.com/",링크_lou)

링크_lou_all <- append(head_링크_lou,링크_lou) %>% unique()


# rselenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()


## n4g - minecraft
head <- "https://n4g.com/channel/"
keyword <- "minecraft"
end <- "?load=3"

target1 <- paste0(head,keyword,end)

remDr$navigate(target1)

Sys.sleep(time = 10)

n <- 200

body <- remDr$getPageSource()[[1]]
body <- body %>% read_html()


# lead more n차례 실행 - 구글 광고 닫고 실행
for(i in 1:n){
  tryCatch({
    
    more_reply <- remDr$findElement("css", "a.f-load-more-link")
    more_reply$clickElement()
    
    if(i==n) break()
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}


# collect element
# head
body <- remDr$getPageSource()[[1]]
body <- body %>% read_html()

head_링크_mc <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_attr("href")

head_링크_mc <- paste0("https://n4g.com/",head_링크_mc)

# 본문
링크_mc <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_attr("href")
링크_mc <- paste0("https://n4g.com/",링크_mc)

링크_mc_all <- append(head_링크_mc,링크_mc) %>% unique()


#rselenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()


## n4g - league-of-legends
head <- "https://n4g.com/channel/"
keyword <- "league-of-legends"
end <- "?load=3"

target1 <- paste0(head,keyword,end)

remDr$navigate(target1)

Sys.sleep(time = 10)

n <- 200

body <- remDr$getPageSource()[[1]]
body <- body %>% read_html()


# lead more n차례 실행 - 구글 광고 닫고 실행
for(i in 1:n){
  tryCatch({
    
    more_reply <- remDr$findElement("css", "a.f-load-more-link")
    more_reply$clickElement()
    
    if(i==n) break()
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}


# collect element
# head
body <- remDr$getPageSource()[[1]]
body <- body %>% read_html()

head_링크_lol <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_attr("href")

head_링크_lol <- paste0("https://n4g.com/",head_링크_lol)

# 본문
링크_lol <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_attr("href")
링크_lol <- paste0("https://n4g.com/",링크_lol)

링크_lol_all <- append(head_링크_lol,링크_lol) %>% unique()




start.time <- Sys.time()

# 링크 탐색
제목_ds <- c()
관심도_ds <- c()
날짜_ds <- c()
종류_ds <- c()
출처_ds <- c()
댓글수_ds <- c()
댓글날짜_ds <- c()

for(i in 1:length(링크_ds_all)){
  tryCatch({
    cat(i, '페이지 수집 중', '입니다.\n') 
    
    body <- 링크_ds_all[i] %>% read_html()
    
    Sys.sleep(time = 1)
    
  제목_ds.tmp <- body %>% 
    html_nodes("div.sd-content") %>%
    html_nodes("h1.sd-title") %>%
    html_text()
  제목_ds <- append(제목_ds, 제목_ds.tmp)  
  
  관심도_ds.tmp <- body %>% 
    html_nodes("div.sd-wrap") %>% 
    html_nodes("span.temp-label") %>%
    html_text()
  관심도_ds <- append(관심도_ds, 관심도_ds.tmp)  
    
  날짜_ds.tmp <- body %>% 
    html_nodes("span.sd-time") %>%
    html_text()
  날짜_ds <- append(날짜_ds, 날짜_ds.tmp)  
    
  종류_ds.tmp <- body %>% 
    html_nodes("a.sd-type") %>%
    html_text()
  종류_ds <- append(종류_ds, 종류_ds.tmp)
    
  출처_ds.tmp <- body %>% 
    html_nodes("span.sd-source-name") %>%
    html_text()
  출처_ds <- append(출처_ds, 출처_ds.tmp) 
    
  댓글수_ds.tmp <- body %>% 
    html_nodes("a.sd-commentcount") %>%
    html_text()
  댓글수_ds <- append(댓글수_ds, 댓글수_ds.tmp) 
    
  댓글날짜_ds.tmp <- (body %>% 
    html_nodes("span.cl-item-time") %>%
    html_text() %>% sort())[1]
  if (length(댓글날짜_ds.tmp) != 0) {
    댓글날짜_ds <- append(댓글날짜_ds, 댓글날짜_ds.tmp) 
  } else {
    댓글날짜_ds <- append(댓글날짜_ds, c(0)) 
  }    
  
  Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}


# 데이터 프레임 만들기
n4g_ds <- data.frame(제목_ds,
                     관심도_ds,
                     날짜_ds,
                     종류_ds,
                     출처_ds,
                     댓글수_ds,
                     댓글날짜_ds)

names(n4g_ds) <- c("제목", "관심도", "날짜", "종류", "출처", "댓글수","댓글날짜")

# 데이터 저장
write.csv(n4g_ds, file = "D:/대학원/논문/소논문/텍스트마이닝 생존분석/n4g_ds.csv", row.names=FALSE, fileEncoding = 'utf-8')


# 링크 탐색
제목_lou <- c()
관심도_lou <- c()
날짜_lou <- c()
종류_lou <- c()
출처_lou <- c()
댓글수_lou <- c()
댓글날짜_lou <- c()

for(i in 1:length(링크_lou_all)){
  tryCatch({
    cat(i, '페이지 수집 중', '입니다.\n') 
    
    body <- 링크_lou_all[i] %>% read_html()
    
    Sys.sleep(time = 1)
    
    제목_lou.tmp <- body %>% 
      html_nodes("div.sd-content") %>%
      html_nodes("h1.sd-title") %>%
      html_text()
    제목_lou <- append(제목_lou, 제목_lou.tmp)  
    
    관심도_lou.tmp <- body %>% 
      html_nodes("div.sd-wrap") %>% 
      html_nodes("span.temp-label") %>%
      html_text()
    관심도_lou <- append(관심도_lou, 관심도_lou.tmp)  
    
    날짜_lou.tmp <- body %>% 
      html_nodes("span.sd-time") %>%
      html_text()
    날짜_lou <- append(날짜_lou, 날짜_lou.tmp)  
    
    종류_lou.tmp <- body %>% 
      html_nodes("a.sd-type") %>%
      html_text()
    종류_lou <- append(종류_lou, 종류_lou.tmp)
    
    출처_lou.tmp <- body %>% 
      html_nodes("span.sd-source-name") %>%
      html_text()
    출처_lou <- append(출처_lou, 출처_lou.tmp) 
    
    댓글수_lou.tmp <- body %>% 
      html_nodes("a.sd-commentcount") %>%
      html_text()
    댓글수_lou <- append(댓글수_lou, 댓글수_lou.tmp) 
    
    댓글날짜_lou.tmp <- (body %>% 
                       html_nodes("span.cl-item-time") %>%
                       html_text() %>% sort())[1]
    if (length(댓글날짜_lou.tmp) != 0) {
      댓글날짜_lou <- append(댓글날짜_lou, 댓글날짜_lou.tmp) 
    } else {
      댓글날짜_lou <- append(댓글날짜_lou, c(0)) 
    }    
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

# 데이터 프레임 만들기
n4g_lou <- data.frame(제목_lou,
                        관심도_lou,
                        날짜_lou,
                        종류_lou,
                        출처_lou,
                        댓글수_lou,
                        댓글날짜_lou)

names(n4g_lou) <- c("제목", "관심도", "날짜", "종류", "출처", "댓글수","댓글날짜")

# 데이터 저장
write.csv(n4g_lou, file = "D:/대학원/논문/소논문/텍스트마이닝 생존분석/n4g_lou.csv", row.names=FALSE, fileEncoding = 'utf-8')


# 링크 탐색
제목_mc <- c()
관심도_mc <- c()
날짜_mc <- c()
종류_mc <- c()
출처_mc <- c()
댓글수_mc <- c()
댓글날짜_mc <- c()

for(i in 1:length(링크_mc_all)){
  tryCatch({
    cat(i, '페이지 수집 중', '입니다.\n') 
    
    body <- 링크_mc_all[i] %>% read_html()
    
    Sys.sleep(time = 1)
    
    제목_mc.tmp <- body %>% 
      html_nodes("div.sd-content") %>%
      html_nodes("h1.sd-title") %>%
      html_text()
    제목_mc <- append(제목_mc, 제목_mc.tmp)  
    
    관심도_mc.tmp <- body %>% 
      html_nodes("div.sd-wrap") %>% 
      html_nodes("span.temp-label") %>%
      html_text()
    관심도_mc <- append(관심도_mc, 관심도_mc.tmp)  
    
    날짜_mc.tmp <- body %>% 
      html_nodes("span.sd-time") %>%
      html_text()
    날짜_mc <- append(날짜_mc, 날짜_mc.tmp)  
    
    종류_mc.tmp <- body %>% 
      html_nodes("a.sd-type") %>%
      html_text()
    종류_mc <- append(종류_mc, 종류_mc.tmp)
    
    출처_mc.tmp <- body %>% 
      html_nodes("span.sd-source-name") %>%
      html_text()
    출처_mc <- append(출처_mc, 출처_mc.tmp) 
    
    댓글수_mc.tmp <- body %>% 
      html_nodes("a.sd-commentcount") %>%
      html_text()
    댓글수_mc <- append(댓글수_mc, 댓글수_mc.tmp) 
    
    댓글날짜_mc.tmp <- (body %>% 
                      html_nodes("span.cl-item-time") %>%
                      html_text() %>% sort())[1]
    if (length(댓글날짜_mc.tmp) != 0) {
      댓글날짜_mc <- append(댓글날짜_mc, 댓글날짜_mc.tmp) 
    } else {
      댓글날짜_mc <- append(댓글날짜_mc, c(0)) 
    }    
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

# 데이터 프레임 만들기
n4g_mc <- data.frame(제목_mc,
                       관심도_mc,
                       날짜_mc,
                       종류_mc,
                       출처_mc,
                       댓글수_mc,
                       댓글날짜_mc)


names(n4g_mc) <- c("제목", "관심도", "날짜", "종류", "출처", "댓글수","댓글날짜")

# 데이터 저장
write.csv(n4g_mc, file = "D:/대학원/논문/소논문/텍스트마이닝 생존분석/n4g_mc.csv", row.names=FALSE, fileEncoding = 'utf-8')


# 링크 탐색
제목_lol <- c()
관심도_lol <- c()
날짜_lol <- c()
종류_lol <- c()
출처_lol <- c()
댓글수_lol <- c()
댓글날짜_lol <- c()

for(i in 1:length(링크_lol_all)){
  tryCatch({
    cat(i, '페이지 수집 중', '입니다.\n') 
    
    body <- 링크_lol_all[i] %>% read_html()
    
    Sys.sleep(time = 1)
    
    제목_lol.tmp <- body %>% 
      html_nodes("div.sd-content") %>%
      html_nodes("h1.sd-title") %>%
      html_text()
    제목_lol <- append(제목_lol, 제목_lol.tmp)  
    
    관심도_lol.tmp <- body %>% 
      html_nodes("div.sd-wrap") %>% 
      html_nodes("span.temp-label") %>%
      html_text()
    관심도_lol <- append(관심도_lol, 관심도_lol.tmp)  
    
    날짜_lol.tmp <- body %>% 
      html_nodes("span.sd-time") %>%
      html_text()
    날짜_lol <- append(날짜_lol, 날짜_lol.tmp)  
    
    종류_lol.tmp <- body %>% 
      html_nodes("a.sd-type") %>%
      html_text()
    종류_lol <- append(종류_lol, 종류_lol.tmp)
    
    출처_lol.tmp <- body %>% 
      html_nodes("span.sd-source-name") %>%
      html_text()
    출처_lol <- append(출처_lol, 출처_lol.tmp) 
    
    댓글수_lol.tmp <- body %>% 
      html_nodes("a.sd-commentcount") %>%
      html_text()
    댓글수_lol <- append(댓글수_lol, 댓글수_lol.tmp) 
    
    댓글날짜_lol.tmp <- (body %>% 
                       html_nodes("span.cl-item-time") %>%
                       html_text() %>% sort())[1]
    if (length(댓글날짜_lol.tmp) != 0) {
      댓글날짜_lol <- append(댓글날짜_lol, 댓글날짜_lol.tmp) 
    } else {
      댓글날짜_lol <- append(댓글날짜_lol, c(0)) 
    }    
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

# 데이터 프레임 만들기
n4g_lol <- data.frame(제목_lol,
                        관심도_lol,
                        날짜_lol,
                        종류_lol,
                        출처_lol,
                        댓글수_lol,
                        댓글날짜_lol)

names(n4g_lol) <- c("제목", "관심도", "날짜", "종류", "출처", "댓글수","댓글날짜")

# 데이터 저장
write.csv(n4g_lol, file = "D:/대학원/논문/소논문/텍스트마이닝 생존분석/n4g_lol.csv", row.names=FALSE, fileEncoding = 'utf-8')

end.time <- Sys.time()

end.time - start.time