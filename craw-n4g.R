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

Sys.sleep(time = 5)

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

head_제목_ds <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_text()

head_관심도_ds <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("span.temp-label") %>%
  html_text()

head_날짜_ds <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-timeago") %>%
  html_text()

head_종류_ds <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-meta") %>%
  html_nodes("a") %>%
  html_text() %>% matrix(ncol=2, byrow = TRUE) %>% as.data.frame() %>% select(V1) %>% as.vector()

head_출처_ds <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-meta") %>%
  html_nodes("a") %>%
  html_text() %>% matrix(ncol=2, byrow = TRUE) %>% as.data.frame() %>% select(V2) %>% as.vector()

# 본문
링크_ds <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_attr("href")
링크_ds <- paste0("https://n4g.com/",링크_ds)

제목_ds <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_text()

관심도_ds <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("span.temp-label") %>%
  html_text()

날짜_ds <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-timeago") %>%
  html_text()

종류_ds <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-meta") %>%
  html_nodes("a") %>%
  html_text() %>% matrix(ncol=2, byrow = TRUE) %>% as.data.frame() %>% select(V1) %>% as.vector()

출처_ds <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-meta") %>%
  html_nodes("a") %>%
  html_text() %>% matrix(ncol=2, byrow = TRUE) %>% as.data.frame() %>% select(V2) %>% as.vector()

# head 댓글
head_댓글_ds <- c()

env = environment()
env$ans <- NULL
for (i in 1:length(head_링크_ds)){
  tryCatch({
    body <- head_링크_ds[i] %>% read_html()
    
    cat(i, '페이지 수집 중입니다.\n')
    
    head_댓글_ds.tmp <- body %>% 
      html_nodes("a.sd-commentcount") %>%
      html_text()
    
    if (length(head_댓글_ds.tmp) != 0) {
      head_댓글_ds <- append(head_댓글_ds, head_댓글_ds.tmp)
    } else {
      head_댓글_ds <- append(head_댓글_ds,"0")
    }   
    
    Sys.sleep(time = 1)
    
  }, error = function(e) {
    cat(conditionMessage(e),"\n")
    env$ans[i] <- "error"
  }
  )
}

head_댓글_ds <- head_댓글_ds %>% as.integer()

# 에러 구간 "0" 집어넣기
er <- grep("error",env$ans)
for (i in 1:length(er)){
  head_댓글_ds <- append(head_댓글_ds,c(0),after = er[i]-1)
}

head_n4g_ds <- data.frame(head_제목_ds,
                          head_관심도_ds,
                          head_날짜_ds,
                          head_종류_ds,
                          head_출처_ds,
                          head_링크_ds,
                          head_댓글_ds)

names(head_n4g_ds) <- c("제목", "관심도", "날짜", "종류", "출처", "링크", "댓글")
head_n4g_ds %>% str()


# 본문 댓글
댓글_ds <- c()

env = environment()
env$ans <- NULL
for (i in 1:length(링크_ds)){
  tryCatch({
    body <- 링크_ds[i] %>% read_html()

    cat(i, '페이지 수집 중입니다.\n')
     
    댓글_ds.tmp <- body %>% 
      html_nodes("a.sd-commentcount") %>%
      html_text()
    
    if (length(댓글_ds.tmp) != 0) {
      댓글_ds <- append(댓글_ds, 댓글_ds.tmp)
    } else {
      댓글_ds <- append(댓글_ds,"0")
    }   

    Sys.sleep(time = 0.5)

  }, error = function(e) {
    cat(conditionMessage(e),"\n")
    env$ans[i] <- "error"
    }
)
}
댓글_ds <- 댓글_ds %>% as.integer()

# 에러 구간 "0" 집어넣기
er <- grep("error",env$ans)
for (i in 1:length(er)){
  댓글_ds <- append(댓글_ds,c(0),after = er[i]-1)
}
  
n4g_ds <- data.frame(제목_ds,
                     관심도_ds,
                     날짜_ds,
                     종류_ds,
                     출처_ds,
                     링크_ds,
                     댓글_ds)

names(n4g_ds) <- c("제목", "관심도", "날짜", "종류", "출처", "링크", "댓글")
n4g_ds %>% str()

# head + 본문
n4g_ds <- bind_rows(head_n4g_ds,n4g_ds)
n4g_ds %>% dim()

write.csv(n4g_ds, file = "D:/대학원/논문/소논문/텍스트마이닝 생존분석/n4g_ds.csv", row.names=FALSE)


## n4g – the last of us
# RSelenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()
head <- "https://n4g.com/channel/"
keyword <- "the-last-of-us"
end <- "?load=3"

target2 <- paste0(head,keyword,end)

remDr$navigate(target2)

Sys.sleep(time = 5)

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

head_제목_lou <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_text()

head_관심도_lou <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("span.temp-label") %>%
  html_text()

head_날짜_lou <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-timeago") %>%
  html_text()

head_종류_lou <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-meta") %>%
  html_nodes("a") %>%
  html_text() %>% matrix(ncol=2, byrow = TRUE) %>% as.data.frame() %>% select(V1) %>% as.vector()

head_출처_lou <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-meta") %>%
  html_nodes("a") %>%
  html_text() %>% matrix(ncol=2, byrow = TRUE) %>% as.data.frame() %>% select(V2) %>% as.vector()

# 본문
링크_lou <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_attr("href")
링크_lou <- paste0("https://n4g.com/",링크_lou)

제목_lou <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_text()

관심도_lou <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("span.temp-label") %>%
  html_text()

날짜_lou <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-timeago") %>%
  html_text()

종류_lou <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-meta") %>%
  html_nodes("a") %>%
  html_text() %>% matrix(ncol=2, byrow = TRUE) %>% as.data.frame() %>% select(V1) %>% as.vector()

출처_lou <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-meta") %>%
  html_nodes("a") %>%
  html_text() %>% matrix(ncol=2, byrow = TRUE) %>% as.data.frame() %>% select(V2) %>% as.vector()

# head 댓글
head_댓글_lou <- c()

env = environment()
env$ans <- NULL
for (i in 1:length(head_링크_lou)){
  tryCatch({
    body <- head_링크_lou[i] %>% read_html()
    
    cat(i, '페이지 수집 중입니다.\n')
    
    head_댓글_lou.tmp <- body %>% 
      html_nodes("a.sd-commentcount") %>%
      html_text()
    
    if (length(head_댓글_lou.tmp) != 0) {
      head_댓글_lou <- append(head_댓글_lou, head_댓글_lou.tmp)
    } else {
      head_댓글_lou <- append(head_댓글_lou,"0")
    }   
    
    Sys.sleep(time = 1)
    
  }, error = function(e) {
    cat(conditionMessage(e),"\n")
    env$ans[i] <- "error"
  }
  )
}

head_댓글_lou <- head_댓글_lou %>% as.integer()

# 에러 구간 "0" 집어넣기
er <- grep("error",env$ans)
for (i in 1:length(er)){
  head_댓글_lou <- append(head_댓글_lou,c(0),after = er[i]-1)
}

head_n4g_lou <- data.frame(head_제목_lou,
                           head_관심도_lou,
                           head_날짜_lou,
                           head_종류_lou,
                           head_출처_lou,
                           head_링크_lou,
                           head_댓글_lou)

names(head_n4g_lou) <- c("제목", "관심도", "날짜", "종류", "출처", "링크", "댓글")
head_n4g_lou %>% str()


# 본문 댓글
댓글_lou <- c()

env = environment()
env$ans <- NULL
for (i in 1:length(링크_lou)){
  tryCatch({
    body <- 링크_lou[i] %>% read_html()
    
    cat(i, '페이지 수집 중입니다.\n')
    
    댓글_lou.tmp <- body %>% 
      html_nodes("a.sd-commentcount") %>%
      html_text()
    
    if (length(댓글_lou.tmp) != 0) {
      댓글_lou <- append(댓글_lou, 댓글_lou.tmp)
    } else {
      댓글_lou <- append(댓글_lou,"0")
    }   
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) {
    cat(conditionMessage(e),"\n")
    env$ans[i] <- "error"
  }
  )
}
댓글_lou <- 댓글_lou %>% as.integer()

# 에러 구간 "0" 집어넣기
er <- grep("error",env$ans)
for (i in 1:length(er)){
  댓글_lou <- append(댓글_lou,c(0),after = er[i]-1)
}

n4g_lou <- data.frame(제목_lou,
                        관심도_lou,
                        날짜_lou,
                        종류_lou,
                        출처_lou,
                        링크_lou,
                        댓글_lou)

names(n4g_lou) <- c("제목", "관심도", "날짜", "종류", "출처", "링크", "댓글")
n4g_lou %>% str()

# head + 본문
n4g_lou <- bind_rows(head_n4g_lou,n4g_lou)
n4g_lou %>% dim()

write.csv(n4g_lou, file = "D:/대학원/논문/소논문/텍스트마이닝 생존분석/n4g_lou.csv", row.names=FALSE)


## n4g – minecraft
# RSelenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()
head <- "https://n4g.com/channel/"
keyword <- "minecraft"
end <- "?load=3"

target3 <- paste0(head,keyword,end)

remDr$navigate(target3)

Sys.sleep(time = 5)

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

head_제목_mc <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_text()

head_관심도_mc <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("span.temp-label") %>%
  html_text()

head_날짜_mc <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-timeago") %>%
  html_text()

head_종류_mc <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-meta") %>%
  html_nodes("a") %>%
  html_text() %>% matrix(ncol=2, byrow = TRUE) %>% as.data.frame() %>% select(V1) %>% as.vector()

head_출처_mc <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-meta") %>%
  html_nodes("a") %>%
  html_text() %>% matrix(ncol=2, byrow = TRUE) %>% as.data.frame() %>% select(V2) %>% as.vector()

# 본문
링크_mc <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_attr("href")
링크_mc <- paste0("https://n4g.com/",링크_mc)

제목_mc <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_text()

관심도_mc <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("span.temp-label") %>%
  html_text()

날짜_mc <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-timeago") %>%
  html_text()

종류_mc <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-meta") %>%
  html_nodes("a") %>%
  html_text() %>% matrix(ncol=2, byrow = TRUE) %>% as.data.frame() %>% select(V1) %>% as.vector()

출처_mc <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-meta") %>%
  html_nodes("a") %>%
  html_text() %>% matrix(ncol=2, byrow = TRUE) %>% as.data.frame() %>% select(V2) %>% as.vector()

# head 댓글
head_댓글_mc <- c()

env = environment()
env$ans <- NULL
for (i in 1:length(head_링크_mc)){
  tryCatch({
    body <- head_링크_mc[i] %>% read_html()
    
    cat(i, '페이지 수집 중입니다.\n')
    
    head_댓글_mc.tmp <- body %>% 
      html_nodes("a.sd-commentcount") %>%
      html_text()
    
    if (length(head_댓글_mc.tmp) != 0) {
      head_댓글_mc <- append(head_댓글_mc, head_댓글_mc.tmp)
    } else {
      head_댓글_mc <- append(head_댓글_mc,"0")
    }   
    
    Sys.sleep(time = 1)
    
  }, error = function(e) {
    cat(conditionMessage(e),"\n")
    env$ans[i] <- "error"
  }
  )
}

head_댓글_mc <- head_댓글_mc %>% as.integer()

# 에러 구간 "0" 집어넣기
er <- grep("error",env$ans)
for (i in 1:length(er)){
  head_댓글_mc <- append(head_댓글_mc,c(0),after = er[i]-1)
}

head_n4g_mc <- data.frame(head_제목_mc,
                          head_관심도_mc,
                          head_날짜_mc,
                          head_종류_mc,
                          head_출처_mc,
                          head_링크_mc,
                          head_댓글_mc)

names(head_n4g_mc) <- c("제목", "관심도", "날짜", "종류", "출처", "링크", "댓글")
head_n4g_mc %>% str()


# 본문 댓글
댓글_mc <- c()

env = environment()
env$ans <- NULL
for (i in 1:length(링크_mc)){
  tryCatch({
    body <- 링크_mc[i] %>% read_html()
    
    cat(i, '페이지 수집 중입니다.\n')
    
    댓글_mc.tmp <- body %>% 
      html_nodes("a.sd-commentcount") %>%
      html_text()
    
    if (length(댓글_mc.tmp) != 0) {
      댓글_mc <- append(댓글_mc, 댓글_mc.tmp)
    } else {
      댓글_mc <- append(댓글_mc,"0")
    }   
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) {
    cat(conditionMessage(e),"\n")
    env$ans[i] <- "error"
  }
  )
}
댓글_mc <- 댓글_mc %>% as.integer()

# 에러 구간 "0" 집어넣기
er <- grep("error",env$ans)
for (i in 1:length(er)){
  댓글_mc <- append(댓글_mc,c(0),after = er[i]-1)
}

n4g_mc <- data.frame(제목_mc,
                       관심도_mc,
                       날짜_mc,
                       종류_mc,
                       출처_mc,
                       링크_mc,
                       댓글_mc)

names(n4g_mc) <- c("제목", "관심도", "날짜", "종류", "출처", "링크", "댓글")
n4g_mc %>% str()

# head + 본문
n4g_mc <- bind_rows(head_n4g_mc,n4g_mc)
n4g_mc %>% dim()

write.csv(n4g_mc, file = "D:/대학원/논문/소논문/텍스트마이닝 생존분석/n4g_mc.csv", row.names=FALSE)


## n4g – league of legends
# RSelenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()
head <- "https://n4g.com/channel/"
keyword <- "league-of-legends"
end <- "?load=3"

target4 <- paste0(head,keyword,end)

remDr$navigate(target4)

Sys.sleep(time = 5)

n <- 300

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

head_제목_lol <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_text()

head_관심도_lol <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("span.temp-label") %>%
  html_text()

head_날짜_lol <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-timeago") %>%
  html_text()

head_종류_lol <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-meta") %>%
  html_nodes("a") %>%
  html_text() %>% matrix(ncol=2, byrow = TRUE) %>% as.data.frame() %>% select(V1) %>% as.vector()

head_출처_lol <- body %>% 
  html_nodes("div.col-sm-12") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-meta") %>%
  html_nodes("a") %>%
  html_text() %>% matrix(ncol=2, byrow = TRUE) %>% as.data.frame() %>% select(V2) %>% as.vector()

# 본문
링크_lol <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_attr("href")
링크_lol <- paste0("https://n4g.com/",링크_lol)

제목_lol <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-content") %>%
  html_nodes("a.si-title") %>%
  html_text()

관심도_lol <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("span.temp-label") %>%
  html_text()

날짜_lol <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-timeago") %>%
  html_text()

종류_lol <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-meta") %>%
  html_nodes("a") %>%
  html_text() %>% matrix(ncol=2, byrow = TRUE) %>% as.data.frame() %>% select(V1) %>% as.vector()

출처_lol <- body %>% 
  html_nodes("div.col-sm-4") %>%
  html_nodes("div.si-wrap") %>%
  html_nodes("div.si-meta") %>%
  html_nodes("a") %>%
  html_text() %>% matrix(ncol=2, byrow = TRUE) %>% as.data.frame() %>% select(V2) %>% as.vector()

# head 댓글
head_댓글_lol <- c()

env = environment()
env$ans <- NULL
for (i in 1:length(head_링크_lol)){
  tryCatch({
    body <- head_링크_lol[i] %>% read_html()
    cat(i, '페이지 수집 중입니다.\n')
    head_댓글_lol.tmp <- body %>% 
      html_nodes("a.sd-commentcount") %>%
      html_text()
    if (length(head_댓글_lol.tmp) != 0) {
      head_댓글_lol <- append(head_댓글_lol, head_댓글_lol.tmp)
    } else {
      head_댓글_lol <- append(head_댓글_lol,"0")
    } 
    Sys.sleep(time = 1)
  }, error = function(e) {
    cat(conditionMessage(e),"\n")
    env$ans[i] <- "error"
  }
  )
}

head_댓글_lol <- head_댓글_lol %>% as.integer()

# 에러 구간 "0" 집어넣기
er <- grep("error",env$ans)
for (i in 1:length(er)){
  head_댓글_lol <- append(head_댓글_lol,c(0),after = er[i]-1)
}

head_n4g_lol <- data.frame(head_제목_lol,
                           head_관심도_lol,
                           head_날짜_lol,
                           head_종류_lol,
                           head_출처_lol,
                           head_링크_lol,
                           head_댓글_lol)

names(head_n4g_lol) <- c("제목", "관심도", "날짜", "종류", "출처", "링크", "댓글")
head_n4g_lol %>% str()


# 본문 댓글
댓글_lol <- c()

env = environment()
env$ans <- NULL
for (i in 1:length(링크_lol)){
  tryCatch({
    body <- 링크_lol[i] %>% read_html()
    cat(i, '페이지 수집 중입니다.\n')
    댓글_lol.tmp <- body %>% 
      html_nodes("a.sd-commentcount") %>%
      html_text()
    if (length(댓글_lol.tmp) != 0) {
      댓글_lol <- append(댓글_lol, 댓글_lol.tmp)
    } else {
      댓글_lol <- append(댓글_lol,"0")
    } 
    Sys.sleep(time = 0.5)
  }, error = function(e) {
    cat(conditionMessage(e),"\n")
    env$ans[i] <- "error"
  }
  )
}
댓글_lol <- 댓글_lol %>% as.integer()

# 에러 구간 "0" 집어넣기
er <- grep("error",env$ans)
for (i in 1:length(er)){
  댓글_lol <- append(댓글_lol,c(0),after = er[i]-1)
}

n4g_lol <- data.frame(제목_lol,
                        관심도_lol,
                        날짜_lol,
                        종류_lol,
                        출처_lol,
                        링크_lol,
                        댓글_lol)

names(n4g_lol) <- c("제목", "관심도", "날짜", "종류", "출처", "링크", "댓글")
n4g_lol %>% str()

# head + 본문
n4g_lol <- bind_rows(head_n4g_lol,n4g_lol)
n4g_lol %>% dim()

write.csv(n4g_lol, file = "D:/대학원/논문/소논문/텍스트마이닝 생존분석/n4g_lol.csv", row.names=FALSE)