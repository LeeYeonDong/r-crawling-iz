install.packages("jsonlite")
install.packages("tidyverse")
install.packages("httr")
install.packages("stringr")
install.packages("dplyr")
install.packages("rvest")
install.packages("reshape")
install.packages("urltools")
install.packages("RSelenium")
library(jsonlite)
library(reshape)
library(tidyverse)
library(httr)
library(stringr)
library(dplyr)
library(rvest)
library(urltools)
library(RSelenium)

Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_271')



### wall street journal - iframe으로 막힘

res_wsj <- GET(url = 'https://www.wsj.com/search/term.html',
               query = list(keywords = 'Biden'))

페이지수.wsj.tmp <- res_wsj %>%
  read_html() %>% 
  html_nodes('div.WSJTheme--SearchResultPagination--2_RDsqbb ') %>%
  html_nodes('span') %>%  
  html_text() 

n <- 174

제목_wsj <- c()
날짜_wsj <- c()

for(i in 1:n){
  tryCatch({
    
    res_wsj <- GET(url = 'https://www.wsj.com/search/',
                  query = list(keywords = 'Biden',
                               page = 1))
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_wsj), '입니다.\n')
    
    
    ## 제목
    제목.wsj.tmp <- res_wsj %>%
      read_html() %>% 
      html_nodes('iframe') %>%
      html_nodes('div.headline-container') %>%
      html_nodes('h3.headline') %>%  
      html_nodes('a') %>% 
      html_text() 
       
    제목_wsj <- append(제목_wsj,제목.wsj.tmp)
    
    ## 날짜
    날짜.wsj.tmp <- res_wsj %>%
      read_html() %>% 
      html_nodes('div.article-info') %>%
      html_nodes('ul') %>%
      html_nodes("li:nth-child(1)") %>%
      html_nodes('time.date-stamp-container') %>%
      html_text() 
    
    if (length(날짜.wsj.tmp) == 0) {
      날짜_wsj <- append(날짜_wsj, "수동확인")
    } else {
      날짜_wsj <- append(날짜_wsj, 날짜.wsj.tmp)
    }
    
    Sys.sleep(time = 0.1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

##cmd
#cd C:\r-selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

##selenium
start_time <- Sys.time()

remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

페이지_wsj <- c()

n <- 174

for(i in 1:n){
main <- "https://www.wsj.com/search?&query="
검색어 <- "biden"
페이지 <- "&mod=searchresults_viewallresults&page="

페이지.wsj.tmp <- paste(main, 검색어,페이지,i,sep = "")
페이지_wsj <- append(페이지_wsj,페이지.wsj.tmp)
}


제목_wsj <- c()

for(i in 1:n){
  tryCatch({
    remDr$navigate(페이지_wsj[i])
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    frontpage <- remDr$getPageSource()[[1]]
    body <- frontpage %>% read_html() 
    
    ## 제목
    제목.wsj.tmp <- body %>%
      html_nodes('h3') %>%
      html_nodes('a') %>%
      html_text() 
    
    제목_wsj <- append(제목_wsj,제목.wsj.tmp)
    
    Sys.sleep(time = 5)

  }, error = function(e) cat("불러올 수 없습니다!\n"))
}
    
## 하나의 데이터프레임으로 합침
paper_wsj <- data.frame(제목_wsj)
write.csv(제목_wsj, file = "D:/대학원/선형모형론/2학기/term/wsj_df.csv", row.names=FALSE) 

write.table(제목_wsj, "D:/대학원/선형모형론/2학기/term/wsj.txt", 
            sep = ",", 
            row.names = FALSE, 
            quote = FALSE, 
            append = TRUE, 
            na = "NA")


### Washington Post

##cmd
#cd C:\r-selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445


### RSelenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

main <- "https://www.washingtonpost.com/newssearch/?query="
검색어 <- "biden"
mid1 <- "&btn-search=&sort=Relevance&datefilter=All%20Since%202005&startat=0#top"

target <- paste(main, 검색어, sep = "")
target <- paste(target, mid1, sep = "")

remDr$navigate(target)


## 제목 날짜
34487/20
n <- 1725

제목_wp <- c()
날짜_wp <- c()

for(i in 1:n){
  tryCatch({
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    frontpage <- remDr$getPageSource()[[1]]
    body <- frontpage %>% read_html() 

    # 제목
    제목.wp.tmp <- body %>% 
      html_nodes("div.pb-feed-headline") %>%
      html_nodes("p") %>%
      html_nodes("a.ng-binding") %>%
      html_text() 
    
    제목_wp <- append(제목_wp,제목.wp.tmp)
    
    
    # 날짜
    날짜.wp.tmp <- body %>% 
      html_nodes("div.pb-feed-byline") %>%
      html_nodes("span.pb-timestamp") %>%
      html_text() 
    
    날짜_wp <- append(날짜_wp,날짜.wp.tmp)
    
    
    element <- remDr$findElement("css", "li.pagination-next >
                                 a.ng-binding")
    element$clickElement()
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

## 하나의 데이터프레임으로 합침
날짜_wp <-날짜_wp[1:length(제목_wp)]
paper_wp <- data.frame(제목_wp,날짜_wp)

write.csv(paper_wp, file = "D:/대학원/선형모형론/2학기/term/wp_df.csv", row.names=FALSE) 

write.table(제목_wp, "D:/대학원/선형모형론/2학기/term/wp.txt", 
               sep = ",", 
               row.names = FALSE, 
               quote = FALSE, 
               append = TRUE, 
               na = "NA") 

end_time <- Sys.time()
end_time - start_time


## 링크

링크_wp <- c()

for(i in 1:5){
  tryCatch({
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    frontpage <- remDr$getPageSource()[[1]]
    body <- frontpage %>% read_html() 
    
    링크.wp.tmp <- body %>% 
      html_nodes("a.ng-binding") %>%
      html_attr("href")
    
    링크.wp.tmp <- 링크.wp.tmp[1:20]
    링크_wp <- append(링크_wp,링크.wp.tmp)
    
    element <- remDr$findElement("css", "li.pagination-next >
                                 a.ng-binding")
    element$clickElement()
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}



## GET/POST 안됨
for(i in 0:n){
  tryCatch({
    
    res_wp <- GET(url = 'https://www.washingtonpost.com/newssearch/',
                   query = list(query = 'biden',
                                sort = "Relevance",
                                datefilter = "All Since 2005",
                                startat = 0,
                                spellcheck = ""))
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_wp), '입니다.\n')
    
    
    ## 제목 수집
    
    제목.wp.tmp <- res_wp %>%
      read_html() %>% 
      html_nodes('div.pb-results-container') %>% 
      html_nodes('a.ng-binding') %>% 
      html_text() 
    
    if (length(제목.wsj.tmp) == 0) {
      제목_wsj <- append(제목_wsj, "수동확인")
    } else {
      제목_wsj <- append(제목_wsj, 제목.wsj.tmp)
    }
    
    Sys.sleep(time = 0.1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}

write.csv(제목_wp, file = "D:/대학원/선형모형론/2학기/term/제목_wp.csv", row.names=FALSE) 


