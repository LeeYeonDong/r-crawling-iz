library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)
library(tidyverse)
library(openxlsx)

Sys.setenv("http_proxy"="")
Sys.setenv("no_proxy"=TRUE)
Sys.setenv("no_proxy"=1)

# 1 R 상단 메뉴에서 Tools Global Options-Packages를 클릭한다.
# 2 Use Internet Explorer library/proxy for HTTP 체크를 해제한다.
# 3 .Renviron을 컴퓨터에서 찾아야 하는데 찾을수 없었고 file.edit('~/.Renviron') 명령을 입력해 설정해주었다.(보통 내문서에 존재.)
# 4 여기에다가 다음 코드를 입력하면 정상적으로 작동하게 된다.
# options(internet.info = 0)
# http_proxy="http://user_id:password@your_proxy:your_port"


start_time <- Sys.time()

# 날짜 생성
yyyy <- c(2023:2024)
mm <- c("01","02","03","04","05","06","07","08","09","10","11","12")
dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")

dot_ymd <- c()

for (k in 1:length(yyyy)){
  for (i in 1:length(mm)){
    for (j in 1:length(dd)){
    dot_ymd.tmp <- paste0(yyyy[k],".",mm[i],".",dd[j])
    dot_ymd <- append(dot_ymd,dot_ymd.tmp)
}}}

ymd <- c()

for (k in 1:length(yyyy)){
  for (i in 1:length(mm)){
    for (j in 1:length(dd)){
      ymd.tmp <- paste0(yyyy[k],mm[i],dd[j])
      ymd <- append(ymd,ymd.tmp)
    }}}


# 링크 만들기_부동산
n = 30

# https://search.naver.com/search.naver?where=news&query=S24&sm=tab_opt&sort=1&photo=0&field=0&pd=3&ds=2024.01.01&de=2024.01.01&docid=&related=0&mynews=1&office_type=0&office_section_code=0&news_office_checked=&nso=so%3Add%2Cp%3Afrom20240101to20240101&is_sug_officeid=0&office_category=0&service_area=0

head <- "https://search.naver.com/search.naver?where=news&query="
searchword <- "S24"
body1 <- "&sm=tab_opt&sort=1&photo=0&field=0&pd=3&ds="
시작일1 <- dot_ymd
body2 <- "&de="
종료일1 <- dot_ymd
경제언론사 <- "&docid=&related=0&mynews=1&office_type=0&office_section_code=0&news_office_checked=&nso=so%3Add%2Cp%3Afrom"
시작일2 <- ymd
to <- "to"
종료일2 <- ymd
body3 <- ",a:all&start="


링크_부동산 <- c()

for (i in 1:length(dot_ymd)){
  for (j in 1:length(seq)){
    링크_부동산.tmp <- paste0(head,searchword,body1,시작일1[i],body2,종료일1[i],경제언론사,시작일2[i],to,종료일2[i],body3,seq[j])
    링크_부동산 <- append(링크_부동산,링크_부동산.tmp)
  }}

링크_부동산 <- 링크_부동산 %>% unique()
length(링크_부동산)
end_time <- Sys.time()
링크_부동산 %>% head()

end_time - start_time

##cmd 관리자권한으로 실행
# cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()


remDr$navigate("https://search.naver.com/search.naver?where=news&query=S24&sm=tab_opt&sort=1&photo=0&field=0&pd=3&ds=2024.01.01&de=2024.01.01&docid=&related=0&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so%3Add%2Cp%3Afrom20240101to20240101&is_sug_officeid=0&office_category=0&service_area=1")

frontpage <- remDr$getPageSource()[[1]]
body <- frontpage %>% read_html() 

# Scroll down to load all articles
last_height <- (remDr$executeScript("return document.documentElement.scrollHeight"))[[1]][1]
while (TRUE) {
  remDr$executeScript("window.scrollTo(0, document.documentElement.scrollHeight);")
  Sys.sleep(3)
  new_height <- (remDr$executeScript("return document.documentElement.scrollHeight"))[[1]][1]
  if (new_height == last_height) {
    break
  }
  last_height <- new_height
}

# 대기 시간을 추가하여 페이지가 완전히 로드될 때까지 기다림
Sys.sleep(5) # 필요한 경우 시간 조절
