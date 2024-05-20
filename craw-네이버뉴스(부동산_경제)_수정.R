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

# https://search.naver.com/search.naver?where=news&query=%EB%B6%80%EB%8F%99%EC%82%B0&sm=tab_opt&sort=1&photo=0&field=0&pd=3&ds=2023.01.01&de=2024.01.01&docid=&related=0&mynews=1&office_type=3&office_section_code=0&news_office_checked=&nso=so%3Add%2Cp%3Afrom20230101to20240101&is_sug_officeid=0&office_category=3&service_area=1

head <- "https://search.naver.com/search.naver?where=news&query="
searchword <- "부동산"
body1 <- "&sm=tab_opt&sort=1&photo=0&field=0&pd=3&ds="
시작일1 <- dot_ymd
body2 <- "&de="
종료일1 <- dot_ymd
경제언론사 <- "&docid=&related=0&mynews=1&office_type=3&office_section_code=0&news_office_checked=&nso=so%3Add%2Cp%3Afrom"
시작일2 <- ymd
to <- "to"
종료일2 <- ymd
body3 <- "&is_sug_officeid=0&office_category=3&service_area=1"


링크_부동산 <- c()

for (i in 1:length(dot_ymd)){
    링크_부동산.tmp <- paste0(head,searchword,body1,시작일1[i],body2,종료일1[i],경제언론사,시작일2[i],to,종료일2[i],body3)
    링크_부동산 <- append(링크_부동산,링크_부동산.tmp)
  }

링크_부동산 <- 링크_부동산 %>% unique()
length(링크_부동산)
end_time <- Sys.time()
링크_부동산 %>% head()


##cmd 관리자권한으로 실행
# cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

start_time1 <- Sys.time()

remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

## 링크 수집
time1 <- seq(from = 1, to = 4, by = 0.000001)
time2 <- seq(from = 3, to = 5, by = 0.000001)

link_re2324 <- c()

# 최신순, 100101-231231, 지면기사, 경제/it
for (i in 1:length(링크_부동산)){
  tryCatch({
remDr$navigate(링크_부동산[i])
    
cat(dot_ymd[i],"블록 링크 수집 중.\n")

frontpage <- remDr$getPageSource()[[1]]
body <- frontpage %>% read_html() 

# Scroll down to load all articles
last_height <- (remDr$executeScript("return document.documentElement.scrollHeight"))[[1]][1]

while (TRUE) {
  remDr$executeScript("window.scrollTo(0, document.documentElement.scrollHeight);")
  Sys.sleep(sample(time1,1))
  new_height <- (remDr$executeScript("return document.documentElement.scrollHeight"))[[1]][1]
  if (new_height == last_height) {
    break
  }
  last_height <- new_height
}

frontpage <- remDr$getPageSource()[[1]]
body <- frontpage %>% read_html() 

링크_부동산.tmp <- body %>% 
  html_nodes("div.info_group") %>%
  html_nodes("a.info") %>%
  html_attr("href")

링크_부동산.tmp <- 링크_부동산.tmp[grepl("^https://n.news.naver.com", 링크_부동산.tmp)]

link_re2324 <- append(link_re2324, 링크_부동산.tmp)

Sys.sleep(sample(time2,1))# 대기 시간을 추가하여 페이지가 완전히 로드될 때까지 기다림
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}

link_re2324 %>% tail()

link_re2324 <- link_re2324 %>% unique()

link_re2324_df <- link_re2324 %>% as_tibble()

write.xlsx(link_re2324_df, file = "D:/대학원/논문/소논문/부동산_감정사전/link_re2324_df.xlsx", rowNames=FALSE, fileEncoding = 'cp949')

end_time1 <- Sys.time()


# element 수집
start_time2 <- Sys.time()

sp = 10
spn = ceiling((length(link_re2324) / sp))
link_re_sp <- link_re2324 %>% split(rep(1:spn, each=sp))

time1 <- seq(from = 0.1, to = 0.5, by = 0.000001)
time2 <- seq(from = 0.1, to = 2, by = 0.000001)

부동산_제목 <- c()
부동산_날짜 <- c()
부동산_언론사 <- c()
부동산_본문 <- c()

부동산_df <- tibble()
부동산_본문df <- tibble()

for (i in 1:length(link_re_sp)){
  for (j in 1:sp){
    tryCatch({
      cat(i, '페이지 수집 중 입니다.\n') 
      
      링크.tmp <- link_re_sp[[i]][j]
      body <- 링크.tmp %>% read_html()
      
      부동산_제목.tmp <- body %>% 
        html_nodes("div.media_end_head_title") %>%
        html_nodes("h2") %>%
        html_nodes("span") %>% 
        html_text()
      
      부동산_날짜.tmp <- body %>% 
        html_nodes("div.media_end_head_info_datestamp_bunch") %>%
        html_nodes("span") %>% 
        html_text() %>% tail(1)
      
      부동산_언론사.tmp <- body %>% 
        html_nodes("a.media_end_head_top_logo") %>%
        html_nodes("img:nth-child(1)") %>% 
        html_attr("alt")
      
      부동산_본문.tmp <- body %>% 
        html_nodes("article.go_trans") %>%
        html_text() 
      
      cat('데이터 프레임', j, 'th 행을 구성하는 중.\n') 
      
      부동산_df <- rbind(부동산_df,tibble(부동산_제목.tmp,부동산_날짜.tmp,부동산_언론사.tmp,링크.tmp))
      
      부동산_본문df <- rbind(부동산_본문df,tibble(부동산_본문.tmp,링크.tmp))
      
      Sys.sleep(time = sample(time1,1))
      
    }, error = function(e) cat("불러올 수 없습니다!\n"))
    
  }
}


names(부동산_df) <- c("제목", "날짜", "언론사", "링크")

names(부동산_본문df) <- c("본문", "링크")

write.xlsx(부동산_df, file = "D:/대학원/논문/소논문/부동산_감정사전/부동산_수정2324_df.xlsx", rowNames=FALSE, fileEncoding = 'cp949')

부동산_본문df <- cbind(부동산_df,부동산_본문df)

write.xlsx(부동산_본문df, file = "D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_수정2324_df.xlsx", rowNames=FALSE, fileEncoding = 'cp949')

end_time2 <- Sys.time()


end_time1 - start_time1
end_time2 - start_time2

