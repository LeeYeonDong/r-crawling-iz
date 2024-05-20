library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)
library(tidyverse)
library(openxlsx)
library(data.table)
library(readr)

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
yyyy <- c(2014:2023)
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

head <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query="
searchword <- "S24"
body1 <- "&sort=1&photo=0&field=0&pd=3&ds="
시작일1 <- dot_ymd
body2 <- "&de="
종료일1 <- dot_ymd
경제언론사 <- "&mynews=1&office_type=3&office_section_code=0&news_office_checked=&office_category=3&service_area=1&nso=so:dd,p:from"
시작일2 <- ymd
to <- "to"
종료일2 <- ymd
body3 <- ",a:all&start="
seq <- seq(from = 1, by = 10, length.out = n)

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



# 네이버 뉴스 링크 - get
time1 <- seq(from = 1, to = 2, by = 0.000001)
time2 <- seq(from = 10, to = 20, by = 0.000001)
time3 <- seq(from = 30, to = 60, by = 0.000001)

dot_ymd[grep(yyyy[k], dot_ymd)][1]

# 벡터 분할
sp = 10
spn = ceiling((length(dot_ymd) / sp))
dot_ymd_sp <- dot_ymd %>% split(rep(1:spn, each=sp))

dot_ymd_sp[[1]][]

start_time1 <- Sys.time()

link_re <- c()

for (i in 1:length(dot_ymd_sp)){
  for (k in 1:sp){
  for (j in 1:length(seq)){
      tryCatch({
res_news <- GET(url = 'https://search.naver.com/search.naver', query = list(where = "news",sm = "tab_pge",query = "부동산",sort=1,photo=0,field=0,pd=3,
                  ds=dot_ymd_sp[[i]][k],
                  de=dot_ymd_sp[[i]][k],
                  mynews=1,office_type=3, office_section_code=0,office_category=3,service_area=1,start=seq[j]))

cat(dot_ymd_sp[[i]][k], j,'th 페이지 링크 수집중. 상태코드는', status_code(x = res_news), '입니다.\n') 


body <- res_news %>% read_html()

링크_부동산.tmp <- body %>% 
  html_nodes("div.info_group") %>%
  html_nodes("a") %>%
  html_attr("href")

링크_부동산.tmp <- 링크_부동산.tmp[grep("https://n.news.naver.com", 링크_부동산.tmp)]

if (length(링크_부동산.tmp) != 0) {
  link_re <- append(link_re,링크_부동산.tmp)
} else {
  link_re <- append(link_re,"수동확인")
}    

Sys.sleep(time = sample(time1,1))

  }, error = function(e) cat("불러올 수 없습니다!\n"))
    
  }
  Sys.sleep(time = sample(time2,1))
  }
  Sys.sleep(time = sample(time3,1))
}

link_re <- link_re %>% unique()

end_time1 <- Sys.time()

end_time1 - start_time1


write.csv(link_re, file = "D:/대학원/논문/부동산_토픽모델링/link_re.csv", row.names=FALSE)


# 네이버 뉴스 링크
len <- seq(from = 1, to = length(링크_부동산), by = 100)

start_time1 <- Sys.time()

link_re <- c()

for (j in 1:length(len)){
for (i in len[j]:len[j+1]){
  tryCatch({
    cat(i, '페이지 링크 수집 중 입니다.\n') 
    
    body <- 링크_부동산[i] %>% read_html()
    
    링크_부동산.tmp <- body %>% 
      html_nodes("div.info_group") %>%
      html_nodes("a") %>%
      html_attr("href")
    
    링크_부동산.tmp <- 링크_부동산.tmp[grep("https://n.news.naver.com", 링크_부동산.tmp)]

    if (length(링크_부동산.tmp) != 0) {
      link_re <- append(link_re,링크_부동산.tmp)
    } else {
      link_re <- append(link_re,"수동확인")
    }    
    
    Sys.sleep(time = sample(time1,1))
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}
  Sys.sleep(time = sample(time2,1))
  cat(j,"th 블록 링크 수집 중.\n")
}

link_re <- link_re %>% unique()

end_time1 <- Sys.time()

end_time1 - start_time1




# element 수집
# install.packages("data.table")

link_re <- fread("D:/대학원/논문/소논문/부동산_토픽모델링/link_re.csv")
# encoding = "UTF-8" 옵션있음

link_re <- link_re %>% as_tibble
link_re <- link_re$x

time1 <- seq(from = 0.1, to = 0.5, by = 0.000001)
time2 <- rep(c(0,0.5), c(20,1))

start_time2 <- Sys.time()

부동산_제목 <- c()
부동산_날짜 <- c()
부동산_언론사 <- c()
부동산_본문 <- c()

부동산_df <- tibble()
부동산_본문df <- tibble()


for (i in 1:length(link_re)){
  tryCatch({
    cat(i, '페이지 수집 중 입니다.\n') 
    
    body <- link_re[i] %>% read_html()
    
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

    부동산_df <- rbind(부동산_df,tibble(부동산_제목.tmp,부동산_날짜.tmp,부동산_언론사.tmp,link_re[i]))
        
    부동산_본문df <- rbind(부동산_본문df,tibble(부동산_본문.tmp,link_re[i]))
    
    Sys.sleep(time = sample(time1,1))
    Sys.sleep(time = sample(time2,1))
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
      
    }


names(부동산_df) <- c("제목", "날짜", "언론사", "링크")

names(부동산_본문df) <- c("본문", "링크")

write.xlsx(부동산_df, file = "D:/대학원/논문/소논문/부동산_감정사전/부동산_수정_df.xlsx", rowNames=FALSE, fileEncoding = 'cp949')

write.xlsx(부동산_본문df, file = "D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_수정_df.xlsx", rowNames=FALSE, fileEncoding = 'cp949')

end_time2 <- Sys.time()

end_time2 - start_time2

부동산_df %>% head()
부동산_본문_추가df <- cbind(부동산_df ,부동산_본문df)
부동산_본문_추가df <- 부동산_본문_추가df %>% 
  select(-6)
부동산_본문_추가df %>% dim()
부동산_본문_추가df %>% glimpse()

write.xlsx(부동산_본문_추가df, file = "D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_df.xlsx", rowNames=FALSE, fileEncoding = 'cp949')


부동산_본문_추가df$날짜 %>% head()

# 연도별로 데이터 프레임 나누기
부동산_본문_추가df$year <- str_extract(부동산_본문_추가df$날짜, "\\d{4}")

# split 함수를 사용하여 연도별로 데이터프레임 분할
list_of_dfs <- split(부동산_본문_추가df, 부동산_본문_추가df$year)

list_of_dfs %>% names()
list_of_dfs %>% glimpse()

# list_of_dfs는 연도별로 분할된 데이터 프레임의 리스트입니다.
# 각 데이터 프레임을 해당 연도의 이름으로 Excel 파일로 저장
for (year in names(list_of_dfs)) {
  # 파일 경로 및 이름 설정
  file_path <- paste0("D:/대학원/논문/소논문/부동산_감정사전/부동산_본문_추가_", year, "_df.xlsx")
  
  # 데이터 프레임을 Excel 파일로 저장
  write.xlsx(list_of_dfs[[year]], file = file_path, rowNames = FALSE)
}
