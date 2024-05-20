library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)


## 다음뉴스 링크 분석
# https://search.daum.net/search?w=news&nil_search=btn&DA=PGD&enc=utf8&cluster=y&cluster_page=1&q=S24&sort=accuracy&sd=20240101000000&ed=20240101235959&period=u&p=2


# 기간 : 최신순(24.1.1.) / 유형 : 전체 / 언론사 : 전체 / 제휴사 언론
 
# 날짜 생성
yyyy <- c(2024)
mm <- c("01","02","03","04","05","06","07","08","09","10","11","12")
dd <- c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31")
tail1 <- "000000"
tail2 <- "235959"

start_ymd <- c()

for (k in 1:length(yyyy)){
  for (j in 1:length(mm)){
    for (i in 1:length(dd)){
      start_ymd.tmp <- paste0(yyyy[k], mm[j] ,dd[i],tail1)
      start_ymd <- append(start_ymd, start_ymd.tmp)
    }}}

end_ymd <- c()

for (k in 1:length(yyyy)){
  for (j in 1:length(mm)){
    for (i in 1:length(dd)){
      end_ymd.tmp <- paste0(yyyy[k], mm[j], dd[i],tail2)
      end_ymd <- append(end_ymd, end_ymd.tmp)
    }}}


# 링크 생성
# https://search.daum.net/search?w=news&nil_search=btn&DA=PGD&enc=utf8&cluster=y&cluster_page=1&q=S24&sort=accuracy&sd=20240101000000&ed=20240101235959&period=u&p=2
head <- "https://search.daum.net/search?w=news&nil_search=btn&DA=PGD&enc=utf8&cluster=y&cluster_page=1&q="
keyword <- "S24"
body1 <- "&sort=accuracy&sd="
start_ymd
body2 <- "&ed="
end_ymd
body3 <- "&period=u&p="
page <- c(1:80)

link_s24 <- c()

for (i in 1:length(start_ymd)){
  for (j in 1:length(page)){
    link_s24.tmp <- paste0(head, keyword, body1, start_ymd[i], body2, end_ymd[i], body3, page[j])
    link_s24 <- append(link_s24,link_s24.tmp)
  }}



# article link
article_link <- c()

i <- 1 # 인덱스 초기화
while (i <= 10) { # (i <= length(link_s24))
  article_link_tmp <- link_s24[i] %>%
    read_html() %>%
    html_nodes('div.item-title') %>%
    html_nodes('a') %>% 
    html_attr("href")
  
  if (length(article_link_tmp) == 0) {
    # article_link_tmp 길이가 0이면 다음 i로 넘어감
    i <- i + 1
    next
  }
  
  cat(i,'번째 페이지에서 기사 링크를 수집 중.\n')
  
  Sys.sleep(time = sample(seq(from = 1, to = 5, by = 0.001),1))
  
  # 길이가 0이 아니면, article_link에 추가
  article_link <- append(article_link, article_link_tmp)
  i <- i + 1
}

article_link %>% tail()
article_link %>% head()


article_df <- tibble()

# article element
# 제목
'https://v.daum.net/v/20240101104210582' %>%
  read_html() %>% 
  html_nodes('h3.tit_view') %>%
  html_text() -> 제목

# 언론사
('https://v.daum.net/v/20240101104210582' %>%
  read_html() %>% 
  html_nodes('div.info_view') %>%
  html_nodes('span.txt_info') %>%
  html_text())[1] -> 언론사

# 날짜
'https://v.daum.net/v/20240101104210582' %>%
    read_html() %>% 
    html_nodes('div.info_view') %>%
    html_nodes('span.txt_info') %>%
    html_nodes('span.num_date') %>%
    html_text() %>% tail(1) -> 날짜

# 본문
'https://v.daum.net/v/20240101104210582' %>%
  read_html() %>% 
  html_nodes('div.article_view') %>%
  html_nodes('p') %>%
  html_text() %>% 
  head(-1) %>% paste(collapse = " ") -> 본문

article_df_tmp <- tibble(제목, 언론사, 날짜, 본문)
  
article_df <- bind_rows(article_df, article_df_tmp)
