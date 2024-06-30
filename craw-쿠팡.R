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


# 검색어
head1 <- "https://m.coupang.com/nm/search?q="

key_cat <- tibble(
  category1 = c(rep("사치품 (Luxury Goods)", 15), rep("생필품 (Necessities)", 15)),
  category2 = c(
    rep("고급 의류 (Luxury Clothing)", 5),
    rep("고가의 전자제품 (High-End Electronics)", 5),
    rep("명품 액세서리 (Luxury Accessories)", 5),
    rep("식료품 (Groceries)", 5),
    rep("청소용품 (Cleaning Supplies)", 5),
    rep("주방용품 (Kitchen Supplies)", 5)
  ),
  keyword = c(
    "명품 드레스", "디자이너 수트", "명품 코트", "브랜드 셔츠", "디자이너 청바지",
    "프리미엄 스마트폰", "고급 노트북", "하이엔드 TV", "프리미엄 오디오 시스템", "고급 카메라",
    "명품 시계", "디자이너 가방", "고급 지갑", "명품 주얼리", "브랜드 선글라스",
    "신선 야채", "유기농 과일", "밀가루", "쌀", "식용유",
    "다목적 세정제", "세탁 세제", "주방 세정제", "변기 세정제", "청소 도구 세트",
    "냄비 세트", "칼 세트", "주방 용품 세트", "전기 밥솥", "믹서기"
  )
)


##cmd 관리자권한으로 실행
# cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

key_cat$link_cp <- paste0(head1, key_cat$keyword)

# 품목 링크 탐색
i = 1
j = 1
k = 1 
times <- seq(from = 1, to = 3, by = 0.001)
tb_cp <- tibble()

for (i in 1:length(key_cat$link_cp)) {
  for (k in 1:10) {
    # 페이지 탐색 (에러 발생 시 무시)
    tryCatch({
      remDr$navigate(paste0(key_cat$link_cp[i],"&page=",k))
      Sys.sleep(sample(times, 1))
      
      # 괜찮습니다 버튼 클릭 (에러 발생 시 무시)
      try({
        괜찮 <- remDr$findElement(using = "xpath", "//*[@id='fullBanner']/div/div/a[2]") 
        괜찮$clickElement()
      }, silent = TRUE)
      
      Sys.sleep(sample(times, 1))
      
      # 품목 링크 수집
      frontpage <- remDr$getPageSource()[[1]]
      body <- frontpage %>% read_html() 
      
      link_rank <- body %>% 
        html_nodes("a.sdw-similar-product-go-to-sdp-click") %>% 
        html_attr("href")
      link_rank <- paste0("https://m.coupang.com", link_rank)
      link_rank <- grep("sourceType=search", link_rank, value = TRUE)
      link_rank <- sub("\\?.*", "/brand-sdp/reviews/detail", link_rank)
      
      # 링크 탐색
      for (j in 1:length(link_rank)) {
        tryCatch({
          remDr$navigate(link_rank[j])
          
          # Scroll down to load all comments
          last_height <- (remDr$executeScript("return document.documentElement.scrollHeight"))[[1]][1]
          
          while (TRUE) {
            webElem <- remDr$findElement("css", "body")
            webElem$sendKeysToElement(list(key = "end"))
            Sys.sleep(sample(times,1))
            new_height <- (remDr$executeScript("return document.documentElement.scrollHeight"))[[1]][1]
            if (new_height == last_height) {
              cat(i,"번째 키워드",j,'번째 링크에서 페이지 스크롤 중 입니다.\n')
              break
            }
            last_height <- new_height
          }
          
          Sys.sleep(sample(times, 1))
          
          frontpage <- remDr$getPageSource()[[1]]
          body <- frontpage %>% read_html() 
          
          본문 <- body %>% 
            html_nodes("div.review-content") %>% 
            html_text()
          
          # 상품명
          상품명 <- body %>% 
            html_nodes("p.review-item-name") %>% 
            html_text() %>% head(length(본문))
          
          # 작성자
          작성자 <- body %>% 
            html_nodes("p.reviewer-name") %>% 
            html_text() %>% head(length(본문))
          
          # 날짜
          날짜 <- body %>% 
            html_nodes("span.review-date") %>% 
            html_text() %>% head(length(본문))
          
          tb_cp.tmp <- tibble(본문, 상품명, 작성자, 날짜)
          tb_cp.tmp$keyword <- key_cat$keyword[i]
          tb_cp.tmp$category1 <- key_cat$category1[i]
          tb_cp.tmp$category2 <- key_cat$category2[i]
          
          tb_cp <- bind_rows(tb_cp, tb_cp.tmp)
        }, error = function(e) {
          cat("에러가 발생했습니다:", conditionMessage(e), "\n")
        })
      }
    }, error = function(e) {
      cat("페이지를 탐색하는 중 에러가 발생했습니다:", conditionMessage(e), "\n")
    })
  }
}

tb_cp %>% print()
tb_cp %>% glimpse()

# 필요한 패키지 로드
library(readr)

# UTF-8 인코딩으로 데이터 저장
write_csv(tb_cp, "D:/대학원/상담/커뮤니케이션학과/쇼핑몰/tb_cp0610_utf8.csv")

# UTF-8로 저장한 파일을 다시 읽고 CP949로 저장
tb_cp_utf8 <- read_csv("D:/대학원/상담/커뮤니케이션학과/쇼핑몰/tb_cp0610_utf8.csv")

# CP949 인코딩으로 저장
write.csv(tb_cp_utf8,  "D:/대학원/상담/커뮤니케이션학과/쇼핑몰/tb_cp0610.csv", row.names = FALSE, fileEncoding = "CP949")

  