

## 사전 벡터 공간

링크_nfin <- c()
언론사_nfin <- c()



## 링크 수집하기

날짜s <- as.Date("2019-12-01")
날짜e <- as.Date("2019-12-31")
날짜se <- seq(from = 날짜s, to = 날짜e, by =1)

날짜 <- str_replace_all(날짜se,"-","")



링크_nfin <- c()
언론사_nfin <- c()


for (j in 1:length(날짜)){
  for(i in 1:100){
    tryCatch({      
      res_nfin <- GET(url = "https://finance.naver.com/news/news_list.nhn",
                      query = list(mode = "LSS2D",
                                   section_id = "101",
                                   section_id2 = "258",
                                   date = 날짜[1],
                                   page = i))
      
      
      cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_yui), '입니다.\n')
      
      
      ##링크
      
      링크_nfin.tmp <- res_nfin %>% 
        read_html(encoding = "CP949") %>%
        html_nodes("li.newsList") %>%
        html_nodes("dl") %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        unique()
      
      if (length(링크_nfin.tmp) == 0) {
        링크_nfin <- append(링크_nfin, "수동확인")
      } else {
        링크_nfin <- append(링크_nfin, 링크_nfin.tmp)
      } 
      
      
      
      ##언론사
      
      언론사_nfin.tmp <- res_nfin %>% 
        read_html(encoding = "CP949") %>%
        html_nodes("span.press") %>% 
        html_text()
      
      
      if (length(언론사_nfin.tmp) == 0) {
        언론사_nfin <- append(언론사_nfin, "수동확인")
      } else {
        언론사_nfin <- append(언론사_nfin, 언론사_nfin.tmp)
      } 
      
      
      Sys.sleep(time = 1)
      
    }, error = function(e) cat("불러올 수 없습니다!\n"))
  }
}


링크_nfin_t <- grep("수동확인",링크_nfin)

언론사_nfin



##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445


#### RSelenium

remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()


제목_nfin <- c()
날짜_nfin <- c()
본문_nfin <- c()
주소_nfin <- c()

for (i in 1:length(링크_nfin)){
  tryCatch({
    
    remDr$navigate(링크_nfin[i])
    body <- remDr$getPageSource()[[1]]
    body <- body %>% read_html("CP949")
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    
    
    ## 제목
    
    제목.nfin.tmp <- body %>% 
      html_nodes("div.article_info") %>% 
      html_nodes("h3") %>% 
      html_text()
    
    
    if (length(제목.각키.tmp) == 0) {
      제목_nfin <- append(제목_nfin, "수동확인")
    } else {
      제목_nfin <- append(제목_nfin, 제목.nfin.tmp)
    }
    
    
    
    ## 날짜
    
    날짜.nfin.tmp <- body %>% 
      html_nodes("div.article_sponsor") %>% 
      html_nodes("span.article_date") %>% 
      html_text()
    
    
    if (length(날짜.각키.tmp) == 0) {
      날짜_nfin <- append(날짜_nfin, "수동확인")
    } else {
      날짜_nfin <- append(날짜_nfin, 날짜.nfin.tmp)
    }
    
    
    
    ## 본문
    
    본문.nfin.tmp <- body %>% 
      html_nodes("div#articleCont") %>% 
      html_text()
    
    
    if (length(본문.각키.tmp) == 0) {
      본문_nfin <- append(본문_nfin, "수동확인")
    } else {
      본문_nfin <- append(본문_nfin, 본문.nfin.tmp)
    }
    
    
    
    ## 주소(URL)
    
    주소_nfin <- append(주소_nfin, 링크_nfin[i])
    
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}



본문_nfin <- str_replace_all(본문_nfin,"@+","")
본문_nfin <- str_replace_all(본문_nfin,"헤럴드경제","")
본문_nfin <- str_replace_all(본문_nfin,"무단 전재 및 재배포 금지","")
본문_nfin <- str_replace_all(본문_nfin,"＜ⓒ종합 경제정보 미디어 이데일리 - 무단전재 & 재배포 금지＞","")
본문_nfin <- str_replace_all(본문_nfin,"<ⓒ경제를 보는 눈, 세계를 보는 창 아시아경제 무단전재 배포금지>","")
본문_nfin <- str_replace_all(본문_nfin,"저작권자 ⓒ 서울경제, 무단 전재 및 재배포 금지","")
본문_nfin <- str_replace_all(본문_nfin,"/+","")
본문_nfin <- str_replace_all(본문_nfin,"^▶","")
본문_nfin <- str_replace_all(본문_nfin,"Copyrights","")
본문_nfin <- str_replace_all(본문_nfin,"ⓒ","")
본문_nfin <- str_replace_all(본문_nfin,"&","")


## 데이터프레임으로 합치기 및 csv파일로 저장

df_nfin <- data.frame(제목_nfin, 언론사_nfin, 날짜_nfin, 본문_nfin, 주소_nfin)
write.csv(df_nfin, file = "D:/df_nfin(네이버 증권 뉴스).csv", row.names=FALSE)

