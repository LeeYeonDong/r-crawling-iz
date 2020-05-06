
##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445


### 다음뉴스 GET

링크_daum <- c() #벡터생성


###
searchword <- "조국"
시작일 <- "20191231000000" #날짜마다 바꿔줘야함
종료일 <- "20191231235959"

n <- 80 #최대 제공 페이지 수

for(i in 1:n){
  tryCatch({
    res_daum <- GET(url = "https://search.daum.net/search",
                    query = list(w = "news",
                                 sort = "recency",
                                 q = searchword,
                                 cluster = "n",
                                 DA = "STC",
                                 dc = "STC",
                                 pg = "1",
                                 r = "1",
                                 p = i,
                                 rc = "1",
                                 at = "more",
                                 sd = 시작일,
                                 ed = 종료일,
                                 period = "u"))
    
    cat('현재', i, '페이지 수집 중! 상태코드는', status_code(x = res_daum), '입니다.\n')
    
    
    ##링크
    
    링크.tmp <- res_daum %>% 
      read_html() %>% 
      html_nodes("a.f_nb") %>%
      html_attr("href") %>%
      unique()
    
    
    링크_daum <- append(링크_daum,링크.tmp) 
    
    
    
    Sys.sleep(time = 0.1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
  
}

## "?w=news" 포함링크제거 & 중복링크제거
length(링크_daum)

trim <- grep("?w=news",링크_daum)
rm(list=링크_daum[trim])


링크_daum <- 링크_daum %>% unique()
length(링크_daum)



##cmd
#cd C:\r_selenium
#java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.11.0.jar -port 4445



## RSelnium

remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()



### 제목 날짜 본문 주소

제목_daum <- c()
날짜_daum <- c()
본문_daum <- c()
주소_daum <- c()
댓글개수_daum <- c()


length(본문_daum)



for (i in 1:length(링크_daum)){
  tryCatch({
    remDr$navigate(링크_daum[i])
    body <- remDr$getPageSource()[[1]]
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    body <- body %>% read_html()
    
    
    ##제목
    
    제목.tmp <- body %>% 
      html_nodes("h3.tit_view") %>%
      html_text() 
    
    if (length(제목.tmp) == 0) {
      제목_daum <- append(제목_daum, "수동확인")
    } else {
      제목_daum <- append(제목_daum, 제목.tmp)
    }
    
    
    ##날짜
    
    날짜.tmp <- body %>% 
      html_nodes("span.info_view") %>%
      html_nodes("span:nth-child(2)") %>% 
      html_text() 
    
    날짜.temp <-날짜.temp[length(날짜.temp)]
    
    if (length(날짜.tmp) == 0) {
      날짜_daum <- append(날짜_daum, "수동확인")
    } else {
      날짜_daum <- append(날짜_daum, 날짜.tmp)
    }
    
    
    ##본문
    
    본문.tmp <- body %>% 
      html_nodes("div.article_view") %>%
      html_nodes("p") %>% 
      html_text() 
    
    본문.tmp <- 본문.tmp %>% paste(collapse = " ")
    
    if (length(본문.tmp) == 0) {
      본문_daum <- append(본문_daum, "수동확인")
    } else {
      본문_daum <- append(본문_daum, 본문.tmp)
    }
    
    
    
    ##댓글개수
    
    댓글개수.tmp <- body %>% 
      html_nodes("span.alex-count-area") %>%
      html_text() 
    
    if (length(댓글개수.tmp) == 0) {
      댓글개수_daum <- append(댓글개수_daum, "수동확인")
    } else {
      댓글개수_daum <- append(댓글개수_daum, 댓글개수.tmp)
    }
    
    
    ##주소
    
    주소_daum <- append(주소_daum, 링크_daum[i])
    
    Sys.sleep(time = 0.1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}


본문_daum <- str_replace_all(본문_daum,"&","")
본문_daum <- str_replace_all(본문_daum,"ⓒ","")

본문_daum <- str_replace_all(본문_daum,"무단전재 및 재배포 금지","")


날짜_daum <- str_replace_all(날짜_daum,"입력","")
날짜_daum <- str_replace_all(날짜_daum,"수정","")


## 하나의 데이터프레임으로 합침
paper_daum <- data.frame(제목_daum, 본문_daum, 주소_daum, 날짜_daum, 댓글개수_daum)


write.csv(paper_daum, file = "D:/성재/크롤링/paper_daum_조국.csv", row.names=FALSE)




