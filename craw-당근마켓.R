
##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445


### RSelnium

remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()



###당근마켓

main <- "https://www.daangn.com/search/"
검색어 <- "에어프라이기"

target <- paste(main, 검색어, sep = "")
remDr$navigate(target)



###더보기

n <- 3030
frontpage <- remDr$getPageSource()[[1]]


for(i in 0:10){
  
  i <- i+1
  if(i==10) break()
  
  
  
  morebutton <- remDr$findElement(using = 'xpath', 
                                  value = '//*[@class="more-btn"]')
  morebutton$clickElement()
  
}



###링크

링크_dg <- c()

frontpage <- remDr$getPageSource()[[1]]
링크.tmp <- read_html(frontpage) 
링크.tmp <- 링크.tmp %>%  
  html_nodes("a.flea-market-article-link") %>%
  html_attr("href") %>%
  unique()

링크_dg <- paste0("https://www.daangn.com",링크.tmp)

링크_dg <- 링크_dg %>% unique()


####### case 1

제목_dg <- c()
가격_dg <- c()
본문_dg <- c()
조회수_dg <- c()
날짜_dg <- c()
주소_dg <- c()

for (i in 1:length(링크_dg)){
  
  tryCatch({
    remDr$navigate(링크_dg[i])
    body <- remDr$getPageSource()[[1]]
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    body <- body %>% read_html()
    
    ##제목
    
    제목.temp <- body %>% 
      html_nodes("#article-title") %>% 
      html_text()  
    
    
    if (length(제목.temp) == 0) 제목_dg <- append(제목_dg, "수동확인")
    else 제목_dg <- append(제목_dg, 제목.temp)
    
    
    ##가격
    
    가격.temp <- body %>% 
      html_nodes("#article-price") %>% 
      html_text()  
    
    if (length(가격.temp) == 0) 가격_dg <- append(가격_dg, "수동확인")
    else 가격_dg <- append(가격_dg, 가격.temp)
    
    
    ##본문
    
    본문.temp <- body %>% 
      html_nodes("#article-detail") %>% 
      html_text()  
    
    if (length(본문.temp) == 0) 본문_dg <- append(본문_dg, "수동확인")
    else 본문_dg <- append(본문_dg, 본문.temp)
    
    
    ##조회수
    
    조회수.temp <- body %>% 
      html_nodes("#article-category") %>% 
      html_text()  
    
    if (length(조회수.temp) == 0) 조회수_dg <- append(조회수_dg, "수동확인")
    else 조회수_dg <- append(조회수_dg, 조회수.temp)
    
    
    ##날짜
    
    날짜.temp <- body %>% 
      html_nodes("#article-counts") %>% 
      html_text()  
    
    if (length(날짜.temp) == 0) 날짜_dg <- append(날짜_dg, "수동확인")
    else 조회수_dg <- append(날짜_dg, 날짜.temp)
    
    
    ###주소
    주소_dg <- append(주소_dg, 링크_dg[i])
    
    Sys.sleep(time = 0.1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}


가격_dg <- gsub("\n", "", 가격_dg)
가격_dg <- gsub(" ", "", 가격_dg)
본문_dg <- gsub("\n", "", 본문_dg)

조회수_dg <- gsub("\n", "", 조회수_dg)
조회수_dg <- gsub("\n", "", 조회수_dg)

날짜_dg <- gsub("\n", "", 날짜_dg)



## data.frame

danggn_에어프라이기_본문 <- data.frame(제목_dg, 가격_dg, 본문_dg, 조회수_dg,                                           날짜_dg, 주소_dg)  
