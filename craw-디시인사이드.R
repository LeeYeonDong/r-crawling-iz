##아라가키갤 크롤링

#####패키지서치


##### 각키갤 url https://gall.dcinside.com/board/lists

링크_각키 <- c() ## "링크_각키" 라는 빈 벡터를 만듭니다

n <- 201

for(i in 1:n){
  tryCatch({
    
    res_yui <- GET(url = 'https://gall.dcinside.com/mgallery/board/lists/',
                   query = list(id = 'aragakiyui',
                                page=i))
    
    
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_yui), '입니다.\n')
    
    
    
    #### 각키갤 링크 수집
    
    링크.각키.tmp <- res_yui %>%
      read_html() %>% 
      html_nodes('td.gall_tit.ub-word') %>% ##링크 주소 바로 위에 있는 Element입니다
      html_nodes('a:nth-child(1)') %>% ## 게시글 링크 주소는 td.gall_tit.ub-word 밑  첫번째 "a"에 있습니다 
      html_attr('href') %>% ## href="링크주소"를 긁는 명령어 입니다
      unique() ##중복되는 링크를 제거해주는 함수입니다
    
    
    if (length(링크_각키.tmp) == 0) {
      링크_각키 <- append(링크.각키, "수동확인")
    } else {
      링크_각키 <- append(링크_각키, 링크.각키.tmp)
    }  ## 수집한 링크가 없을경우 "수동확인", 제대로 수집한 경우는 "링크_각키"로 저장합니다 
    
    
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}


링크_각키 <- paste0("https://gall.dcinside.com/",링크_각키)





## RSelenium

remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()




제목_각키 <- c()
작성자_각키 <- c()
날짜_각키 <- c()
본문_각키 <-c()
주소_각키 <-c()

for (i in 1:length(링크_각키)){
  tryCatch({
    
    remDr$navigate(링크_각키[1])
    body <- remDr$getPageSource()[[1]]
    body <- body %>% read_html()
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    
    ## 제목
    
    제목.각키.tmp <- body %>% 
      html_nodes("span.title_subject") %>% 
      html_text()
    
    
    if (length(제목.각키.tmp) == 0) {
      제목_각키 <- append(제목_각키, "수동확인")
    } else {
      제목_각키 <- append(제목_각키, 제목.각키.tmp)
    }  ## 수집한 제목 없을 경우 "수동확인", 제대로 수집한 경우는 "제목_각키"벡터 공간에 저장합니다
    
    
    
    ## 작성자
    
    작성자.각키.tmp <- body %>% 
      html_nodes("div.fl") %>% 
      html_nodes("span.nickname") %>% 
      html_nodes("em") %>%
      html_text()
    
    
    if (length(제목.각키.tmp) == 0) {
      작성자_각키 <- append(작성자_각키, "수동확인")
    } else {
      작성자_각키 <- append(작성자_각키, 작성자.각키.tmp)
    }  ## 수집한 작성자 없을 경우 "수동확인", 제대로 수집한 경우는 "작성자_각키"벡터 공간에 저장합니다
    
    
    
    
    ## 날짜
    
    날짜.각키.tmp <- body %>% 
      html_nodes("span.gall_date") %>% 
      html_text()
    
    
    if (length(날짜.각키.tmp) == 0) {
      날짜_각키 <- append(날짜_각키, "수동확인")
    } else {
      날짜_각키 <- append(날짜_각키, 날짜.각키.tmp)
    }  ## 수집한 날짜 없을 경우 "수동확인", 제대로 수집한 경우는 "날짜_각키"벡터 공간에 저장합니다
    
    
    
    ## 본문
    
    본문.각키.tmp <- body %>% 
      html_nodes("div.writing_view_box") %>% 
      html_text()
    
    
    if (length(본문.각키.tmp) == 0) {
      본문_각키 <- append(본문_각키, "수동확인")
    } else {
      본문_각키 <- append(본문_각키, 본문.각키.tmp)
    }  ## 수집한 본문 없을 경우 "수동확인", 제대로 수집한 경우는 "본문_각키"벡터 공간에 저장합니다
    
    
    
    ## 주소(URL)
    
    주소_각키 <- append(주소_각키, 링크_각키[i])
    
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}





## 데이터 전처리

본문_각키 <- str_replace_all(본문_각키,"\n","")
본문_각키 <- str_replace_all(본문_각키,"\t","")



## 데이터프레임으로 합치기 및 csv파일로 저장

df_각키 <- data.frame(제목_각키, 작성자_각키, 날짜_각키, 본문_각키, 주소_각키)
write.csv(df_각키, file = "D:/df_각키.csv", row.names=FALSE)











#### 각키갤 GET/POST방식

링크_각키 <- c() 

n <- 201

for(i in 1:n){
  tryCatch({
    
    res_yui <- GET(url = 'https://gall.dcinside.com/mgallery/board/lists/',
                   query = list(id = 'aragakiyui',
                                page=i))
    
    
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_yui), '입니다.\n')
    
    
    
    #### 각키갤 링크 수집
    
    링크.각키.tmp <- res_yui %>%
      read_html() %>% 
      html_nodes('td.gall_tit.ub-word') %>% 
      html_nodes('a:nth-child(1)') %>%  
      html_attr('href') %>% 
      unique() 
    
    
    if (length(링크_각키.tmp) == 0) {
      링크_각키 <- append(링크.각키, "수동확인")
    } else {
      링크_각키 <- append(링크_각키, 링크.각키.tmp)
    }  ## 수집한 링크가 없을경우 "수동확인", 제대로 수집한 경우는 "링크_각키"로 저장합니다 
    
    
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}




## 링크 분할

링크_각키_no <- c()
링크_각키_page <- c()



링크_각키sp <- strsplit(링크_각키, split="&")


for(i in 1:length(링크_각키)){
  링크_각키_no.tmp <- 링크_각키sp[[i]][2]
  링크_각키_page.tmp <- 링크_각키sp[[i]][3]
  
  링크_각키_no <- append(링크_각키_no, 링크_각키_no.tmp)
  링크_각키_page <- append(링크_각키_page, 링크_각키_page.tmp)
  
}


링크_각키_no <- str_replace_all(링크_각키_no,"no=","")
링크_각키_page <- str_replace_all(링크_각키_page,"page=","")




## 페이지별 element 수집

제목_각키 <- c()
작성자_각키 <- c()
날짜_각키 <- c()
본문_각키 <-c()
주소_각키 <-c() 



for(i in 1:length(링크_각키)){
  tryCatch({
    
    res_yui <- GET(url = 'https://gall.dcinside.com/mgallery/board/view/',
                   query = list(id = 'aragakiyui',
                                no = 링크_각키_no[i],
                                page = 링크_각키_page[i]))
    
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_yui), '입니다.\n')
    
    
    
    
    ## 제목
    
    제목.각키.tmp <- res_yui %>%
      read_html() %>%  
      html_nodes("span.title_subject") %>% 
      html_text()
    
    
    if (length(제목.각키.tmp) == 0) {
      제목_각키 <- append(제목_각키, "수동확인")
    } else {
      제목_각키 <- append(제목_각키, 제목.각키.tmp)
    } 
    
    
    ## 작성자
    
    작성자.각키.tmp <- res_yui %>%
      read_html() %>% 
      html_nodes("div.fl") %>% 
      html_nodes("span.nickname") %>% 
      html_nodes("em") %>%
      html_text()
    
    
    if (length(제목.각키.tmp) == 0) {
      작성자_각키 <- append(작성자_각키, "수동확인")
    } else {
      작성자_각키 <- append(작성자_각키, 작성자.각키.tmp)
    }  
    
    
    
    ## 날짜
    
    날짜.각키.tmp <- res_yui %>%
      read_html() %>% 
      html_nodes("span.gall_date") %>% 
      html_text()
    
    
    if (length(날짜.각키.tmp) == 0) {
      날짜_각키 <- append(날짜_각키, "수동확인")
    } else {
      날짜_각키 <- append(날짜_각키, 날짜.각키.tmp)
    }  
    
    
    ## 본문
    
    본문.각키.tmp <- res_yui %>%
      read_html() %>% 
      html_nodes("div.writing_view_box") %>% 
      html_text()
    
    
    if (length(본문.각키.tmp) == 0) {
      본문_각키 <- append(본문_각키, "수동확인")
    } else {
      본문_각키 <- append(본문_각키, 본문.각키.tmp)
    } 
    
    
    
    ## 주소(URL)
    
    주소_각키 <- append(주소_각키, 링크_각키[i])
    
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}



## 데이터 전처리

본문_각키 <- str_replace_all(본문_각키,"\n","")
본문_각키 <- str_replace_all(본문_각키,"\t","")



length(제목_각키)
length(작성자_각키)
length(날짜_각키)
length(본문_각키)
length(주소_각키)

## 데이터프레임으로 합치기 및 csv파일로 저장
View(제목_각키)
df_각키 <- data.frame(작성자_각키, 날짜_각키, 본문_각키, 주소_각키)
df_각키_제목 <- data.frame(제목_각키)
write.csv(df_각키, file = "D:/df_각키.csv", row.names=FALSE)
write.csv(df_각키_제목, file = "D:/df_각키_제목.csv", row.names=FALSE)
