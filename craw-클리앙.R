
#####crawling 클리앙 - "갤럭시폴드" 19.12

#####url수집
n <- 50
searchword <- "갤럭시 폴드"

링크_cl <- c()
날짜_cl <- c()

for(i in 0:n){
  tryCatch({
    res_cl <- GET(url = "https://www.clien.net/service/search",
                  query = list(q = searchword,
                               sort = "recency",
                               p = i,
                               boardCd = "",
                               isBoard = "false"))
    
    cat('현재', i, '페이지 수집 중! 상태코드는', status_code(x = res_cl), '입니다.\n')
    
    ##링크
    
    링크.tmp <- res_cl %>% 
      read_html() %>% 
      html_nodes("a.subject_fixed") %>%
      html_attr("href") %>%
      unique()
    
    링크.tmp <- paste0("https://www.clien.net",링크.tmp)
    
    링크_cl <- append(링크.tmp,링크_cl) 
    
    
    
    ##날짜
    
    날짜.tmp <- res_cl %>% 
      read_html() %>% 
      html_nodes("span.timestamp") %>%
      html_text() 
    
    if (length(날짜.tmp) == 0) {
      날짜_cl <- append(날짜_cl, "수동확인")
    } else {
      날짜_cl <- append(날짜_cl, 날짜.tmp)
    }
    
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}







##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

## RSelnium

remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()



### 제목 본문 댓글 조회수 댓글수 주소

제목_cl <- c()
본문_cl <- c()
댓글_cl <- c()
조회수_cl <- c()
댓글수_cl <- c()
주소_cl <- c()


for (i in 403:length(링크_cl)){
  tryCatch({
    remDr$navigate(링크_cl[i])
    body <- remDr$getPageSource()[[1]]
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    body <- body %>% read_html()
    
    
    ##제목
    
    제목.tmp <- body %>% 
      html_nodes("h3.post_subject") %>% 
      html_text()
    
    if (length(제목_cl) == 0) {
      제목_cl <- append(제목_cl, "수동확인")
    } else {
      제목_cl <- append(제목_cl, 제목.tmp)
    }
    
    
    ##본문
    
    본문.tmp <- body %>% 
      html_nodes("div.post_article") %>% 
      html_text()
    
    if (length(본문_cl) == 0) {
      본문_cl <- append(본문_cl, "수동확인")
    } else {
      본문_cl <- append(본문_cl, 본문.tmp)
    }
    
    
    
    ##댓글
    
    댓글.tmp <- body %>% 
      html_nodes("div.comment_view") %>% 
      html_text()
    
    if (length(댓글_cl) == 0) {
      댓글_cl <- append(댓글_cl, "수동확인")
    } else {
      댓글_cl <- append(댓글_cl, 댓글.tmp)
    }
    
    
    ##조회수
    
    조회수.tmp <- body %>% 
      html_nodes("span.view_count") %>% 
      html_text()
    
    if (length(조회수_cl) == 0) {
      조회수_cl <- append(조회수_cl, "수동확인")
    } else {
      조회수_cl <- append(조회수_cl, 조회수.tmp)
    }
    
    
    ##댓글수
    
    댓글수.tmp <- body %>% 
      html_nodes('div.comment_head') %>%
      html_text()
    
    if (length(댓글수_cl) == 0) {
      댓글수_cl <- append(댓글수_cl, "수동확인")
    } else {
      댓글수_cl <- append(댓글수_cl, 댓글수.tmp)
    }
    
    
    
    ##주소
    
    주소_cl <- append(주소_cl, 링크_cl[i])
    
    
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}



## triming

제목_cl <- gsub("\n", "", 제목_cl)
제목_cl <- gsub("\t", "", 제목_cl)

본문_cl <- gsub("\n", "", 본문_cl)
본문_cl <- gsub("\t", "", 본문_cl)

조회수_cl <- gsub("\\D", "", 조회수_cl)

댓글_cl <- gsub("\n", "", 댓글_cl)
댓글_cl <- gsub("\t", "", 댓글_cl)

댓글수_cl <- gsub("\\D", "", 댓글수_cl)


## data.frame

clien_갤럭시폴드_본문 <- data.frame(날짜_cl, 제목_cl, 본문_cl, 조회수_cl, 댓글수_cl, 주소_cl)  
clien_갤럭시폴드_댓글 <- data.frame(댓글_cl) 

write.csv(clien_갤럭시폴드_본문, file = "D:/성재/크롤링/clien_갤럭시폴드_본문.csv", row.names=FALSE)
write.csv(clien_갤럭시폴드_댓글, file = "D:/성재/크롤링/clien_갤럭시폴드_댓글.csv", row.names=FALSE)




#####crawling 클리앙 - "갤럭시폴드" 19.12

##### GET/POST 방식

n <- 50
searchword <- "갤럭시 폴드"

링크_cl <- c()
날짜_cl <- c()

for(i in 0:n){
  tryCatch({
    res_cl <- GET(url = "https://www.clien.net/service/search",
                  query = list(q = searchword,
                               sort = "recency",
                               p = i,
                               boardCd = "",
                               isBoard = "false"))
    
    cat('현재', i, '페이지 수집 중! 상태코드는', status_code(x = res_cl), '입니다.\n')
    
    ##링크
    
    링크.tmp <- res_cl %>% 
      read_html() %>% 
      html_nodes("a.subject_fixed") %>%
      html_attr("href") %>%
      unique()
    
    링크.tmp <- paste0("https://www.clien.net",링크.tmp)
    
    링크_cl <- append(링크.tmp,링크_cl) 
    
    
    
    ##날짜
    
    날짜.tmp <- res_cl %>% 
      read_html() %>% 
      html_nodes("span.timestamp") %>%
      html_text() 
    
    if (length(날짜.tmp) == 0) {
      날짜_cl <- append(날짜_cl, "수동확인")
    } else {
      날짜_cl <- append(날짜_cl, 날짜.tmp)
    }
    
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}


### 링크 분할


링크_clsp <- 링크_cl

링크_clsp[1]

링크_cl_url <- c()
링크_cl_p <- c()



링크_clsp <- str_replace_all(링크_clsp,"&sort=recency&boardCd=&isBoard=false","")

링크_clsp <- strsplit(링크_clsp, split="\\?")


for(i in 1:length(링크_clsp)){
  
  링크_cl_url.tmp <- 링크_clsp[[i]][1]
  
  링크_cl_url <- append(링크_cl_url, 링크_cl_url.tmp)
  
}



## element 수집


제목_cl <- c()
본문_cl <- c()
댓글_cl <- c()
조회수_cl <- c()
댓글수_cl <- c()
주소_cl <- c()


searchword <- "갤럭시 폴드"

for(i in 1:length(링크_cl_url)){
  tryCatch({
    res_cl <- GET(url = 링크_cl_url[i],
                  query = list(q = searchword,
                               sort = "recency",
                               p = 0,
                               boardCd = "",
                               isBoard = "false"))
    
    cat('현재', i, '페이지 수집 중! 상태코드는', status_code(x = res_cl), '입니다.\n')
    
    
    ##제목
    
    제목.tmp <- res_cl %>% 
      read_html() %>% 
      html_nodes("h3.post_subject") %>% 
      html_text()
    
    if (length(제목_cl) == 0) {
      제목_cl <- append(제목_cl, "수동확인")
    } else {
      제목_cl <- append(제목_cl, 제목.tmp)
    }
    
    
    
    ##본문
    
    본문.tmp <- res_cl %>% 
      read_html() %>% 
      html_nodes("div.post_article") %>% 
      html_text()
    
    if (length(본문_cl) == 0) {
      본문_cl <- append(본문_cl, "수동확인")
    } else {
      본문_cl <- append(본문_cl, 본문.tmp)
    }
    
    
    
    ##댓글
    
    댓글.tmp <- res_cl %>% 
      read_html() %>% 
      html_nodes("div.comment_view") %>% 
      html_text()
    
    if (length(댓글_cl) == 0) {
      댓글_cl <- append(댓글_cl, "수동확인")
    } else {
      댓글_cl <- append(댓글_cl, 댓글.tmp)
    }
    
    
    
    ##조회수
    
    조회수.tmp <- res_cl %>% 
      read_html() %>% 
      html_nodes("span.view_count") %>% 
      html_text()
    
    if (length(조회수_cl) == 0) {
      조회수_cl <- append(조회수_cl, "수동확인")
    } else {
      조회수_cl <- append(조회수_cl, 조회수.tmp)
    }
    
    
    
    ##댓글수
    
    댓글수.tmp <- res_cl %>% 
      read_html() %>%  
      html_nodes('div.comment_head') %>%
      html_text()
    
    if (length(댓글수_cl) == 0) {
      댓글수_cl <- append(댓글수_cl, "수동확인")
    } else {
      댓글수_cl <- append(댓글수_cl, 댓글수.tmp)
    }
    
    
    
    ##주소
    
    주소_cl <- append(주소_cl, 링크_cl[i])
    
    
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}




## triming

제목_cl <- gsub("\n", "", 제목_cl)
제목_cl <- gsub("\t", "", 제목_cl)

본문_cl <- gsub("\n", "", 본문_cl)
본문_cl <- gsub("\t", "", 본문_cl)

조회수_cl <- gsub("\\D", "", 조회수_cl)

댓글_cl <- gsub("\n", "", 댓글_cl)
댓글_cl <- gsub("\t", "", 댓글_cl)

댓글수_cl <- gsub("\\D", "", 댓글수_cl)


## data.frame

clien_갤럭시폴드_본문 <- data.frame(날짜_cl, 제목_cl, 본문_cl, 조회수_cl, 댓글수_cl, 주소_cl)  
clien_갤럭시폴드_댓글 <- data.frame(댓글_cl) 

write.csv(clien_갤럭시폴드_본문, file = "D:/clien_갤럭시폴드_본문.csv", row.names=FALSE)
write.csv(clien_갤럭시폴드_댓글, file = "D:/clien_갤럭시폴드_댓글.csv", row.names=FALSE)