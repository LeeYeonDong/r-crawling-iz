##네이버 부동산 카페 - selenium


##명령 프롬프트 실행

#cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445


### RSelenium 가동

remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()


target <- "https://m.cafe.naver.com/jaegebal"

remDr$navigate(target)



## Selenium 구동창에서 네이버 로그인-카페회원가입하기

remDr$navigate(target)

#더보기

n <- 10 ############설정하기 = 전체게시글수/20 -> 전체글 긁어오기가능###########

for(i in 1:n){
  tryCatch({
    
    
    body <- remDr$getPageSource()[[1]]
    body <- body %>% read_html()
    
    more_reply <- remDr$findElement("css", "span.u_cbox_page_more")
    more_reply$clickElement()
    
    if(i==n) break()
    
    Sys.sleep(time = 0.01)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}

Sys.sleep(time = 1)


## 현재페이지 정보 읽어오기

frontpage <- remDr$getPageSource()[[1]]
body <- frontpage %>% read_html() 


##링크

링크_ncafe <- body %>% 
  html_nodes("ul.list_area") %>%
  html_nodes("li.board_box") %>%
  html_nodes("a.txt_area") %>%
  html_attr("href")

Sys.sleep(time = 1)


##제목

제목_ncafe <- body %>% 
  html_nodes("a.txt_area") %>%
  html_nodes("strong.tit") %>%
  html_text()  

Sys.sleep(time = 1)


##날짜

날짜_ncafe <- body %>% 
  html_nodes("span.time") %>%
  html_text()  

Sys.sleep(time = 1)


##조회수

조회수_ncafe <- body %>% 
  html_nodes("span.no") %>%
  html_text()  

조회수_ncafe <- str_replace_all(조회수_ncafe,"조회 ", "")
조회수_ncafe <- str_replace_all(조회수_ncafe,",", "")

Sys.sleep(time = 1)


##작성자

작성자_ncafe <- body %>% 
  html_nodes("span.nick") %>%
  html_nodes("span.ellip") %>%
  html_text()  

Sys.sleep(time = 1)


##댓글수

댓글수_ncafe <- body %>% 
  html_nodes("em.num") %>%
  html_text()  

Sys.sleep(time = 1)


## 본문

본문_ncafe <- c()

for (i in 1:length(링크_ncafe)){
  
  tryCatch({
    remDr$navigate(링크_ncafe[i])
    body <- remDr$getPageSource()[[1]]
    body <- body %>% read_html()
    
    Sys.sleep(time = 0.1)
    
    본문.tmp <- body %>% 
      html_nodes("div.post_cont") %>% 
      html_nodes("p") %>% 
      html_text()
    
    본문.tmp <- 본문.tmp %>% paste(collapse = " ")
    본문.tmp <- str_replace_all(본문.tmp,"투표는 표시되지 않습니다.\n", "")
    
    if (length(본문.tmp) != 0) {
      본문_ncafe <- append(본문_ncafe, 본문.tmp)
    } else {
      본문_ncafe <- append(본문_ncafe, "수동확인")
    }
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
  
}


##

링크_ncafe %>% length()
제목_ncafe %>% length()
날짜_ncafe %>% length()

조회수_ncafe %>% length()
작성자_ncafe %>% length()
댓글수_ncafe %>% length()

본문_ncafe %>% length()

df_ncafe <- data.frame(링크_ncafe,제목_ncafe,날짜_ncafe,조회수_ncafe,
                          작성자_ncafe,댓글수_ncafe,본문_ncafe)

names(df_ncafe) <- c("링크","제목","날짜","조회수","작성자","댓글수","본문")

df_ncafe %>% View()


























##링크분해

링크_ncafe_sp <- strsplit(링크_ncafe, split="m.")

링크_ncafe_sp2 <- c()
링크_ncafe_sp3 <- c()

for (i in 1:length(링크_ncafe)){
  링크_ncafe_sp2.tmp <- 링크_ncafe_sp[[i]][2]
  링크_ncafe_sp3.tmp <- 링크_ncafe_sp[[i]][3]
  
  링크_ncafe_sp2 <- append(링크_ncafe_sp2,링크_ncafe_sp2.tmp)
  링크_ncafe_sp3 <- append(링크_ncafe_sp3,링크_ncafe_sp3.tmp)
}

링크_ncafe_sp <- paste0("https://",링크_ncafe_sp2,"m/",링크_ncafe_sp3)



###

날짜_ncafe <- c()
제목_ncafe <- c()
본문_ncafe <- c()
주소_ncafe <- c()


for (i in 1:length(링크_ncafe_sp)){
  
  tryCatch({
    remDr$navigate(링크_ncafe_sp[1])
    body <- remDr$getPageSource()[[1]]
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    body <- body %>% read_html()
    
    
    ##제목
    
    제목.tmp <- body %>% 
      html_nodes("div.tit-box") %>% 
      html_nodes("td:nth-child(1)") %>%
      html_text()  
    
    if (length(제목.tmp) != 0) {
      제목_ncafe <- append(제목_ncafe, 제목.tmp)
    } else {
      제목_ncafe <- append(제목_ncafe, "수동확인")
    }
    
    