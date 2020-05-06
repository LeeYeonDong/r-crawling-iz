
###뽐뿌 크롤링
##url수집


n <- 120

searchword <- "갤럭시 폴드"

for(i in 1:n){
  tryCatch({
    res_bb <- GET(url = "http://www.ppomppu.co.kr/search_bbs.php",
                  query = list(search_type = "sub_memo",
                               page_no = i,
                               keyword = searchword,
                               page_size = "20",
                               bbs_id = "",
                               order_type = "date",
                               bbs_cate = "1"))
    
    
    cat('현재', i, '페이지 수집 중! 상태코드는', status_code(x = res_bb), '입니다.\n') 
    
    
    
    
    링크_bb <- c()
    
    제목_bb <- c()
    포럼_bb <- c()
    조회수_bb <- c()
    날짜_bb <- c()
    공감_bb <- c()
    비공감_bb <- c()
    댓글수_bb <- c()
    
    
    
    ##링크
    
    링크.tmp <- res_bb %>% 
      read_html() %>% 
      html_nodes("span.title") %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      unique()
    
    if (length(링크.tmp) == 0) {
      링크_bb <- append(링크_bb, "수동확인")
    } else {
      링크_bb <- append(링크_bb, 링크.tmp)
    }
    
    
    
    ##제목
    
    제목.tmp <- res_bb %>% 
      read_html() %>%  
      html_nodes("span.title") %>% 
      html_nodes("a") %>% 
      html_text() 
    
    if (length(제목.tmp) == 0) {
      제목_bb <- append(제목_bb, "수동확인")
    } else {
      제목_bb <- append(제목_bb, 제목.tmp)
    }
    
    
    ##포럼
    
    포럼.tmp <- res_bb %>% 
      read_html() %>% 
      html_nodes("p.desc") %>%
      html_nodes("span:nth-child(1)") %>% 
      html_text() 
    
    if (length(포럼.tmp) == 0) {
      포럼_bb <- append(포럼_bb, "수동확인")
    } else {
      포럼_bb <- append(포럼_bb, 포럼.tmp)
    }
    
    
    
    ##조회수
    
    조회수.tmp <- res_bb %>% 
      read_html() %>% 
      html_nodes("p.desc") %>%
      html_nodes("span:nth-child(2)") %>% 
      html_text() 
    
    if (length(조회수.tmp) == 0) {
      조회수_bb <- append(조회수_bb, "수동확인")
    } else {
      조회수_bb <- append(조회수_bb, 조회수.tmp)
    }
    
    
    ##날짜
    
    날짜.tmp <- res_bb %>% 
      read_html() %>% 
      html_nodes("p.desc") %>%
      html_nodes("span:nth-child(3)") %>% 
      html_text() 
    
    if (length(날짜.tmp) == 0) {
      날짜_bb <- append(날짜_bb, "수동확인")
    } else {
      날짜_bb <- append(날짜_bb, 날짜.tmp)
    }
    
    
    
    ##공감
    
    공감.tmp <- res_bb %>% 
      read_html() %>% 
      html_nodes("p.desc") %>%
      html_nodes("span:nth-child(4)") %>% 
      html_text() 
    
    if (length(공감.tmp) == 0) {
      공감_bb <- append(공감_bb, "수동확인")
    } else {
      공감_bb <- append(공감_bb, 공감.tmp)
    }
    
    
    ##비공감
    
    비공감.tmp <- res_bb %>% 
      read_html() %>% 
      html_nodes("p.desc") %>%
      html_nodes("span:nth-child(5)") %>% 
      html_text() 
    
    if (length(비공감.tmp) == 0) {
      비공감_bb <- append(비공감_bb, "수동확인")
    } else {
      비공감_bb <- append(비공감_bb, 비공감.tmp)
    }
    
    
    
    ##댓글수
    
    댓글수.tmp <- res_bb %>% 
      read_html() %>% 
      html_nodes("small.comment-count") %>%
      html_text() 
    
    if (length(댓글수.tmp) == 0) {
      댓글수_bb <- append(댓글수_bb, "수동확인")
    } else {
      댓글수_bb <- append(댓글수_bb, 댓글수.tmp)
    }
    
    
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
  
}


length(링크_bb)



##cmd
#cd C:\r_selenium
#java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445


## RSelnium

remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()


## 아이디 비밀번호 #자동로그인안됨


remDr$navigate(링크_bb[1])


## 로그인 - 네이버계정
element <- remDr$findElement("css", "span.smloginbtnbox >
                                     a.loginsmbtn")
element$clickElement()


naver.lg <- remDr$findElement("css", "div.sns_login >
                                      a:nth-child(3)")
naver.lg$clickElement()


id <- remDr$findElement("css", value="input#id") 
id$sendKeysToElement(list("1125mirhan")) 

Sys.sleep(time = 3) 

pw <- remDr$findElement("css", value="input#pw") 
pw$sendKeysToElement(list("zl5slglf")) 


login <- remDr$findElement("css", value="input.btn_global") 
login$clickElement() 




### 제목 작성자 본문 댓글 댓글수 주소


작성자_bb <- c()
본문_bb <- c()
댓글_bb <- c()
주소_bb <- c()



###for 문

for (i in 1:length(링크_bb)){
  tryCatch({
    remDr$navigate(링크_bb[i])
    body <- remDr$getPageSource()[[1]]
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    body <- body %>% read_html()
    
    
    ##작성자
    
    작성자.tmp <- body %>% 
      html_nodes("table.info_bg") %>% 
      html_nodes("td:nth-child(1)") %>% 
      html_nodes("font.view_name") %>% 
      html_text() 
    
    if (length(작성자.tmp) == 0) {
      작성자_bb <- append(작성자_bb, "수동확인")
    } else {
      작성자_bb <- append(작성자_bb, 작성자.tmp)
    }
    
    
    
    
    ##본문
    
    본문.tmp <- body %>% 
      html_nodes("td.board-contents") %>% 
      html_text() 
    
    if (length(본문.tmp) == 0) {
      본문_bb <- append(본문_bb, "수동확인")
    } else {
      본문_bb <- append(본문_bb, 본문.tmp)
    }
    
    
    
    ##댓글
    
    댓글.tmp <- body %>% 
      html_nodes("div.comment_line") %>% 
      html_nodes("div.han") %>% 
      html_text() %>%
      unique()
    
    if (length(댓글_bb) == 0) {
      댓글_bb <- append(댓글_bb, "수동확인")
    } else {
      댓글_bb <- append(댓글_bb, 댓글.tmp)
    }
    
    
    ##주소
    
    주소_bb <- append(주소_bb, 링크_bb[i])
    
    
    Sys.sleep(time = 1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}

## triming

본문_bb <- gsub("\n", "", 본문_bb)
본문_bb <- gsub("\t", "", 본문_bb)

조회수_bb <- gsub("\\D", "", 조회수_bb)

댓글_bb <- gsub("\n", "", 댓글_bb)
댓글_bb <- gsub("\t", "", 댓글_bb)


포럼_bb <- gsub("\\W","", 포럼_bb)



## data.frame

length(링크_bb)

length(포럼_bb)
length(제목_bb)
length(작성자_bb)
length(조회수_bb)
length(본문_bb)

length(공감_bb)
length(비공감_bb)
length(작성자_bb)
length(댓글수_bb)
length(주소_bb)
length(날짜_bb)

length(댓글_bb)


bbombbu_갤럭시폴드_본문 <- data.frame(포럼_bb, 제목_bb, 작성자_bb, 조회수_bb, 본문_bb, 공감_bb, 비공감_bb, 댓글수_bb, 주소_bb, 날짜_bb)  
bbombbu_갤럭시폴드_댓글 <- data.frame(댓글_bb) 

write.csv(bbombbu_갤럭시폴드_본문, file = "D:/bbombbu_갤럭시폴드_본문.csv", row.names=FALSE)
write.csv(bbombbu_갤럭시폴드_댓글, file = "D:/bbombbu_갤럭시폴드_댓글.csv", row.names=FALSE)








#### 개별 페이지 링크 분해  

링크_bbsp <- 링크_bb  

링크_bbsp <- strsplit(링크_bbsp, split="\\?")  


링크_bbsp_url <- c()  

for(i in 1:length(링크_bbsp)){  
  링크_bbsp.tmp <- 링크_bbsp[[i]][2]  
  링크_bbsp_url <- append(링크_bbsp_url, 링크_bbsp.tmp)   
}  


링크_bbsp <- str_replace_all(링크_bbsp_url,"id=","")  
링크_bbsp <- str_replace_all(링크_bbsp,"no=","")  

링크_bbsp <- strsplit(링크_bbsp , split="\\&")  



## GET함수에 필요한 ID, NO추출  

링크_bb_ID <- c()  
링크_bb_NO <- c()  

for(i in 1:length(링크_bbsp)){  
  
  링크_bbsp1.tmp <- 링크_bbsp[[i]][1]  
  링크_bbsp2.tmp <- 링크_bbsp[[i]][2]  
  
  링크_bb_ID <- append(링크_bb_ID, 링크_bbsp1.tmp)   
  링크_bb_NO <- append(링크_bb_NO, 링크_bbsp2.tmp)   
  
}  




#### for문 : 개별 페이지  

작성자_bb <- c()  
본문_bb <- c()  
댓글_bb <- c()  

링크_bb[19] 

for(i in 1:length(링크_bb)){   
  tryCatch({   
    res_bb2 <- GET(url = "http://www.ppomppu.co.kr/zboard/view.php",  
                   query = list(id = 링크_bb_ID[i],  
                                no = 링크_bb_NO[i],  
                                keyword = searchword))  
    
    cat('현재', i, '페이지 수집 중! 상태코드는', status_code(x = res_bb2), '입니다.\n')   
    
    ##작성자  
    
    작성자.tmp <- res_bb2 %>%   
      read_html() %>%    
      html_nodes("table.info_bg") %>%   
      html_nodes("td:nth-child(1)") %>%   
      html_nodes("font.view_name") %>%   
      html_text()   
    
    if (length(작성자.tmp) == 0) {  
      작성자_bb <- append(작성자_bb, "수동확인")  
    } else {  
      작성자_bb <- append(작성자_bb, 작성자.tmp)  
    }  
    
    
    
    ##본문  
    
    본문.tmp <- res_bb2 %>%   
      read_html() %>%    
      html_nodes("td.board-contents") %>%   
      html_text()   
    
    if (length(본문.tmp) == 0) {  
      본문_bb <- append(본문_bb, "수동확인")  
    } else {  
      본문_bb <- append(본문_bb, 본문.tmp)  
    }  
    
    
    
    ##댓글  
    
    댓글.tmp <- res_bb2 %>%   
      read_html() %>%   
      html_nodes("div.comment_line") %>%   
      html_nodes("div.han") %>%   
      html_text() %>%  
      unique()  
    
    if (length(댓글_bb) == 0) {  
      댓글_bb <- append(댓글_bb, "수동확인")  
    } else {  
      댓글_bb <- append(댓글_bb, 댓글.tmp)  
    }  
    
    
    Sys.sleep(time = 1)     
    
    
  }, error = function(e) cat("불러올 수 없습니다!\n")) 
  
  
}  



## 데이터 전처리  

본문_bb <- str_replace_all(본문_bb,"\n", "")  
본문_bb <- str_replace_all(본문_bb,"\t", "")  

조회수_bb <- str_replace_all(조회수_bb,"\\D", "")  

댓글_bb <- str_replace_all(댓글_bb,"\n", "")  
댓글_bb <- str_replace_all(댓글_bb,"\t", "")  

포럼_bb <- str_replace_all(포럼_bb,"\\W","") 




#### 수집 데이터 data frame으로 저장  

bbombbu_갤럭시폴드_본문 <- data.frame(포럼_bb, 제목_bb, 작성자_bb, 조회수_bb, 본문_bb, 공감_bb, 비공감_bb, 댓글수_bb, 주소_bb, 날짜_bb)    
bbombbu_갤럭시폴드_댓글 <- data.frame(댓글_bb) 