
##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445


### RSelenium

remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()


#####네이버 뉴스(최신순)

main <- "https://search.naver.com/search.naver?&where=news&query="
검색어 <- "다이노스"

mid1 <- "&sm=tab_srt&sort=1&photo=0&field=0&reporter_article=&pd=3&ds="
시작일1 <- "2020.01.01"
부터1 <- "&de="
종료일1 <- "2020.03.31"

mid2 <- "&docid=&nso=so%3Ar%2Cp%3Afrom"
시작일2 <- "20200101"
부터2 <- "to"
종료일2 <- "20200331"

mid4 <- "%2Ca%3Aall&mynews=0&refresh_start=0&related=0"




## 최초의 검색 주소(반복문 전까지) 이어붙이기

target <- paste(main, 검색어, sep = "")

target <- paste(target, mid1, sep = "")
target <- paste(target, 시작일1, sep = "")
target <- paste(target, 부터1, sep = "")
target <- paste(target, 종료일1, sep = "")

target <- paste(target, mid2, sep = "")
target <- paste(target, 시작일2, sep = "")
target <- paste(target, 부터2, sep = "")
target <- paste(target, 종료일2, sep = "")

target <- paste(target,mid4, sep = "")

remDr$navigate(target)




## 언론사 선택창 
element <- remDr$findElement("css", "#news_popup > 
                                    a:nth-child(1)")
element$clickElement()
Sys.sleep(time = 0.5)

## 언론사 선택
element <- remDr$findElement("css", "#ca_p1")
element$clickElement()
Sys.sleep(time = 0.5)

## 언론사 선택 확인
element <- remDr$findElement("css", "#_nx_option_media >
                                     div.con_bx >
                                     div.view_btn > 
                                     button.impact._submit_btn")
element$clickElement()
Sys.sleep(time = 0.5)



########## 최신순

element <- remDr$findElement("css", "div.news_option > 
                                     ul.sort > 
                                     li:nth-child(2)")
element$clickElement()
Sys.sleep(time = 0.5)



## 링크

링크_nnews <- c()

for(i in 1:10){
  tryCatch({
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    frontpage <- remDr$getPageSource()[[1]]
    body <- frontpage %>% read_html() 
    
    
    ##링크
    
    링크.tmp1 <- body %>% 
      html_nodes("dd.txt_inline") %>%
      html_nodes("a._sp_each_url") %>%
      html_attr("href")
    
    링크.tmp2 <- body %>% 
      html_nodes("span.txt_sinfo") %>%
      html_nodes("a._sp_each_url") %>%
      html_attr("href")
    
    링크.tmp <- append(링크.tmp1,링크.tmp2)
    링크.tmp <- 링크.tmp %>% unique()
    
    링크_nnews <- append(링크_nnews,링크.tmp)
    
    
    
    ##언론사
    
    언론사.tmp1 <- body %>% 
      html_nodes("span._sp_each_source") %>%
      html_text()
    
    언론사.tmp2 <- body %>% 
      html_nodes("span.press") %>%
      html_text()
    
    언론사.tmp <- append(언론사.tmp1,언론사.tmp2)
    
    언론사_nnews <- append(언론사_nnews,언론사.tmp)
    
    if(i==10) break()
    
    element <- remDr$findElement("css", "#main_pack > 
                             div.news.mynews.section._prs_nws>
                             div.paging >
                             a.next")
    element$clickElement()
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}

링크_nnews <- 링크_nnews %>% unique()
링크_nnews %>% View()


######## 1.sports.news-case

sports.news <- grep("http://sports.news",링크_nnews)
링크_sports.news <- 링크_nnews[sports.news]

링크_sports.news %>% head()

언론사_sports.news <- c()

날짜_sports.news <- c()
제목_sports.news <- c()
본문_sports.news <- c()
주소_sports.news <- c()

좋아_sports.news <- c()
슬퍼_sports.news <- c()
화나_sports.news <- c()
팬이_sports.news <- c()
후속_sports.news <- c()

댓글_sports.news <- c()
댓글수_sports.news <- c()

주소_sports.news.re <- c() 


####

for (i in 1:length(링크_sports.news)){
  
  tryCatch({
    remDr$navigate(링크_sports.news[i])
    body <- remDr$getPageSource()[[1]]
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    body <- body %>% read_html()
    
    
    ##언론사(최종수정기사입력)
    
    언론사.tmp1 <- body %>% 
      html_nodes("div.link_popular_news") %>% 
      html_nodes("span.logo") %>% 
      html_text()  
    
    if (length(언론사.tmp1) != 0) {
      언론사_sports.news <- append(언론사_sports.news, 언론사.tmp1)
    } else {
      언론사_sports.news <- append(언론사_sports.news, "수동확인")
    }
    
    
    ##날짜(최종수정기사입력)
    
    날짜.tmp1 <- body %>% 
      html_nodes("div.info") %>% 
      html_nodes("span") %>% 
      html_text()  
    
    날짜.tmp1 <- 날짜.tmp1[2]
    
    if (length(날짜.tmp1) != 0) {
      날짜_sports.news <- append(날짜_sports.news, 날짜.tmp1)
    } else {
      날짜_sports.news <- append(날짜_sports.news, "수동확인")
    }
    
    
    ##제목
    
    제목.tmp1 <- body %>% 
      html_nodes("h4.title") %>% 
      html_text()
    
    if (length(제목.tmp1) != 0) {
      제목_sports.news <- append(제목_sports.news, 제목.tmp1)
    } else {
      제목_sports.news <- append(제목_sports.news, "수동확인")
    }
    
    
    ##본문
    
    본문.tmp1 <- body %>% 
      html_nodes("div#newsEndContents") %>% 
      html_text()
    
    if (length(본문.tmp1) != 0) {
      본문_sports.news <- append(본문_sports.news, 본문.tmp1)
    } else {
      본문_sports.news <- append(본문_sports.news, "수동확인")
    }
    
    
    ## 반응
    
    반응.tmp1 <- body %>% 
      html_nodes("ul.u_likeit_inline") %>% 
      html_text()
    
    반응.tmp1 <- str_replace_all(반응.tmp1,"\t","")
    반응.tmp1 <- strsplit(반응.tmp1, split="\n")
    
    
    #좋아
    
    좋아.tmp11 <- 반응.tmp1[[1]][3]
    좋아.tmp12 <- 반응.tmp1[[1]][4]
    
    좋아.tmp1 <- paste0(좋아.tmp11,":",좋아.tmp12)
    
    if (length(좋아.tmp1) != 0) {
      좋아_sports.news <- append(좋아_sports.news, 좋아.tmp1)
    } else {
      좋아_sports.news <- append(좋아_sports.news, "수동확인")
    }
    
    
    #슬퍼
    
    슬퍼.tmp11 <- 반응.tmp1[[1]][9]
    슬퍼.tmp12 <- 반응.tmp1[[1]][10]
    
    슬퍼.tmp1 <- paste0(슬퍼.tmp11,":",슬퍼.tmp12)
    
    if (length(슬퍼.tmp1) != 0) {
      슬퍼_sports.news <- append(슬퍼_sports.news, 슬퍼.tmp1)
    } else {
      슬퍼_sports.news <- append(슬퍼_sports.news, "수동확인")
    }
    
    
    #화나
    
    화나.tmp11 <- 반응.tmp1[[1]][15]
    화나.tmp12 <- 반응.tmp1[[1]][16]
    
    화나.tmp1 <- paste0(화나.tmp11,":",화나.tmp12)
    
    if (length(화나.tmp1) != 0) {
      화나_sports.news <- append(화나_sports.news, 화나.tmp1)
    } else {
      화나_sports.news <- append(화나_sports.news, "수동확인")
    }
    
    
    #팬이
    
    팬이.tmp11 <- 반응.tmp1[[1]][21]
    팬이.tmp12 <- 반응.tmp1[[1]][22]
    
    팬이.tmp1 <- paste0(팬이.tmp11,":",팬이.tmp12)
    
    if (length(팬이.tmp1) != 0) {
      팬이_sports.news <- append(팬이_sports.news, 팬이.tmp1)
    } else {
      팬이_sports.news <- append(팬이_sports.news, "수동확인")
    }
    
    
    #후속
    
    후속.tmp11 <- 반응.tmp1[[1]][27]
    후속.tmp12 <- 반응.tmp1[[1]][28]
    
    후속.tmp1 <- paste0(후속.tmp11,":",후속.tmp12)
    
    if (length(후속.tmp1) != 0) {
      후속_sports.news <- append(후속_sports.news, 후속.tmp1)
    } else {
      후속_sports.news <- append(후속_sports.news, "수동확인")
    }
    
    
    
    ##주소
    
    주소_sports.news <- append(주소_sports.news , 링크_sports.news[i])
    
    Sys.sleep(time = 1)
    
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}



##댓글

for (i in 1:length(링크_sports.news)){
  
  tryCatch({
    remDr$navigate(링크_sports.news[i])
    body <- remDr$getPageSource()[[1]]
    body <- body %>% read_html()
    
    
    Sys.sleep(time = 1)
    
    #전체 댓글 더보기
    
    reply <- remDr$findElement("css", "span.u_cbox_in_view_comment")
    
    reply$clickElement()
    
    
    #더보기
    
    for(j in 0:500){
      tryCatch({
        body <- remDr$getPageSource()[[1]]
        body <- body %>% read_html()
        
        j <- j+1    
        
        more_reply <- remDr$findElement("css", "span.u_cbox_page_more")
        
        more_reply$clickElement()
        
        if(j==500) break()
        
        Sys.sleep(time = 0.01)
        
      }, error = function(e) cat("불러올 수 없습니다!\n"))
      
    }
    
    Sys.sleep(time = 1)
    
    
    #댓글+댓글수
    
    body <- remDr$getPageSource()[[1]]
    body <- body %>% read_html()
    
    댓글.tmp1 <- body %>% 
      html_nodes("span.u_cbox_contents") %>% 
      html_text()
    
    댓글수.tmp1 <- length(댓글.tmp1)
    
    if (length(댓글.tmp1) != 0) {
      댓글_sports.news <- append(댓글_sports.news, 댓글.tmp1)
    } else {
      댓글_sports.news <- append(댓글_sports.news, "수동확인")
    }
    
    if (length(댓글수.tmp1) != 0) {
      댓글수_sports.news <- append(댓글수_sports.news, 댓글수.tmp1)
    } else {
      댓글수_sports.news <- append(댓글수_sports.news, "0")
    }
    
    
    ##주소
    
    주소_sports.news.re <- append(주소_sports.news.re, 링크_sports.news[i])
    
    Sys.sleep(time = 1)
    
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}


## 길이확인
length(언론사_sports.news)

length(날짜_sports.news)
length(제목_sports.news)
length(본문_sports.news)
length(주소_sports.news)

length(좋아_sports.news)
length(슬퍼_sports.news)
length(화나_sports.news)
length(팬이_sports.news)
length(후속_sports.news)

length(댓글_sports.news)
length(댓글수_sports.news)
length(주소_sports.news.re)



######## 2.news.naver

news.naver <- grep("https://news.naver",링크_nnews)
링크_news.naver <- 링크_nnews[news.naver]

링크_news.naver %>% head()

언론사_news.naver <- c()

날짜_news.naver <- c()
제목_news.naver <- c()
본문_news.naver <- c()

주소_news.naver <- c()

좋아_news.naver <- c()
슬퍼_news.naver <- c()
화나_news.naver <- c()
팬이_news.naver <- c()
후속_news.naver <- c()

댓글_news.naver <- c()
댓글수_news.naver <- c()

주소_news.naver.re <- c() 

####

for (i in 1:length(링크_news.naver)){
  
  tryCatch({
    remDr$navigate(링크_news.naver[i])
    body <- remDr$getPageSource()[[1]]
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    body <- body %>% read_html()
    
    
    ##언론사(최종수정기사입력)
    
    언론사.tmp2 <- body %>% 
      html_nodes("div.link_news") %>% 
      html_nodes("h3") %>% 
      html_text()  
    
    언론사.tmp2 <- str_sub(언론사.tmp2,end=5)
    언론사.tmp2 <- 언론사.tmp2 %>% str_trim()
    
    if (length(언론사.tmp2) != 0) {
      언론사_news.naver <- append(언론사_news.naver, 언론사.tmp2)
    } else {
      언론사_news.naver <- append(언론사_news.naver, "수동확인")
    }
    
    
    ##날짜(최종수정기사입력)
    
    날짜.tmp2 <- body %>% 
      html_nodes("span.t11") %>% 
      html_text()  
    
    날짜.tmp2 <-날짜.tmp2[2]
    
    if (length(날짜.tmp1) != 0) {
      날짜_news.naver <- append(날짜_news.naver, 날짜.tmp1)
    } else {
      날짜_news.naver <- append(날짜_news.naver, "수동확인")
    }
    
    
    
    ##제목
    
    제목.tmp2 <- body %>% 
      html_nodes("h3.tts_head") %>% 
      html_text()
    
    if (length(제목.tmp2) != 0) {
      제목_news.naver <- append(제목_news.naver, 제목.tmp2)
    } else {
      제목_news.naver <- append(제목_news.naver, "수동확인")
    }
    
    
    ##본문
    
    본문.tmp2 <- body %>% 
      html_nodes("div#articleBodyContents") %>% 
      html_text()
    
    if (length(본문.tmp2) != 0) {
      본문_news.naver <- append(본문_news.naver, 본문.tmp2)
    } else {
      본문_news.naver <- append(본문_news.naver, "수동확인")
    }
    
    
    
    ## 반응
    
    반응.tmp1 <- body %>% 
      html_nodes("ul.u_likeit_layer") %>% 
      html_text()
    
    반응.tmp1 <- 반응.tmp1[1]
    
    반응.tmp1 <- str_replace_all(반응.tmp1,"\t","")
    반응.tmp1 <- strsplit(반응.tmp1, split="\n")
    
    
    #좋아
    
    좋아.tmp11 <- 반응.tmp1[[1]][3]
    좋아.tmp12 <- 반응.tmp1[[1]][4]
    
    좋아.tmp1 <- paste0(좋아.tmp11,":",좋아.tmp12)
    
    if (length(좋아.tmp1) != 0) {
      좋아_news.naver <- append(좋아_news.naver, 좋아.tmp1)
    } else {
      좋아_news.naver <- append(좋아_news.naver, "수동확인")
    }
    
    
    #슬퍼
    
    슬퍼.tmp11 <- 반응.tmp1[[1]][9]
    슬퍼.tmp12 <- 반응.tmp1[[1]][10]
    
    슬퍼.tmp1 <- paste0(슬퍼.tmp11,":",슬퍼.tmp12)
    
    if (length(슬퍼.tmp1) != 0) {
      슬퍼_news.naver <- append(슬퍼_news.naver, 슬퍼.tmp1)
    } else {
      슬퍼_news.naver <- append(슬퍼_news.naver, "수동확인")
    }
    
    
    #화나
    
    화나.tmp11 <- 반응.tmp1[[1]][15]
    화나.tmp12 <- 반응.tmp1[[1]][16]
    
    화나.tmp1 <- paste0(화나.tmp11,":",화나.tmp12)
    
    if (length(화나.tmp1) != 0) {
      화나_news.naver <- append(화나_news.naver, 화나.tmp1)
    } else {
      화나_news.naver <- append(화나_news.naver, "수동확인")
    }
    
    
    #팬이
    
    팬이.tmp11 <- 반응.tmp1[[1]][21]
    팬이.tmp12 <- 반응.tmp1[[1]][22]
    
    팬이.tmp1 <- paste0(팬이.tmp11,":",팬이.tmp12)
    
    if (length(팬이.tmp1) != 0) {
      팬이_news.naver <- append(팬이_news.naver, 팬이.tmp1)
    } else {
      팬이_news.naver <- append(팬이_news.naver, "수동확인")
    }
    
    
    #후속
    
    후속.tmp11 <- 반응.tmp1[[1]][27]
    후속.tmp12 <- 반응.tmp1[[1]][28]
    
    후속.tmp1 <- paste0(후속.tmp11,":",후속.tmp12)
    
    if (length(후속.tmp1) != 0) {
      후속_news.naver <- append(후속_news.naver, 후속.tmp1)
    } else {
      후속_news.naver <- append(후속_news.naver, "수동확인")
    }
    
    
    
    ##주소
    
    주소_news.naver <- append(주소_news.naver , 링크_news.naver[i])
    
    Sys.sleep(time = 1)
    
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}



##댓글

댓글_news.naver <- c()
댓글수_news.naver <- c()
주소_news.naver.re <- c()


for (i in 1:length(링크_news.naver)){
  
  tryCatch({
    remDr$navigate(링크_news.naver[i])
    body <- remDr$getPageSource()[[1]]
    body <- body %>% read_html()
    
    
    Sys.sleep(time = 1)
    
    
    #전체 댓글 더보기
    
    reply <- remDr$findElement("css", "span.u_cbox_in_view_comment")
    reply$clickElement()
    
    
    #더보기
    body <- remDr$getPageSource()[[1]]
    body <- body %>% read_html()
    
    for(j in 0:500){
      tryCatch({
        body <- remDr$getPageSource()[[1]]
        body <- body %>% read_html()
        
        j <- j+1    
        
        more_reply <- remDr$findElement("css", "span.u_cbox_page_more")
        
        more_reply$clickElement()
        
        if(j==500) break()
        
        Sys.sleep(time = 0.01)
        
      }, error = function(e) cat("불러올 수 없습니다!\n"))
      
    }
    
    Sys.sleep(time = 1)
    
    
    #댓글+댓글수

    body <- remDr$getPageSource()[[1]]
    body <- body %>% read_html()
    
    댓글.tmp1 <- body %>% 
      html_nodes("span.u_cbox_contents") %>% 
      html_text()
    
    댓글수.tmp1 <- length(댓글.tmp1)
    
    if (length(댓글.tmp1) != 0) {
      댓글_news.naver <- append(댓글_news.naver, 댓글.tmp1)
    } else {
      댓글_news.naver <- append(댓글_news.naver, "수동확인")
    }
    
    if (length(댓글수.tmp1) != 0) {
      댓글수_news.naver <- append(댓글수_news.naver, 댓글수.tmp1)
    } else {
      댓글수_news.naver <- append(댓글수_news.naver, "0")
    }
    
    
    ##주소
    
    주소_news.naver.re <- append(주소_news.naver.re, 링크_news.naver[i])
    
    Sys.sleep(time = 1)
    
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}



## 길이확인

length(날짜_news.naver)
length(제목_news.naver)
length(본문_news.naver)
length(주소_news.naver)

length(좋아_news.naver)
length(슬퍼_news.naver)
length(화나_news.naver)
length(팬이_news.naver)
length(후속_news.naver)

length(댓글_news.naver)
length(댓글수_news.naver)
length(주소_news.naver.re)



###### 데이터 전처리

본문_sports.news <- str_replace_all(본문_sports.news,"\n", "")
본문_sports.news <- str_replace_all(본문_sports.news,"\t", "")
본문_sports.news <- str_replace_all(본문_sports.news,"//", "")
본문_sports.news <- str_replace_all(본문_sports.news,"flash 오류를 우회하기 위한 함수 추가", "")
본문_sports.news <- str_replace_all(본문_sports.news,"function _flash_removeCallback()", "")
본문_sports.news <- str_replace_all(본문_sports.news,"\\()", "")
본문_sports.news <- str_replace_all(본문_sports.news,"\\{}", "")


본문_news.naver <- str_replace_all(본문_news.naver,"\n", "")
본문_news.naver <- str_replace_all(본문_news.naver,"\t", "")
본문_news.naver <- str_replace_all(본문_news.naver,"//", "")
본문_news.naver <- str_replace_all(본문_news.naver,"flash 오류를 우회하기 위한 함수 추가", "")
본문_news.naver <- str_replace_all(본문_news.naver,"function _flash_removeCallback()", "")
본문_news.naver <- str_replace_all(본문_news.naver,"\\()", "")
본문_news.naver <- str_replace_all(본문_news.naver,"\\{}", "")





## 하나의 데이터프레임으로 합침

paper_sports.news <- data.frame(날짜_sports.news, 제목_sports.news, 본문_sports.news,주소_sports.news,좋아_sports.news,슬퍼_sports.news,화나_sports.news,팬이_sports.news,후속_sports.news)

paper_sports.news <- rename(paper_sports.news,c("날짜" = 날짜_sports.news,
                                                "제목" = 제목_sports.news,
                                                "본문" = 본문_sports.news,
                                                "주소" = 주소_sports.news,
                                                "좋아요" = 좋아_sports.news,
                                                "슬퍼요" = 슬퍼_sports.news, 
                                                "화나요" = 화나_sports.news,
                                                "팬이에요" = 팬이_sports.news,
                                                "후속기사" = 후속_sports.news))

paper_news.naver <- data.frame(날짜_news.naver, 제목_news.naver, 본문_news.naver,주소_news.naver,좋아_news.naver,슬퍼_news.naver,화나_news.naver,팬이_news.naver,후속_news.naver)


paper_news.naver <- rename(paper_news.naver,c("날짜" = 날짜_news.naver,
                                              "제목" = 제목_news.naver,
                                              "본문" = 본문_news.naver,
                                              "주소" = 주소_news.naver,
                                              "좋아요" = 좋아_news.naver,
                                              "슬퍼요" = 슬퍼_news.naver, 
                                              "화나요" = 화나_news.naver,
                                              "팬이에요" = 팬이_news.naver,
                                              "후속기사" = 후속_news.naver))

paper_nnews <- rbind(paper_sports.news,paper_news.naver)


write.csv(paper_nnews, file = "D:/paper_nnews_다이노스.csv", row.names=FALSE)




#### GET/POST

## 링크수집


searchword <- "다이노스"

seq <- seq(from = 1, by = 10, length.out = 200)

sd <- "2020.01.01"
ed <- "2020.03.29"


res_nnews <- GET(url = 'https://search.naver.com/search.naver',
                 query = list(
                   where = "news",
                   query = searchword,
                   sm = "tab_srt",
                   sort = 1,
                   photo = 0,
                   field = 0,
                   reporter_article = "", 
                   pd = 3,
                   ds = sd,
                   de = ed,
                   docid ="" ,
                   nso = "so:r,p:from20200101to20200331,a:all",
                   mynews = 1,
                   start = seq[1],
                   refresh_start = 0))


n <- 192

for (i in 1:length(링크_news.naver)){
  
  tryCatch({
    
    ##링크
    
    링크_nnews <- c()
    
    링크.tmp1 <- res_nnews %>%  
      read_html() %>% 
      html_nodes("dd.txt_inline") %>%
      html_nodes("a._sp_each_url") %>%
      html_attr("href")
    
    링크.tmp2 <- res_nnews %>%  
      read_html() %>% 
      html_nodes("span.txt_sinfo") %>%
      html_nodes("a._sp_each_url") %>%
      html_attr("href")
    
    링크.tmp <- append(링크.tmp1,링크.tmp2)
    링크.tmp <- 링크.tmp %>% unique()
    
    링크_nnews <- append(링크_nnews,링크.tmp)
    
    
    
    ##언론사
    
    언론사_nnews <- c()
    
    언론사.tmp1 <- res_nnews %>%  
      read_html() %>% 
      html_nodes("span._sp_each_source") %>%
      html_text()
    
    언론사.tmp2 <- res_nnews %>%  
      read_html() %>% 
      html_nodes("span.press") %>%
      html_text()
    
    언론사.tmp <- append(언론사.tmp1,언론사.tmp2)
    
    언론사_nnews <- append(언론사_nnews,언론사.tmp)
    
    
    Sys.sleep(time = 1)
    
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}




#### 기사 페이지

제목_nnews <- c()
날짜_nnews <- c()
본문_nnews <- c()


## 제목

제목.tmp <- 링크_nnews[1] %>% 
  read_html() %>% 
  html_nodes("span.logo") %>%
  html_text()

if (length(제목.tmp) == 0) {  
  제목_nnews <- append(제목_nnews, "수동확인")  
} else {  
  제목_nnews <- append(제목_nnews, 제목.tmp)  
}  



## 날짜

날짜.tmp1 <- 링크_nnews[1] %>% 
  read_html() %>% 
  html_nodes("div.article_info") %>%
  html_nodes("span:nth-child(1)") %>%
  html_nodes("em") %>%
  html_text()

날짜.tmp2 <- 링크_nnews[1] %>% 
  read_html() %>% 
  html_nodes("div.article_info") %>%
  html_nodes("span:nth-child(2)") %>%
  html_nodes("em") %>%
  html_text()


if (length(날짜.tmp2) != 0) {  
  날짜_nnews <- append(날짜_nnews, 날짜.tmp2)  
} else if (length(날짜.tmp2) == 0){  
  날짜_nnews <- append(날짜_nnews, 날짜.tmp1)  
} else {
  날짜_nnews <- append(날짜_nnews, "수동확인")  
}



## 본문

본문.tmp <- 링크_nnews[1] %>% 
  read_html() %>% 
  html_nodes("div#articeBody.article_body.font1.size3") %>%
  html_text()

if (length(본문.tmp) == 0) {  
  본문_nnews <- append(본문_nnews, "수동확인")  
} else {  
  본문_nnews <- append(본문_nnews, 본문.tmp)  
}  


## 본문

본문.tmp <- 링크_nnews[1] %>% 
  read_html() %>% 
  html_nodes("span.u_cbox_count") %>%
  html_text()

if (length(본문.tmp) == 0) {  
  본문_nnews <- append(본문_nnews, "수동확인")  
} else {  
  본문_nnews <- append(본문_nnews, 본문.tmp)  
}  



##주소
주소_nnews <- append(주소_nnews, 링크_nnews[i])






##댓글개수

##링크분해

링크_nnews_sp <- strsplit(링크_nnews, split="&")  

## oid

링크_nnews_sp1 <- c()
링크_nnews_sp2 <- c()


for (i in 1:length(링크_nnews_sp)){
  링크_nnews_sp1.tmp <- 링크_nnews_sp[[i]][1]
  링크_nnews_sp2.tmp <- 링크_nnews_sp[[i]][2]
  
  링크_nnews_sp1 <- append(링크_nnews_sp1,링크_nnews_sp1.tmp)
  링크_nnews_sp2 <- append(링크_nnews_sp2,링크_nnews_sp2.tmp)
}


링크_nnews_aid <- 링크_nnews_sp2


## oid

링크_nnews_sp1 <- strsplit(링크_nnews_sp1, split="\\?")  

링크_nnews_sp3 <- c()


for (i in 1:length(링크_nnews_sp1)){
  링크_nnews_sp3.tmp <- 링크_nnews_sp1[[i]][2]
  
  링크_nnews_sp3 <- append(링크_nnews_sp3,링크_nnews_sp3.tmp)
}

링크_nnews_oid <- 링크_nnews_sp3


링크_nnews_oid <- str_replace_all(링크_nnews_oid,"oid=","")
링크_nnews_aid <- str_replace_all(링크_nnews_aid,"aid=","")
링크_nnews_object <- paste0("news",링크_nnews_oid,",",링크_nnews_aid)




##댓글수
##셀레니움

댓글수.tmp <- body %>%  
  html_nodes("span#newsCommentCount") %>% 
  html_text()

if (length(댓글수.tmp) == 0) {
  댓글수_nnews <- append(댓글수_nnews, "수동확인")
} else {
  댓글수_nnews <- append(댓글수_nnews, 댓글수.tmp)
}



