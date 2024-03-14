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
검색어 <- "김경수"

mid1 <- "&sm=tab_srt&sort=1&photo=0&field=0&reporter_article=&pd=3&ds="
시작일1 <- "2018.07.01"
부터1 <- "&de="
종료일1 <- "2020.09.30"

mid2 <- "&docid=&nso=so%3Ar%2Cp%3Afrom"
시작일2 <- "20180701"
부터2 <- "to"
종료일2 <- "20200930"

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

## 언론사 선택(일간지)
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



########## 관련순
element <- remDr$findElement("css", "div.news_option > 
                                     ul.sort > 
                                     li:nth-child(1)")
element$clickElement()
Sys.sleep(time = 0.5)



## 링크
링크_nnews <- c()
언론사_nnews<- c()

n <- 10000

for(i in 1:n){
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
    
    
    if(i==10000) break()
    
    element <- remDr$findElement("css", "#main_pack > 
                             div.news.mynews.section._prs_nws>
                             div.paging >
                             a.next")
    element$clickElement()
    
    Sys.sleep(time = 0.5)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}


링크_nnews %>% length()

write.csv(링크_nnews, "D:/대학원/소논문/텍스트마이닝_김/링크_nnews.csv")

링크_nnews %>% head()


##수집
언론사_nnews <- c()

날짜_nnews <- c()
제목_nnews <- c()
본문_news <- c()
주소_news <- c()

좋아_news <- c()
슬퍼_news <- c()
화나_news <- c()
팬이_news <- c()
후속_news <- c()

댓글_news <- c()
댓글수_news <- c()

주소_news.re <- c() 


#### 링크분할
nnews_sid1 <- c()
nnews_oid <- c()
nnews_aid <- c()

링크_nnews_1 <- str_replace_all(링크_nnews,"https://news.naver.com/main/read.nhn","")
링크_nnews_1 <- strsplit(링크_nnews_1, split="\\?")

링크_nnews_2 <- c()

for(i in 1:length(링크_nnews_1)){
  링크_nnews_1.tmp <- 링크_nnews_1[[i]][2]
  링크_nnews_2 <- append(링크_nnews_2,링크_nnews_1.tmp)
}

링크_nnews_3 <- strsplit(링크_nnews_2, split="\\&")
링크_nnews_3 %>% head()

링크_nnews_sid <- c()
링크_nnews_oid <- c()
링크_nnews_aid <- c()

for(i in 1:length(링크_nnews_3)){
  링크_nnews_sid.tmp <- 링크_nnews_3[[i]][3]
  링크_nnews_oid.tmp <- 링크_nnews_3[[i]][4]
  링크_nnews_aid.tmp <- 링크_nnews_3[[i]][5]
  
  링크_nnews_sid <- append(링크_nnews_sid,링크_nnews_sid.tmp)
  링크_nnews_oid <- append(링크_nnews_oid,링크_nnews_oid.tmp)
  링크_nnews_aid <- append(링크_nnews_aid,링크_nnews_aid.tmp)
}

링크_nnews_sid <- strsplit(링크_nnews_sid, split="sid1=","")
링크_nnews_oid <- strsplit(링크_nnews_oid, split="oid1=","")
링크_nnews_aid <- strsplit(링크_nnews_aid, split="aid1=","")

링크_nnews_sid1 <- c()
링크_nnews_oid1 <- c()
링크_nnews_aid1 <- c()

for(i in 1:length(링크_nnews_sid)){
 링크_nnews_sid.tmp <- 링크_nnews_sid[[i]][2]
 링크_nnews_oid.tmp <- 링크_nnews_oid[[i]][2]
 링크_nnews_aid.tmp <- 링크_nnews_aid[[i]][2]
  
 링크_nnews_sid1 <- append(링크_nnews_sid1,링크_nnews_sid.tmp)
 링크_nnews_oid1 <- append(링크_nnews_oid1,링크_nnews_oid.tmp)
 링크_nnews_aid1 <- append(링크_nnews_aid1,링크_nnews_aid.tmp)
 }

링크_nnews_sid1 %>% length()
링크_nnews_oid1 %>% length()
링크_nnews_aid1 %>% length()

####
for (i in 1:length(링크_news)){
    tryCatch({
    res_nnews <- GET(url = 'https://news.naver.com/main/read.nhn',
                     query = list(
                       mode = "LSD",
                       mid = "sec",
                       sid1 = nnews_sid1,
                       oid = nnews_oid,
                       aid = nnews_aid))
    
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    body <- body %>% read_html()
    
    
    ##언론사(최종수정기사입력)
    
    언론사.tmp1 <- body %>% 
      html_nodes("div.link_popular_news") %>% 
      html_nodes("span.logo") %>% 
      html_text()  
    
    if (length(언론사.tmp1) != 0) {
      언론사_news <- append(언론사_news, 언론사.tmp1)
    } else {
      언론사_news <- append(언론사_news, "수동확인")
    }
    
    
    ##날짜(최종수정기사입력)
    
    날짜.tmp1 <- body %>% 
      html_nodes("div.info") %>% 
      html_nodes("span") %>% 
      html_text()  
    
    날짜.tmp1 <- 날짜.tmp1[2]
    
    if (length(날짜.tmp1) != 0) {
      날짜_news <- append(날짜_news, 날짜.tmp1)
    } else {
      날짜_news <- append(날짜_news, "수동확인")
    }
    
    
    ##제목
    
    제목.tmp1 <- body %>% 
      html_nodes("h4.title") %>% 
      html_text()
    
    if (length(제목.tmp1) != 0) {
      제목_news <- append(제목_news, 제목.tmp1)
    } else {
      제목_news <- append(제목_news, "수동확인")
    }
    
    
    ##본문
    
    본문.tmp1 <- body %>% 
      html_nodes("div#newsEndContents") %>% 
      html_text()
    
    if (length(본문.tmp1) != 0) {
      본문_news <- append(본문_news, 본문.tmp1)
    } else {
      본문_news <- append(본문_news, "수동확인")
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
      좋아_news <- append(좋아_news, 좋아.tmp1)
    } else {
      좋아_news <- append(좋아_news, "수동확인")
    }
    
    
    #슬퍼
    
    슬퍼.tmp11 <- 반응.tmp1[[1]][9]
    슬퍼.tmp12 <- 반응.tmp1[[1]][10]
    
    슬퍼.tmp1 <- paste0(슬퍼.tmp11,":",슬퍼.tmp12)
    
    if (length(슬퍼.tmp1) != 0) {
      슬퍼_news <- append(슬퍼_news, 슬퍼.tmp1)
    } else {
      슬퍼_news <- append(슬퍼_news, "수동확인")
    }
    
    
    #화나
    
    화나.tmp11 <- 반응.tmp1[[1]][15]
    화나.tmp12 <- 반응.tmp1[[1]][16]
    
    화나.tmp1 <- paste0(화나.tmp11,":",화나.tmp12)
    
    if (length(화나.tmp1) != 0) {
      화나_news <- append(화나_news, 화나.tmp1)
    } else {
      화나_news <- append(화나_news, "수동확인")
    }
    
    
    #팬이
    
    팬이.tmp11 <- 반응.tmp1[[1]][21]
    팬이.tmp12 <- 반응.tmp1[[1]][22]
    
    팬이.tmp1 <- paste0(팬이.tmp11,":",팬이.tmp12)
    
    if (length(팬이.tmp1) != 0) {
      팬이_news <- append(팬이_news, 팬이.tmp1)
    } else {
      팬이_news <- append(팬이_news, "수동확인")
    }
    
    
    #후속
    
    후속.tmp11 <- 반응.tmp1[[1]][27]
    후속.tmp12 <- 반응.tmp1[[1]][28]
    
    후속.tmp1 <- paste0(후속.tmp11,":",후속.tmp12)
    
    if (length(후속.tmp1) != 0) {
      후속_news <- append(후속_news, 후속.tmp1)
    } else {
      후속_news <- append(후속_news, "수동확인")
    }
    
    
    
    ##주소
    
    주소_news <- append(주소_news , 링크_news[i])
    
    Sys.sleep(time = 1)
    
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}






#### GET/POST

## 링크수집
searchword <- "김경수"

seq <- seq(from = 1, by = 10, length.out = 110000)

sd <- "2018.07.01"
ed <- "2020.09.22"

n <- 1081

링크_nnews <- c()
언론사_nnews <- c()

for (i in 1:n){
  
  tryCatch({
      res_nnews <- GET(url = 'https://search.naver.com/search.naver',
                     query = list(
                       where = "news",
                       query = searchword,
                       sm = "tab_pge",
                       sort = 0,
                       photo = 0,
                       field = 0,
                       reporter_article = "", 
                       pd = 3,
                       ds = sd,
                       de = ed,
                       docid ="" ,
                       nso = "so:r,p:from20180701to20200922,a:all",
                       mynews = 0,
                       start = seq[i],
                       refresh_start = 0))
    
    cat('현재', i, '페이지 수집 중! \n') 
    
    ##링크

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
   
    링크_nnews <- append(링크_nnews,링크.tmp)
    
    
    
    ##언론사
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
    
    
    Sys.sleep(time = 0.1)
    
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}




