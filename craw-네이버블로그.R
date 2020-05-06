

##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445


#### RSelnium

remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()



#####네이버 블로그(최신순)

main <- "https://search.naver.com/search.naver?where=post&query="
검색어 <- "스토브리그"

mid1 <- "&st=sim&sm=tab_opt&date_from="
시작일1 <- "20191201"
부터1 <- "&date_to="
종료일1 <- "20191231"

mid2 <- "&date_option=8&srchby=all&dup_remove=1&post_blogurl=&post_blogurl_without=&nso=so%3Ar%2Ca%3Aall%2Cp%3Afrom"
시작일2 <- "20191201"
부터2 <- "to"
종료일2 <- "20191231"



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

remDr$navigate(target)




## 로그인 
element <- remDr$findElement("css", "span.gnb_txt")

element$clickElement()


## 아이디 비밀번호 #자동로그인안됨
id <- remDr$findElement(using = "css", value="input#id")
id$sendKeysToElement(list("1125mirhan"))

Sys.sleep(time = 3)

pw <- remDr$findElement(using = "css", value="input#pw")
pw$sendKeysToElement(list("zl5slglf"))


login <- remDr$findElement(using = "css", value="input.btn_global")
login$clickElement()



##최신 정렬
lineup <- remDr$findElement("css", "ul.option_menu > 
                                    li.menu > 
                                    a")
lineup$clickElement()


latest <- remDr$findElement("css", "ul.lst_choice > 
                                    li:nth-child(2) > 
                                    a")
latest$clickElement()



## 링크 & 제목


링크_blog <- c()
제목_blog <- c()


for(i in 0:224){
  tryCatch({
    
    i <- i+1
    cat('현재', i, '페이지 수집 중 \n') 
    
    
    frontpage <- remDr$getPageSource()[[1]]
    tmp <- read_html(frontpage) 
    
    링크.tmp <- tmp %>%  
      html_nodes("span.inline") %>%
      html_nodes("a:nth-child(2)") %>%
      html_attr("href") %>%
      unique()
    
    링크_blog <- append(링크_blog,링크.tmp) 
    
    
    제목.tmp <- tmp %>%  
      html_nodes("li.sh_blog_top") %>%
      html_nodes("dl") %>%
      html_nodes("dt") %>%
      html_nodes("a:nth-child(1)") %>%
      html_text()
    
    제목_blog <- append(제목_blog,제목.tmp) 
    
    
    if(i==224) break()
    
    nextpage <- remDr$findElement("css", "div.paging >
                                         a.next")
    nextpage$clickElement()
    
    Sys.sleep(time = 0.1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}

length(링크_blog)
length(제목_blog)



## 링크 분류 및 전처리

blog.naver.com <- grep("https://blog.naver.com/",링크_blog)

링크_blog_n <- 링크_blog[blog.naver.com]
링크_blog_e <- 링크_blog[-blog.naver.com]


링크_blog_n <- str_replace_all(링크_blog_n,"https://blog.naver.com/","")
링크_blog_n <- str_replace_all(링크_blog_n,"Redirect=Log&logNo=","")
링크_blog_n <- strsplit(링크_blog_n, split="\\?")


링크_blog_e <- str_replace_all(링크_blog_e,"https://","")
링크_blog_e <- str_replace_all(링크_blog_e,".blog.me","")
링크_blog_e <- strsplit(링크_blog_e, split="/")


링크_blog_ne <- append(링크_blog_n,링크_blog_e)



## 링크 ID No 분류

링크_blog_ID <- c()
링크_blog_No <- c()



for(i in 1:length(링크_blog)){
  링크_blog_ID.tmp <- 링크_blog_ne[[i]][1]
  링크_blog_No.tmp <- 링크_blog_ne[[i]][2]
  
  링크_blog_ID <- append(링크_blog_ID, 링크_blog_ID.tmp)
  링크_blog_No <- append(링크_blog_No, 링크_blog_No.tmp)
  
}

링크_blog_IN <- data.frame(링크_blog_ID,링크_blog_No)



########

날짜_blog <- c()
작성자_blog <- c()
타이틀_blog <- c()
본문_blog <- c()
주소_blog <- c()


##

for(i in 1:length(링크_blog)){
  tryCatch({
    res_blog <- GET(url = 'https://blog.naver.com/PostView.nhn',
                    query = list(blogId = 링크_blog_ID[i], 
                                 logNo = 링크_blog_No[i]))
    
    
    cat('현재', i, '페이지 수집 중! 상태코드는', status_code(x = res_blog), '입니다.\n')
    
    
    #날짜
    
    날짜.tmp <- res_blog %>% 
      read_html() %>% 
      html_nodes("div.blog2_container") %>%
      html_nodes("span.se_publishDate.pcol2") %>%
      html_text() %>% 
      unique()
    
    
    날짜.tmp <- 날짜.tmp[length(날짜.tmp)]
    
    if (length(날짜.tmp) == 0) {
      날짜_blog <- append(날짜_blog, "수동확인")
    } else {
      날짜_blog <- append(날짜_blog, 날짜.tmp)
    }
    
    
    #작성자
    
    작성자.tmp <- res_blog %>% 
      read_html() %>% 
      html_nodes("span.nick") %>%
      html_text() %>% 
      unique()
    
    if (length(작성자.tmp) == 0) {
      작성자_blog <- append(작성자_blog, "수동확인")
    } else {
      작성자_blog <- append(작성자_blog, 작성자.tmp)
    }
    
    
    #제목
    
    타이틀_blog <- append(타이틀_blog, 제목_blog[i])
    
    
    #본문
    
    본문.tmp <- res_blog %>% 
      read_html() %>% 
      html_nodes("div.se-module.se-module-text") %>%
      html_nodes("p") %>%
      html_text() %>% 
      unique()
    
    본문.tmp <- 본문.tmp %>% paste(collapse = " ")
    
    if (length(본문.tmp) == 0) {
      본문_blog <- append(본문_blog, "수동확인")
    } else {
      본문_blog <- append(본문_blog, 본문.tmp)
    }
    
    
    #주소
    
    주소_blog <- append(주소_blog, 링크_blog[i])
    
    
    Sys.sleep(time = 0.1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
  
}


length(날짜_blog)
length(작성자_blog)
length(타이틀_blog)
length(본문_blog)
length(주소_blog)



## 하나의 데이터프레임으로 합침
paper_blog1 <- data.frame(날짜_blog, 작성자_blog, 타이틀_blog, 본문_blog, 주소_blog)

View(paper_blog)

write.csv(paper_blog, file = "D:/성재/크롤링/paper_blog1_스토브리그.csv", row.names=FALSE)









###네이버블로그 링크 GET/POST

링크_blog2 <- c() #벡터생성


###
searchword <- "스토브리그"
시작일 <- "2019-12-01" #날짜마다 바꿔줘야함
종료일 <- "2019-12-31"

usethis::edit_r_environ()
NAVER_BLOG_REF <- 'https://section.blog.naver.com/BlogHome.nhn'
Sys.getenv('NAVER_BLOG_REF')


n <- 288 #최대 제공 페이지 수

for(i in 1:n){
  tryCatch({
    res_blog2 <- GET(url = "https://section.blog.naver.com/ajax/SearchList.nhn",
                     query = list(countPerPage = 7,
                                  currentPage = 1,
                                  endDate = 종료일,
                                  keyword = searchword,
                                  orderBy = "sim",
                                  startDate = 시작일,
                                  type = "post"))
    add_headers(referer = Sys.getenv('NAVER_BLOG_REF'))
    
    cat('현재', i, '페이지 수집 중! 상태코드는', status_code(x = res_daum), '입니다.\n')
    
    
    링크.tmp <- res_blog2 %>% 
      read_html() %>% 
      html_nodes("div.desc") %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      unique()
    
    
    링크_blog2 <- append(링크_blog2, 링크.tmp) 
    
    
    Sys.sleep(time = 0.1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
  
}