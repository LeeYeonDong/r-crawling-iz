링크_NC <- c() ## "링크_NC" 라는 빈 벡터를 만듭니다

n <- 5800

for(i in 1:n){
  tryCatch({
    
    res_nc <- GET(url = 'https://gall.dcinside.com/board/lists/',
                   query = list(id = 'ncdinos',
                                page = i))
    
    
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_nc), '입니다.\n')
    
    
    
    #### NC갤 링크 수집
    
    링크.NC.tmp <- res_nc %>%
      read_html() %>% 
      html_nodes('td.gall_tit.ub-word') %>% ##링크 주소 바로 위에 있는 Element입니다
      html_nodes('a:nth-child(1)') %>% ## 게시글 링크 주소는 td.gall_tit.ub-word 밑  첫번째 "a"에 있습니다 
      html_attr('href') %>% ## href="링크주소"를 긁는 명령어 입니다
      unique() ##중복되는 링크를 제거해주는 함수입니다
    
    
    if (length(링크.NC.tmp) == 0) {
      링크_NC <- append(링크_NC, "수동확인")
    } else {
      링크_NC <- append(링크_NC, 링크.NC.tmp)
    }  ## 수집한 링크가 없을경우 "수동확인", 제대로 수집한 경우는 "링크_NC"로 저장합니다 
    
    
    
    Sys.sleep(time = 0.05)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
  
}

링크_NC <- paste0("https://gall.dcinside.com/",링크_NC)

링크_NC <- 링크_NC[!(링크_NC=="https://gall.dcinside.com/javascript:;")] 
 

## 링크 분할
링크_NC_no <- c()
링크_NC_page <- c()

링크_NCsp <- strsplit(링크_NC, split="&")


for(i in 1:length(링크_NC)){
  링크_NC_no.tmp <- 링크_NCsp[[i]][2]
  링크_NC_page.tmp <- 링크_NCsp[[i]][4]
  
  링크_NC_no <- append(링크_NC_no, 링크_NC_no.tmp)
  링크_NC_page <- append(링크_NC_page, 링크_NC_page.tmp)
}


링크_NC_no <- str_replace_all(링크_NC_no,"no=","")
링크_NC_page <- str_replace_all(링크_NC_page,"page=","")


## 페이지별 element 수집 
제목_NC <- c()
작성자_NC <- c()
날짜_NC <- c()
본문_NC <-c()
주소_NC <-c()

링크_NC100 <- seq(from = 1, to = 100000, by = 100)

for(j in 1:length(링크_NC100)){
  
  Sys.sleep(time = 10)
  
for(i in 링크_NC100[j]:링크_NC100[j+1]-1){
  tryCatch({
    
    res_NC <- GET(url = 'https://gall.dcinside.com/board/view/',
                   query = list(id = 'ncdinos',
                                no = 링크_NC_no[i],
                                page = 링크_NC_page[i]))
    
    Sys.sleep(time = 0.1)
    
    cat(i, '페이지 수집 중. 상태코드는', status_code(x = res_NC), '입니다.\n')
    
    
    ## 제목
    제목.NC.tmp <- res_NC %>%
      read_html() %>% 
      html_nodes("span.title_subject") %>% 
      html_text()
    
    if (length(제목.NC.tmp) == 0) {
      제목_NC <- append(제목_NC, "수동확인")
    } else {
      제목_NC <- append(제목_NC, 제목.NC.tmp)
    }  ## 수집한 제목 없을 경우 "수동확인", 제대로 수집한 경우는 "제목_NC"벡터 공간에 저장합니다
    
    
    ## 작성자
    작성자.NC.tmp <- res_NC %>%
      read_html() %>% 
      html_nodes("div.fl") %>% 
      html_nodes("span.nickname") %>% 
      html_nodes("em") %>%
      html_text()
    
    
    if (length(제목.NC.tmp) == 0) {
      작성자_NC <- append(작성자_NC, "수동확인")
    } else {
      작성자_NC <- append(작성자_NC, 작성자.NC.tmp)
    }  
    
    
    ## 날짜
    날짜.NC.tmp <- res_NC %>%
      read_html() %>% 
      html_nodes("span.gall_date") %>% 
      html_text()
    
    
    if (length(날짜.NC.tmp) == 0) {
      날짜_NC <- append(날짜_NC, "수동확인")
    } else {
      날짜_NC <- append(날짜_NC, 날짜.NC.tmp)
    }  ## 수집한 날짜 없을 경우 "수동확인", 제대로 수집한 경우는 "날짜_NC"벡터 공간에 저장합니다
    
    
    ## 본문
    본문.NC.tmp <- res_NC %>%
      read_html() %>% 
      html_nodes("div.writing_view_box") %>% 
      html_text()
    

    if (length(본문.NC.tmp) == 0) {
      본문_NC <- append(본문_NC, "수동확인")
    } else {
      본문_NC <- append(본문_NC, 본문.NC.tmp)
    }  ## 수집한 본문 없을 경우 "수동확인", 제대로 수집한 경우는 "본문_NC"벡터 공간에 저장합니다
    
    
    ## 주소(URL)
    주소_NC <- append(주소_NC, 링크_NC[i])
    
    Sys.sleep(time = 0.1)
    
  }, error = function(e) cat("불러올 수 없습니다!\n"))
}
  Sys.sleep(time = 0.1)
}

제목_NC <- 제목_NC[-c(1:3)]
작성자_NC <- 작성자_NC[-c(1:6)]
날짜_NC <- 날짜_NC[-1]
본문_NC <- 본문_NC[-c(1:2)]
주소_NC <- 주소_NC[-1]

length(제목_NC)
length(작성자_NC)
length(날짜_NC)
length(본문_NC)
length(주소_NC)


## 데이터프레임으로 합치기 및 csv파일로 저장 
df_NC1 <- data.frame(제목_NC, 작성자_NC, 날짜_NC, 본문_NC, 주소_NC) 
write.csv(df_NC1, file = "D:/df_NC1.csv", row.names=FALSE) 

df_NC1 <- read.csv("D:/df_NC1.csv", header = TRUE)
df_NC1 %>% View()

## 데이터 전처리 
NC_words <- str_replace_all(NC_words,"\n","") 
NC_words <- str_replace_all(NC_words,"\t","") 
NC_words <- str_replace_all(NC_words,"\r","") 

NC_words <- str_replace_all(NC_words, "[^[:alnum:][:blank:]+?&/\\-]","")

NC_words <- str_replace_all(NC_words,"-","") 
NC_words <- str_replace_all(NC_words, "^.{1}$","") 
NC_words <- str_replace_all(NC_words,"\\^","")
NC_words <- str_replace_all(NC_words,'\\d+',"")
NC_words <- str_replace_all(NC_words,'ㅋ',"")
NC_words <- str_replace_all(NC_words,'ㅎ',"")

NC_words <- NC_words %>% as.list()
NC_words[NC_words ==""] <- NULL
NC_words <- NC_words %>% unlist()


NC_words %>% head(100)

#### 한글 - 품사별 처리
NC_words <- NC_words %>% unlist()
NC_words <- NC_words %>% as.vector()

NC_words <- NC_words %>% SimplePos09()

NC_words <- NC_words %>% as.data.frame()


#### 데이터 셋 만들기
NC_words <- NC_words %>%   
  melt() %>%  
  as_tibble() %>% 
  select(3,1)    ## 3열과 1열 추출 



#### 명사 용언 수식언만 추출하기
## 명사 추출
NC_words_명사 <- NC_words %>%  
  mutate(명사=str_match(value,'([가-힣]+)/N')[,2]) %>%      ## "명사" variable을 만들고 한글만 저장                   
  
  na.omit() %>%                                      ## ([가-힣]+)/P') 한글 중 명사(N)만을 선택하는 정규표현식
  
  mutate(글자수=str_length(명사)) %>%   ## "글자수" variable을 만듭니다 
  
  filter(str_length(명사)>=2)                               ## 2글자 이상만 추려냅니다




## 용언 추출
NC_words_용언 <- NC_words %>%  
  mutate(용언=str_match(value,'([가-힣]+)/P')[,2]) %>%   ## "용언" variable을 만들고 한글만 저장 
  na.omit() %>%                           ## ([가-힣]+)/P') 한글 중 용언(P)만을 선택하는 정규표현식
  
  mutate(글자수=str_length(용언)) %>%        ## "글자수" variable을 만듭니다 
  filter(str_length(용언)>=2)                         ##  2글자 이상만 추려냅니다


## 수식언 추출
NC_words_수식언 <- NC_words %>%  
  mutate(수식언=str_match(value,'([가-힣]+)/M')[,2]) %>%    ## "수식언" variable을 만들고 한글만 저장
  na.omit() %>%                                ## ([가-힣]+)/M') 한글 중 수식언(M)만을 선택하는 정규표현식
  mutate(글자수=str_length(수식언)) %>%  ## "글자수" variable을 만듭니다 
  filter(str_length(수식언)>=2)                 ##  2글자 이상만 추려냅니다




#### 데이터 전처리

##명사 전처리 
NC_words_명사 <- NC_words_명사$명사 
NC_words_명사 <- NC_words_명사 %>% unlist() 
NC_words_명사 <- NC_words_명사 %>% as.vector() 
NC_words_명사 <- str_replace_all(NC_words_명사, "\\^","")          ## 특수문자를 처리합니다
NC_words_명사 <- str_replace_all(NC_words_명사, "^.{1}$","")         ## 혹시라도  들어갈 수 있는 한글자를 처리합니다
NC_words_명사 <- str_replace_all(NC_words_명사, "\\d+","") 
NC_words_명사 <- NC_words_명사 %>% as.list() 
NC_words_명사[NC_words_명사 ==""] <- NULL 
NC_words_명사 <- NC_words_명사 %>% unlist()                         ## 공백을 제거합니다
NC_words_명사 <- NC_words_명사 %>% as.data.frame()


##용언 전처리
NC_words_용언 <- NC_words_용언$용언 
NC_words_용언 <- NC_words_용언 %>% unlist() 
NC_words_용언 <- NC_words_용언 %>% as.vector() 
NC_words_용언 <- str_replace_all(NC_words_용언, "\\^","")       ## 특수문자를 처리합니다
NC_words_용언 <- str_replace_all(NC_words_용언, "^.{1}$","")      ## 혹시라도  들어갈 수 있는 한글자를 처리합니다
NC_words_용언 <- NC_words_용언 %>% as.list() 
NC_words_용언[NC_words_용언 ==""] <- NULL 
NC_words_용언 <- NC_words_용언 %>% unlist()                    ## 공백을 제거합니다
NC_words_용언 <- NC_words_용언 %>% as.data.frame() 


##수식언 전처리
NC_words_수식언 <- NC_words_수식언$수식언 
NC_words_수식언 <- NC_words_수식언 %>% unlist() 
NC_words_수식언 <- NC_words_수식언 %>% as.vector() 
NC_words_수식언 <- str_replace_all(NC_words_수식언, "\\^","")    ## 특수문자를 처리합니다
NC_words_수식언 <- str_replace_all(NC_words_수식언, "^.{1}$","")   ## 혹시라도  들어갈 수 있는 한글자를 처리합니다
NC_words_수식언 <- NC_words_수식언 %>% as.list() 
NC_words_수식언[NC_words_수식언 ==""] <- NULL 
NC_words_수식언 <- NC_words_수식언 %>% unlist()                ## 공백을 제거합니다
NC_words_수식언 <- NC_words_수식언 %>% as.data.frame()          



#### 명사 용언 수식언을 묶어서 하나로 만듭니다
NC_words1 <- bind_rows(NC_words_명사,NC_words_용언,NC_words_수식언)

NC_words2 <- NC_words1
#### 최다 빈도 단어 Top30을 뽑습니다 시각화에 적합한 수준으로 데이터를 추려봅니다

NC_words1 <- table(NC_words1)                       ## 객체별 빈도를 셉니다
NC_count <- sort(NC_words1, decreasing = TRUE)         ##내림차순 정렬 합니다

NC_count50 <- NC_count[1:50]            ## Top 30까지 추립니다


#### 빈도그래프 작성
NC_count50df <- NC_count50 %>% as.data.frame()             ## data frame변환하고 그래프 작성 
ggplot(NC_count50df, aes(x=NC_words1, y=Freq)) + geom_bar(stat="identity")


#### 워드크라우드 작성
NC_count50df %>% wordcloud2(minRotation = 0, maxRotation = 0, minSize = 5,
                             rotateRatio = 1)



##noun
NC_words <- NC_words %>% extractNoun()
NC_words <- NC_words %>% as.list()
NC_words[NC_words ==""] <- NULL
NC_words <- NC_words %>% unlist()
NC_words <- NC_words %>% as.vector()
NC_words <- str_replace_all(NC_words, "^.{1}$","") 
NC_words <- str_replace_all(NC_words,"\\^","")
NC_words <- str_replace_all(NC_words,'\\d+',"")

NC_words_count <- NC_words %>% table()
NC_words_count_1_50 <- sort(NC_words_count, decreasing = TRUE)[2:50]
NC_words_count_1_50 %>% wordcloud2(minRotation = 0, maxRotation = 0, minSize = 5,
                                   rotateRatio = 1)

NC_words_count_1_50 %>% View()
