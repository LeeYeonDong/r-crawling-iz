
###
install_mecab("C:/Rlibs/mecab")

Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_241')
buildDictionary(ext_dic = "woorimalsam")
useNIADic()




####twitteR

api_key <- "BoDdKtDizn6IYFF8yTGCZDXe0"
api_secret_key <- "0SHvyAVTNGjdENJSAqrprGbV2Jcc7l182sFgTVOi8h17VIsc4A"
access_token <- "1125641588699324417-d18aL18ENQV5y7fPtLanwAiIskUJju"
access_token_secret <- "k6yoPmD8VtYIUhWycmBRaMXxoMDt99aNgKS6vqLbKzJWo"

options(httr_oauth_cache = TRUE)
setup_twitter_oauth(api_key,api_secret_key,access_token,access_token_secret)



#### 수집가능한 메타정보

getCurRateLimitInfo()


#### 키워드 설정

keyword_이 <- enc2utf8("이낙연")
keyword_황 <- enc2utf8("황교안")


keyword_ja <- enc2utf8("アイズワン")

keyword_en1 <- enc2utf8("IZONE")
keyword_en2 <- enc2utf8("izone")
keyword_en3 <- enc2utf8("IZ*ONE")
keyword_en4 <- enc2utf8("iz*one")


#### 크롤링

이_0405 <- searchTwitter(keyword_이, n=20000, lang="ko", since="2020-04-04", until="2020-04-05")
이_0607 <- searchTwitter(keyword_이, n=20000, lang="ko", since="2020-04-06", until="2020-04-07")
이_0809 <- searchTwitter(keyword_이, n=20000, lang="ko", since="2020-04-08", until="2020-04-09")
이_1011 <- searchTwitter(keyword_이, n=20000, lang="ko", since="2020-04-10", until="2020-04-11")

황_0405 <- searchTwitter(keyword_황, n=20000, lang="ko", since="2020-04-04", until="2020-04-05")
황_0607 <- searchTwitter(keyword_황, n=20000, lang="ko", since="2020-04-06", until="2020-04-07")
황_0809 <- searchTwitter(keyword_황, n=20000, lang="ko", since="2020-04-08", until="2020-04-09")
황_1011 <- searchTwitter(keyword_황, n=20000, lang="ko", since="2020-04-10", until="2020-04-11")




####df저장

이_0405_df <- twListToDF(이_0405)
이_0607_df <- twListToDF(이_0607)
이_0809_df <- twListToDF(이_0809)
이_1011_df <- twListToDF(이_1011)

이_df <- rbind(이_0405_df,이_0607_df,이_0809_df,이_1011_df)


황_0405_df <- twListToDF(황_0405)
황_0607_df <- twListToDF(황_0607)
황_0809_df <- twListToDF(황_0809)
황_1011_df <- twListToDF(황_1011)

황_df <- rbind(황_0405_df,황_0607_df,황_0809_df,황_1011_df)



#### 텍스트파일 추출

이_df_word <- 이_df$text
황_df_word <- 황_df$text


#### 텍스트 저장


write.table(이_df_word, file="D:/이_df_word.txt", sep = ",", row.names=FALSE) 
write.table(황_df_word, file="D:/황_df_word.txt", sep = ",", row.names=FALSE) 



#### 감정분석-이낙연

이낙연 <- readLines("D:/이_df_word.txt") 
이낙연 <- 이낙연 %>% as.list()
이낙연[이낙연 ==""] <- NULL


#데이터길이

수집트윗_이낙연 <- 이낙연 %>% length() 

word.pos <- readLines("D:/textmining/imotion/wordpos.txt") 
word.neg <- readLines("D:/textmining/imotion/wordneg.txt")



이낙연 <- sample(이낙연, size = 1000, replace = FALSE)

이낙연_words <- 이낙연 %>% SimplePos09()

이낙연 <- 이낙연_words


이낙연 <- 이낙연 %>% melt()
이낙연 <- 이낙연 %>% as_tibble()
이낙연_df <- 이낙연 %>% select(3,1)


이낙연_df_n <- 이낙연_df %>% mutate(명사=str_match(value,'([가-힣]+)/N')[,2]) %>% na.omit()
이낙연_df_p <- 이낙연_df %>% mutate(용언=str_match(value,'([가-힣]+)/P')[,2]) %>% na.omit()
이낙연_df_m <- 이낙연_df %>% mutate(수식언=str_match(value,'([가-힣]+)/M')[,2]) %>% na.omit()

word.n <- 이낙연_df_n[,3]
word.n <- word.n$명사
word.n <- word.n %>% unlist()
word.n <- word.n %>% as.vector()
word.n <- gsub("\\^","",word.n)
word.n <- gsub("^.{1}$","",word.n)
word.n <- gsub('\\d+',"", word.n)
word.n <- word.n %>% as.list()
word.n[word.n ==""] <- NULL
word.n <- word.n %>% unlist()
word.n <- word.n %>% as.data.frame()

word.p <- 이낙연_df_p[,3]
word.p <- word.p$용언
word.p <- word.p %>% unlist()
word.p <- word.p %>% as.vector()
word.p <- gsub("\\^","",word.p)
word.p <- gsub("^.{1}$","",word.p)
word.p <- gsub('\\d+',"", word.p)
word.p <- word.p %>% as.list()
word.p[word.p ==""] <- NULL
word.p <- word.p %>% unlist()
word.p <- word.p %>% as.data.frame()

word.m <- 이낙연_df_m[,3]
word.m <- word.m$수식언
word.m <- word.m %>% unlist()
word.m <- word.m %>% as.vector()
word.m <- gsub("\\^","",word.m)
word.m <- gsub("^.{1}$","",word.m)
word.m <- gsub('\\d+',"", word.m)
word.m <- word.m %>% as.list()
word.m[word.m ==""] <- NULL
word.m <- word.m %>% unlist()
word.m <- word.m %>% as.data.frame()

word <- rbind(word.n, word.p, word.m)


word <- word %>% unlist()
word <- word %>% as.vector()




scores <- laply(word, function(word, word.pos, word.neg) {
  
  words <- unlist(word)  
  
  pos.matches <- match(words, word.pos)           # words의 단어를 positive에서 matching
  neg.matches <- match(words, word.neg)
  
  pos.matches <- !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
  neg.matches <- !is.na(neg.matches)
  
  score <-  sum(pos.matches) - sum(neg.matches)  # 긍정 - 부정   
}, word.pos, word.neg)

scores.df <-  data.frame(score=scores, text=word)



scores.df$color[scores.df$score >=1] = "긍정"
scores.df$color[scores.df$score ==0] = "중립"
scores.df$color[scores.df$score < 0] = "부정"

scores.df.t <- table(scores.df$color)
scores.df.t <- scores.df.t %>% as.data.frame()

이낙연_결과 <- scores.df.t




#### 감정분석-황교안

황교안 <- readLines("D:/황_df_word.txt") 
황교안 <- 황교안 %>% as.list()
황교안[황교안 ==""] <- NULL


#데이터길이

수집트윗_황교안 <- 황교안 %>% length() 

황교안 <- sample(황교안, size = 1000, replace=FALSE)

word.pos <- readLines("D:/textmining/imotion/wordpos.txt") 
word.neg <- readLines("D:/textmining/imotion/wordneg.txt")


황교안_words <- 황교안 %>% SimplePos09()

황교안 <- 황교안_words


황교안 <- 황교안 %>% melt()
황교안 <- 황교안 %>% as_tibble()
황교안_df <- 황교안 %>% select(3,1)


황교안_df_n <- 황교안_df %>% mutate(명사=str_match(value,'([가-힣]+)/N')[,2]) %>% na.omit()
황교안_df_p <- 황교안_df %>% mutate(용언=str_match(value,'([가-힣]+)/P')[,2]) %>% na.omit()
황교안_df_m <- 황교안_df %>% mutate(수식언=str_match(value,'([가-힣]+)/M')[,2]) %>% na.omit()

word.n <- 황교안_df_n[,3]
word.n <- word.n$명사
word.n <- word.n %>% unlist()
word.n <- word.n %>% as.vector()
word.n <- gsub("\\^","",word.n)
word.n <- gsub("^.{1}$","",word.n)
word.n <- gsub('\\d+',"", word.n)
word.n <- word.n %>% as.list()
word.n[word.n ==""] <- NULL
word.n <- word.n %>% unlist()
word.n <- word.n %>% as.data.frame()

word.p <- 황교안_df_p[,3]
word.p <- word.p$용언
word.p <- word.p %>% unlist()
word.p <- word.p %>% as.vector()
word.p <- gsub("\\^","",word.p)
word.p <- gsub("^.{1}$","",word.p)
word.p <- gsub('\\d+',"", word.p)
word.p <- word.p %>% as.list()
word.p[word.p ==""] <- NULL
word.p <- word.p %>% unlist()
word.p <- word.p %>% as.data.frame()

word.m <- 황교안_df_m[,3]
word.m <- word.m$수식언
word.m <- word.m %>% unlist()
word.m <- word.m %>% as.vector()
word.m <- gsub("\\^","",word.m)
word.m <- gsub("^.{1}$","",word.m)
word.m <- gsub('\\d+',"", word.m)
word.m <- word.m %>% as.list()
word.m[word.m ==""] <- NULL
word.m <- word.m %>% unlist()
word.m <- word.m %>% as.data.frame()

word <- rbind(word.n, word.p, word.m)


word <- word %>% unlist()
word <- word %>% as.vector()




scores <- laply(word, function(word, word.pos, word.neg) {
  
  words <- unlist(word)  
  
  pos.matches <- match(words, word.pos)           # words의 단어를 positive에서 matching
  neg.matches <- match(words, word.neg)
  
  pos.matches <- !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
  neg.matches <- !is.na(neg.matches)
  
  score <-  sum(pos.matches) - sum(neg.matches)  # 긍정 - 부정   
}, word.pos, word.neg)

scores.df <-  data.frame(score=scores, text=word)



scores.df$color[scores.df$score >=1] = "긍정"
scores.df$color[scores.df$score ==0] = "중립"
scores.df$color[scores.df$score < 0] = "부정"

scores.df.t <- table(scores.df$color)
scores.df.t <- scores.df.t %>% as.data.frame()

황교안_결과 <- scores.df.t



####워드크라우드 - 이낙연
#### 한글 - 품사별 처리

이낙연 <- readLines("D:/이_df_word.txt") 
이낙연 <- 이낙연 %>% as.list()
이낙연[이낙연 ==""] <- NULL


이낙연 <- sample(이낙연, size = 1000, replace=FALSE)


이낙연_words <- 이낙연 %>% SimplePos09()

이낙연_words <- 이낙연_words %>%  
  melt() %>% 
  as_tibble() %>%
  select(3,1)

이낙연_명사 <- 이낙연_words %>% 
  mutate(명사=str_match(value,'([가-힣]+)/N')[,2]) %>% 
  na.omit() %>% 
  mutate(글자수=str_length(명사)) %>% 
  filter(str_length(명사)>=2) 


이낙연_용언 <- 이낙연_words %>% 
  mutate(용언=str_match(value,'([가-힣]+)/P')[,2]) %>% 
  na.omit() %>% 
  mutate(글자수=str_length(용언))%>% 
  filter(str_length(용언)>=2) 


이낙연_수식언 <- 이낙연_words %>% 
  mutate(수식언=str_match(value,'([가-힣]+)/M')[,2]) %>% 
  na.omit() %>% 
  mutate(글자수=str_length(수식언)) %>% 
  filter(str_length(수식언)>=2) 



##명사 순위

이낙연.명사 <- 이낙연_명사$명사
이낙연.명사 <- 이낙연.명사 %>% unlist()
이낙연.명사 <- 이낙연.명사 %>% as.vector()
이낙연.명사 <- str_replace_all(이낙연.명사, "\\^","")
이낙연.명사 <- str_replace_all(이낙연.명사, "^.{1}$","")
이낙연.명사 <- 이낙연.명사 %>% as.list()
이낙연.명사[이낙연.명사 ==""] <- NULL
이낙연.명사 <- 이낙연.명사 %>% unlist()
이낙연.명사 <- 이낙연.명사 %>% as.data.frame()




##용언 순위

이낙연.용언 <- 이낙연_용언$용언
이낙연.용언 <- 이낙연.용언 %>% unlist()
이낙연.용언 <- 이낙연.용언 %>% as.vector()
이낙연.용언 <- str_replace_all(이낙연.용언, "\\^","")
이낙연.용언 <- str_replace_all(이낙연.용언, "^.{1}$","")
이낙연.용언 <- str_replace_all(이낙연.용언, "\\d+","")
이낙연.용언 <- 이낙연.용언 %>% as.list()
이낙연.용언[이낙연.용언 ==""] <- NULL
이낙연.용언 <- 이낙연.용언 %>% unlist()
이낙연.용언 <- 이낙연.용언 %>% as.data.frame()




##수식언 순위

이낙연.수식언 <- 이낙연_수식언$수식언
이낙연.수식언 <- 이낙연.수식언 %>% unlist()
이낙연.수식언 <- 이낙연.수식언 %>% as.vector()
이낙연.수식언 <- str_replace_all(이낙연.수식언, "\\^","")
이낙연.수식언 <- str_replace_all(이낙연.수식언, "^.{1}$","")
이낙연.수식언 <- str_replace_all(이낙연.수식언, "\\d+","")
이낙연.수식언 <- 이낙연.수식언 %>% as.list()
이낙연.수식언[이낙연.수식언 ==""] <- NULL
이낙연.수식언 <- 이낙연.수식언 %>% unlist()
이낙연.수식언 <- 이낙연.수식언 %>% as.data.frame()



## 시각화 top 20

이낙연 <- bind_rows(이낙연.명사,이낙연.용언,이낙연.수식언)

이낙연_count <- table(이낙연)
이낙연_count <- sort(이낙연_count, decreasing = TRUE)

이낙연_count50 <- 이낙연_count[1:50]
이낙연_count50 %>% class()

## 빈도그래프 

이낙연_count50df <- 이낙연_count50 %>% as.data.frame()
이낙연_단어빈도 <- ggplot(이낙연_count50df, aes(x=이낙연, y=Freq)) + geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90, hjust=1))


#워드크라우드

이낙연_count50 <- 이낙연_count50[2:length(이낙연_count50)]
이낙연_워드크라우드 <- 이낙연_count50 %>% wordcloud2(minRotation = 0, maxRotation = 0, shape ="circle")

letterCloud()



####워드크라우드 - 황교안
#### 한글 - 품사별 처리

황교안 <- readLines("D:/황_df_word.txt") 
황교안 <- 황교안 %>% as.list()
황교안[황교안 ==""] <- NULL


황교안 <- sample(황교안, size = 20000, replace=FALSE)


황교안_words <- 황교안 %>% SimplePos09()

황교안_words <- 황교안_words %>%  
  melt() %>% 
  as_tibble() %>%
  select(3,1)

황교안_명사 <- 황교안_words %>% 
  mutate(명사=str_match(value,'([가-힣]+)/N')[,2]) %>% 
  na.omit() %>% 
  mutate(글자수=str_length(명사)) %>% 
  filter(str_length(명사)>=2) 


황교안_용언 <- 황교안_words %>% 
  mutate(용언=str_match(value,'([가-힣]+)/P')[,2]) %>% 
  na.omit() %>% 
  mutate(글자수=str_length(용언))%>% 
  filter(str_length(용언)>=2) 


황교안_수식언 <- 황교안_words %>% 
  mutate(수식언=str_match(value,'([가-힣]+)/M')[,2]) %>% 
  na.omit() %>% 
  mutate(글자수=str_length(수식언)) %>% 
  filter(str_length(수식언)>=2) 



##명사 순위

황교안.명사 <- 황교안_명사$명사
황교안.명사 <- 황교안.명사 %>% unlist()
황교안.명사 <- 황교안.명사 %>% as.vector()
황교안.명사 <- str_replace_all(황교안.명사, "\\^","")
황교안.명사 <- str_replace_all(황교안.명사, "^.{1}$","")
황교안.명사 <- 황교안.명사 %>% as.list()
황교안.명사[황교안.명사 ==""] <- NULL
황교안.명사 <- 황교안.명사 %>% unlist()
황교안.명사 <- 황교안.명사 %>% as.data.frame()




##용언 순위

황교안.용언 <- 황교안_용언$용언
황교안.용언 <- 황교안.용언 %>% unlist()
황교안.용언 <- 황교안.용언 %>% as.vector()
황교안.용언 <- str_replace_all(황교안.용언, "\\^","")
황교안.용언 <- str_replace_all(황교안.용언, "^.{1}$","")
황교안.용언 <- str_replace_all(황교안.용언, "\\d+","")
황교안.용언 <- 황교안.용언 %>% as.list()
황교안.용언[황교안.용언 ==""] <- NULL
황교안.용언 <- 황교안.용언 %>% unlist()
황교안.용언 <- 황교안.용언 %>% as.data.frame()




##수식언 순위

황교안.수식언 <- 황교안_수식언$수식언
황교안.수식언 <- 황교안.수식언 %>% unlist()
황교안.수식언 <- 황교안.수식언 %>% as.vector()
황교안.수식언 <- str_replace_all(황교안.수식언, "\\^","")
황교안.수식언 <- str_replace_all(황교안.수식언, "^.{1}$","")
황교안.수식언 <- str_replace_all(황교안.수식언, "\\d+","")
황교안.수식언 <- 황교안.수식언 %>% as.list()
황교안.수식언[황교안.수식언 ==""] <- NULL
황교안.수식언 <- 황교안.수식언 %>% unlist()
황교안.수식언 <- 황교안.수식언 %>% as.data.frame()



## 시각화 top 20

황교안 <- bind_rows(황교안.명사,황교안.용언,황교안.수식언)

황교안_count <- table(황교안)
황교안_count <- sort(황교안_count, decreasing = TRUE)

황교안_count50 <- 황교안_count[1:50]


## 빈도그래프 

황교안_count50df <- 황교안_count50 %>% as.data.frame()
황교안_단어빈도 <- ggplot(황교안_count50df, aes(x=황교안, y=Freq)) + geom_bar(stat="identity")  +
  theme(axis.text.x=element_text(angle=90, hjust=1))


#워드크라우드

황교안_count50 <- 황교안_count50[2:length(황교안_count50)]
황교안_워드크라우드 <- 황교안_count50 %>% wordcloud2()




## 연관분석-이낙연
##

이낙연 <- readLines("D:/황_df_word.txt") 
이낙연 <- 이낙연 %>% as.list()
이낙연[이낙연 ==""] <- NULL


이낙연 <- sample(이낙연, size = 5000, replace=FALSE)

이낙연_words <- 이낙연 %>% SimplePos09()

이낙연_words <- 이낙연_words %>%  
  melt() %>% 
  as_tibble() %>%
  select(3,1)


이낙연_명사 <- 이낙연_words %>% 
  mutate(명사=str_match(value,'([가-힣]+)/N')[,2]) %>% 
  na.omit() %>% 
  mutate(글자수=str_length(명사)) %>% 
  filter(str_length(명사)>=2) 


이낙연_용언 <- 이낙연_words %>% 
  mutate(용언=str_match(value,'([가-힣]+)/P')[,2]) %>% 
  na.omit() %>% 
  mutate(글자수=str_length(용언))%>% 
  filter(str_length(용언)>=2) 


이낙연_수식언 <- 이낙연_words %>% 
  mutate(수식언=str_match(value,'([가-힣]+)/M')[,2]) %>% 
  na.omit() %>% 
  mutate(글자수=str_length(수식언)) %>% 
  filter(str_length(수식언)>=2) 



##명사 순위

이낙연.명사 <- 이낙연_명사$명사
이낙연.명사 <- 이낙연.명사 %>% unlist()
이낙연.명사 <- 이낙연.명사 %>% as.vector()
이낙연.명사 <- str_replace_all(이낙연.명사, "\\^","")
이낙연.명사 <- str_replace_all(이낙연.명사, "^.{1}$","")
이낙연.명사 <- 이낙연.명사 %>% as.list()
이낙연.명사[이낙연.명사 ==""] <- NULL
이낙연.명사 <- 이낙연.명사 %>% unlist()
이낙연.명사 <- 이낙연.명사 %>% as.data.frame()




##용언 순위

이낙연.용언 <- 이낙연_용언$용언
이낙연.용언 <- 이낙연.용언 %>% unlist()
이낙연.용언 <- 이낙연.용언 %>% as.vector()
이낙연.용언 <- str_replace_all(이낙연.용언, "\\^","")
이낙연.용언 <- str_replace_all(이낙연.용언, "^.{1}$","")
이낙연.용언 <- str_replace_all(이낙연.용언, "\\d+","")
이낙연.용언 <- 이낙연.용언 %>% as.list()
이낙연.용언[이낙연.용언 ==""] <- NULL
이낙연.용언 <- 이낙연.용언 %>% unlist()
이낙연.용언 <- 이낙연.용언 %>% as.data.frame()




##수식언 순위

이낙연.수식언 <- 이낙연_수식언$수식언
이낙연.수식언 <- 이낙연.수식언 %>% unlist()
이낙연.수식언 <- 이낙연.수식언 %>% as.vector()
이낙연.수식언 <- str_replace_all(이낙연.수식언, "\\^","")
이낙연.수식언 <- str_replace_all(이낙연.수식언, "^.{1}$","")
이낙연.수식언 <- str_replace_all(이낙연.수식언, "\\d+","")
이낙연.수식언 <- 이낙연.수식언 %>% as.list()
이낙연.수식언[이낙연.수식언 ==""] <- NULL
이낙연.수식언 <- 이낙연.수식언 %>% unlist()
이낙연.수식언 <- 이낙연.수식언 %>% as.data.frame()



####

이낙연_연관 <- bind_rows(이낙연_명사,
                          이낙연_용언,
                          이낙연_수식언)



이낙연_연관_명사 <- 이낙연_연관 %>% 
  select(3, 1) %>%  
  na.omit()


이낙연_연관_명사 <- rename(이낙연_연관_명사,
                             c(단어 = 명사))


이낙연_연관_용언 <- 이낙연_연관 %>% 
  select(5, 1) %>% 
  na.omit() 


이낙연_연관_용언 <- rename(이낙연_연관_용언,
                             c(단어 = 용언))



이낙연_연관_수식언 <- 이낙연_연관 %>% 
  select(6, 1) %>% 
  na.omit()


이낙연_연관_수식언 <- rename(이낙연_연관_수식언,
                               c(단어 = 수식언))



이낙연_연관_단어 <- bind_rows(이낙연_연관_명사,
                                이낙연_연관_용언,
                                이낙연_연관_수식언)




이낙연_연관_단어sp <- split(이낙연_연관_단어,이낙연_연관_단어$L1)

이낙연_연관_단어list <- lapply(이낙연_연관_단어sp, function(x){
  return(x$단어)})

이낙연_연관_단어list %>% head()


#### 리스트 파일에서 일부만 추출

rd_n <- 1:length(이낙연_연관_단어list)
rd_n_list <- 이낙연_연관_단어list[rd_n]




#### transactions 생성

names(이낙연_연관_단어list) <- paste("Tr", 1:length(이낙연_연관_단어list), sep="")
이낙연_tran <- as(이낙연_연관_단어list, "transactions") #  중복데이터가 있으면 error발생

tran <- 이낙연_tran %>% crossTable()
View(tran)


## apriori 함수를 사용
이낙연_tran_apr <- apriori(이낙연_tran, parameter = list(supp=0.05, conf=0.05))

이낙연_tran_apr %>% summary()


#### labels 함수로 rules변수의 내용을 입력
rd_n_list_rul <- 이낙연_tran_apr %>% labels(ruleSep=" ")  

# rules변수의 내용을 리스트 구조로 변경
rd_n_list_rul <- sapply(rd_n_list_rul, strsplit, " ",USE.NAMES=F) 

# 행 단위로 묶어서 matrix로 반환
rd_n_list_mat <- do.call("rbind", rd_n_list_rul) 



#### 그래프 그리기
rd_n_list_mat_rulg <- rd_n_list_mat %>% graph.edgelist(directed=FALSE) 

rd_n_list_mat_rulg_이낙연 <- rd_n_list_mat_rulg

이낙연_연관 <- plot.igraph(rd_n_list_mat_rulg, vertex.label=V(rd_n_list_mat_rulg)$name,
                      vertex.label.cex=1.5, vertex.label.color='#000000', 
                      vertex.size=30, vertex.color='#E85100', vertex.frame.color='#E85100')





## 연관분석-황교안

##

황교안 <- readLines("D:/황_df_word.txt") 
황교안 <- 황교안 %>% as.list()
황교안[황교안 ==""] <- NULL


황교안 <- sample(황교안, size = 5000, replace=FALSE)

황교안_words <- 황교안 %>% SimplePos09()

황교안_words <- 황교안_words %>%  
  melt() %>% 
  as_tibble() %>%
  select(3,1)


황교안_명사 <- 황교안_words %>% 
  mutate(명사=str_match(value,'([가-힣]+)/N')[,2]) %>% 
  na.omit() %>% 
  mutate(글자수=str_length(명사)) %>% 
  filter(str_length(명사)>=2) 


황교안_용언 <- 황교안_words %>% 
  mutate(용언=str_match(value,'([가-힣]+)/P')[,2]) %>% 
  na.omit() %>% 
  mutate(글자수=str_length(용언))%>% 
  filter(str_length(용언)>=2) 


황교안_수식언 <- 황교안_words %>% 
  mutate(수식언=str_match(value,'([가-힣]+)/M')[,2]) %>% 
  na.omit() %>% 
  mutate(글자수=str_length(수식언)) %>% 
  filter(str_length(수식언)>=2) 



##명사 순위

황교안.명사 <- 황교안_명사$명사
황교안.명사 <- 황교안.명사 %>% unlist()
황교안.명사 <- 황교안.명사 %>% as.vector()
황교안.명사 <- str_replace_all(황교안.명사, "\\^","")
황교안.명사 <- str_replace_all(황교안.명사, "^.{1}$","")
황교안.명사 <- 황교안.명사 %>% as.list()
황교안.명사[황교안.명사 ==""] <- NULL
황교안.명사 <- 황교안.명사 %>% unlist()
황교안.명사 <- 황교안.명사 %>% as.data.frame()




##용언 순위

황교안.용언 <- 황교안_용언$용언
황교안.용언 <- 황교안.용언 %>% unlist()
황교안.용언 <- 황교안.용언 %>% as.vector()
황교안.용언 <- str_replace_all(황교안.용언, "\\^","")
황교안.용언 <- str_replace_all(황교안.용언, "^.{1}$","")
황교안.용언 <- str_replace_all(황교안.용언, "\\d+","")
황교안.용언 <- 황교안.용언 %>% as.list()
황교안.용언[황교안.용언 ==""] <- NULL
황교안.용언 <- 황교안.용언 %>% unlist()
황교안.용언 <- 황교안.용언 %>% as.data.frame()




##수식언 순위

황교안.수식언 <- 황교안_수식언$수식언
황교안.수식언 <- 황교안.수식언 %>% unlist()
황교안.수식언 <- 황교안.수식언 %>% as.vector()
황교안.수식언 <- str_replace_all(황교안.수식언, "\\^","")
황교안.수식언 <- str_replace_all(황교안.수식언, "^.{1}$","")
황교안.수식언 <- str_replace_all(황교안.수식언, "\\d+","")
황교안.수식언 <- 황교안.수식언 %>% as.list()
황교안.수식언[황교안.수식언 ==""] <- NULL
황교안.수식언 <- 황교안.수식언 %>% unlist()
황교안.수식언 <- 황교안.수식언 %>% as.data.frame()


##

황교안_연관 <- bind_rows(황교안_명사,
                          황교안_용언,
                          황교안_수식언)



황교안_연관_명사 <- 황교안_연관 %>% 
  select(3, 1) %>%  
  na.omit()


황교안_연관_명사 <- rename(황교안_연관_명사,
                             c(단어 = 명사))


황교안_연관_용언 <- 황교안_연관 %>% 
  select(5, 1) %>% 
  na.omit() 


황교안_연관_용언 <- rename(황교안_연관_용언,
                             c(단어 = 용언))



황교안_연관_수식언 <- 황교안_연관 %>% 
  select(6, 1) %>% 
  na.omit()


황교안_연관_수식언 <- rename(황교안_연관_수식언,
                               c(단어 = 수식언))



황교안_연관_단어 <- bind_rows(황교안_연관_명사,
                                황교안_연관_용언,
                                황교안_연관_수식언)




황교안_연관_단어sp <- split(황교안_연관_단어,황교안_연관_단어$L1)

황교안_연관_단어list <- lapply(황교안_연관_단어sp, function(x){
  return(x$단어)})

황교안_연관_단어list %>% head()


#### 리스트 파일에서 일부만 추출

rd_n <- 1:length(황교안_연관_단어list)
rd_n_list <- 황교안_연관_단어list[rd_n]




#### transactions 생성

names(황교안_연관_단어list) <- paste("Tr", 1:length(황교안_연관_단어list), sep="")
황교안_tran <- as(황교안_연관_단어list, "transactions") #  중복데이터가 있으면 error발생

tran <- 황교안_tran %>% crossTable()
View(tran)


## apriori 함수를 사용
황교안_tran_apr <- apriori(황교안_tran, parameter = list(supp=0.05, conf=0.05))

황교안_tran_apr %>% summary()


#### labels 함수로 rules변수의 내용을 입력
rd_n_list_rul <- 황교안_tran_apr %>% labels(ruleSep=" ")  

# rules변수의 내용을 리스트 구조로 변경
rd_n_list_rul <- sapply(rd_n_list_rul, strsplit, " ",USE.NAMES=F) 

# 행 단위로 묶어서 matrix로 반환
rd_n_list_mat <- do.call("rbind", rd_n_list_rul) 



#### 그래프 그리기
rd_n_list_mat_rulg <- rd_n_list_mat %>% graph.edgelist(directed=FALSE) 

rd_n_list_mat_rulg_황교안 <- rd_n_list_mat_rulg

황교안_연관 <- plot.igraph(rd_n_list_mat_rulg, vertex.label=V(rd_n_list_mat_rulg)$name,
                      vertex.label.cex=1.5, vertex.label.color='#000000', 
                      vertex.size=30, vertex.color='#E85100', vertex.frame.color='#E85100')



##### 결과확인

이낙연_결과
황교안_결과 

수집트윗_이낙연
수집트윗_황교안

이낙연_단어빈도
황교안_단어빈도

이낙연_워드크라우드
황교안_워드크라우드

이낙연_연관
황교안_연관

rd_n_list_mat_rulg_이낙연
rd_n_list_mat_rulg_황교안