library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)

# 데이터 수집
head <- "https://www.pgr21.com/spoent/" 

링크_pgr <- c()

for (i in 59990:60000){
  링크_pgr.tmp <- paste0(head,i)
  링크_pgr <- append(링크_pgr,링크_pgr.tmp)
}

##cmd
#cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445
    
# RSelenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                          port=4445L, 
                          browserName="chrome")
remDr$open()
    
remDr$navigate("https://www.pgr21.com/spoent/69490")
body <- remDr$getPageSource()[[1]]
    
body <- body %>% read_html()

# depth0
id_num0 <- body %>% 
  html_nodes("div.aReply") %>% 
  html_nodes("a") %>% 
  html_attr("name") %>% 
  na.omit()

div_c <- paste0("div#","comment_",id_num0)

depth0 <- c()
id_depth0 <- c()

for (i in 1:length(id_num0)){
  depth0.tmp <- body %>% 
    html_nodes(div_c[i]) %>%
    html_nodes("div.aReply") %>% 
    html_nodes("span.cmtName") %>% 
    html_text() %>% 
    str_remove_all("\n") %>% 
    str_remove_all("\t") %>% 
    str_trim()
  
    depth0 <- append(depth0, depth0.tmp)
    id_depth0 <- append(id_depth0, div_c[i])
}

id_depth0 <- gsub("div#comment_","",id_depth0)

depth0_df <- tibble(id_depth0, depth0)
names(depth0_df) <- c("id", "depth0")


# depth1
div_cc <- paste0("div#","commentContainer_",id_num0,".depth1")

depth1 <- c()
id_depth1 <- c()

for (i in 1:length(id_num0)){
  depth1.tmp <- body %>% 
    html_nodes(div_cc[i]) %>%
    html_nodes("div.aReply") %>% 
    html_nodes("span.cmtName") %>% 
    html_text() %>% 
    str_remove_all("\n") %>% 
    str_remove_all("\t") %>% 
    str_trim()
  
  if (length(depth1.tmp) != 0) {
    depth1 <- append(depth1, depth1.tmp)
    id_depth1 <- append(id_depth1, rep(div_cc[i],length(depth1.tmp)))
  } else {
    depth1 <- append(depth1, NA)
    id_depth1 <- append(id_depth1, NA)
  }    
}  

id_depth1 <- gsub("div#commentContainer_","",id_depth1)
id_depth1 <- gsub(".depth1","",id_depth1)

depth1 <- depth1 %>% na.omit() 
id_depth1 <- id_depth1 %>% na.omit() 

depth1_df <- tibble(id_depth1, depth1)
names(depth1_df) <- c("id", "depth1")


#  이분그래프 df
df <- depth0_df %>% 
     left_join(depth1_df, by = "id") %>%
    select(id, depth1) %>% 
     na.omit() %>% 
  graph_from_data_frame(directed = FALSE)
# 
# dep0 <- append(df$depth0, df$depth1)
# dep0 <- dep0 %>% unique() %>% as_tibble()
# dep0$id0 <- c(1:length(dep0$value))
# names(dep0) <- c("depth0","value0")
# 
# dep1 <- dep0
# names(dep1) <- c("depth1","value1")
# 
# df_numid <- df %>%
#   select("depth0", "depth1") %>%
#   left_join(dep0, by = "depth0") %>%
#   left_join(dep1, by = "depth1") %>%
#         select("value0","value1") %>%
#         graph_from_data_frame(directed = FALSE)

df %>% get.incidence()

V(df)$type <- bipartite_mapping(df)$type 
## bipartite_mapping( )함수는 양자간(TRUE / FALSE)로 구성하는 그래프를 그려주는 함수입니다 
# 사용자 이분그래프모형을 이용한 온라인 커뮤니티 토론 네트워크의 군집성과 극성 분석 - 논문참조

V(df)$color <- V(df)$type
V(df)$color <- gsub("FALSE","red",V(df)$color)
V(df)$color <- gsub("TRUE","blue",V(df)$color)
plot(df, edge.color="gray30",edge.width=E(df)$weight, layout=layout_as_bipartite, )
