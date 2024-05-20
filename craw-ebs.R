library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)
library(tidyverse)
library(openxlsx)

Sys.setenv("http_proxy"="")
Sys.setenv("no_proxy"=TRUE)
Sys.setenv("no_proxy"=1)

# 1 R 상단 메뉴에서 Tools Global Options-Packages를 클릭한다.
# 2 Use Internet Explorer library/proxy for HTTP 체크를 해제한다.
# 3 .Renviron을 컴퓨터에서 찾아야 하는데 찾을수 없었고 file.edit('~/.Renviron') 명령을 입력해 설정해주었다.(보통 내문서에 존재.)
# 4 여기에다가 다음 코드를 입력하면 정상적으로 작동하게 된다.
# options(internet.info = 0)
# http_proxy="http://user_id:password@your_proxy:your_port"

##cmd 관리자권한으로 실행
# cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445
start_time1 <- Sys.time()

remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

remDr$navigate("https://www.ebsi.co.kr/ebs/xip/xipc/previousPaperList.ebs?targetCd=D300")

# 로그인 먼저하기
# login <- remDr$findElement(using = "css", "li.user_login")
# login$clickElement()
# Sys.sleep(time = 1)
# 
# login <- remDr$findElement(using = "css", "a.naver")
# login$clickElement()
# Sys.sleep(time = 1)
# 
# login <- remDr$findElement(using = "css", "input.input_text")
# login$clickElement()
# login$sendKeysToElement(list("1125mirhan"))
# Sys.sleep(time = 1)
# 
# login <- remDr$findElement(using = "xpath", "//*[@id='pw']")
# login$clickElement()
# login$sendKeysToElement(list("zl5slglf"))
# Sys.sleep(time = 1)
# 
# login <- remDr$findElement(using = "css", "button.btn_login")
# login$clickElement()


# 06-24
year <- remDr$findElement(using = "xpath", "//*[@id='beginYear']/option[19]") 
year$clickElement()
Sys.sleep(time = 1)

# 영어
eng <- remDr$findElement(using = "xpath", "//*[@id='subj3']") 
eng$clickElement()
Sys.sleep(time = 1)

# 검색
검색 <- remDr$findElement(using = "xpath", "//*[@id='paperListFrm']/div[2]/div[2]/button") 
검색$clickElement()
Sys.sleep(time = 1)

# 50개씩 보기
x50개 <- remDr$findElement(using = "xpath", "//*[@id='pagingForm']/div[1]/div[2]/select/option[3]") 
x50개$clickElement()
Sys.sleep(time = 1)

# 문제
Sys.sleep(time = 1)

frontpage <- remDr$getPageSource()[[1]]
body <- frontpage %>% read_html() 

end_length <- body %>% 
  html_nodes("div.count") %>% 
  html_nodes("a:nth-child(2)") %>% 
  html_text() %>% 
  str_trim()

start_time <- Sys.time()

for (k in 1:end_length) {
frontpage <- remDr$getPageSource()[[1]]
body <- frontpage %>% read_html() 

# 초기 빈 벡터를 준비합니다
xpaths <- c()

# i는 1부터 3까지, j는 1부터 50까지 반복합니다
for (i in 1:5) {
  for (j in 1:50) {
    # XPath를 형식에 맞게 생성하고 벡터에 추가합니다
    xpath <- sprintf('//*[@id="pagingForm"]/div[2]/ul/li[%d]/div[3]/div[1]/button[%d]', j, i)
    xpaths <- c(xpaths, xpath)
  }
}


# 제목
제목 <- (body %>% 
      html_nodes("p.tit") %>% 
      html_text())[-1]

제목 <- 제목 %>% 
  str_remove_all("\n") %>% 
  str_remove_all("\t")

# 각 요소를 3번씩 반복하는 함수
repeat_elements <- function(vec, times) {
  return(unlist(lapply(vec, function(x) rep(x, times))))
}

# 각 요소를 3번씩 반복하여 결과 생성
제목 <- rep(제목, 5)
제목 <- 제목 %>% str_trim()
pdf_df <- tibble(제목, xpaths)


# 
onclick_df <- tibble()

for (i in 1:length(pdf_df$xpaths)) {
onclick.tmp <- body %>% 
  html_nodes(xpath = pdf_df$xpaths[i]) %>% 
  html_attr("onclick")

onclick_df.tmp <- tibble(제목 = pdf_df$제목[i], onclick = onclick.tmp)
onclick_df <- bind_rows(onclick_df.tmp , onclick_df)
}

onclick_df <- onclick_df %>%
  filter(grepl(".pdf", onclick))


# "mun", "scr", "hsj"가 포함된 행을 필터링
hsj_df <- onclick_df %>%
  filter(grepl("hsj", onclick))
hsj_df$제목 <- paste0("hsj_",hsj_df$제목)

scr_df <- onclick_df %>%
  filter(grepl("scr", onclick))
scr_df$제목 <- paste0("scr_",scr_df$제목)

mum_df <- onclick_df %>%
  filter(!grepl("hsj|scr", onclick))
mum_df$제목 <- paste0("mum_",mum_df$제목)

onclick_df <- bind_rows(hsj_df, scr_df, mum_df)


# 정규 표현식을 사용하여 링크 부분 추출
base_url <- "https://wdown.ebsi.co.kr/W61001/01exam"

# 링크 추출 함수 정의
extract_link <- function(onclick) {
  pattern <- "/[^\']*\\.pdf"
  match <- regmatches(onclick, regexpr(pattern, onclick))
  return(paste0(base_url, match))
}

# 각 문자열에서 링크를 추출하여 전체 URL 생성
onclick_df$url <- lapply(onclick_df$onclick, extract_link) %>% unlist()

# pdf 다운
# 직접 다운로드 URL 설정
downloads <- paste0("D:/대학원/논문/Double Negation/rawdata/", onclick_df$제목,".pdf")

# 파일 다운로드 시도
for(i in 1:length(downloads)){
response <- GET(onclick_df$url[i], write_disk(downloads[i], overwrite = TRUE))

# 다운로드 결과 확인
if (response$status_code == 200) {
  print("PDF 파일이 성공적으로 다운로드되었습니다.")
} else {
  print(paste("다운로드 중 오류 발생: 상태 코드", response$status_code))
  Sys.sleep(time = 1)
  
}
}

btn_next <- remDr$findElement(using = "css", "a.btn_next")
btn_next$clickElement()

k <- k + 1

}

end_time <- Sys.time()

end_time - start_time
