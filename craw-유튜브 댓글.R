install.packages("httr")
install.packages("jsonlite")
install.packages("rJava")
install.packages("rvest")
install.packages("RSelenium")
install.packages("XML")

library(dplyr)
library(httr)
library(jsonlite)
library(rJava)
library(stringr)
library(rvest)
library(RSelenium)
library(tidyverse)
library(XML)

Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre-1.8')

##cmd 관리자권한으로 실행
# cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

# https://googlechromelabs.github.io/chrome-for-testing/#stable chromedriver downloads
# java -Dwebdriver.chrome.driver="C:\r_selenium\chromedriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445


# RSelenium
remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

remDr$navigate("https://www.youtube.com/watch?v=zTJa_SwHcTE")

Sys.sleep(time = 1)

pause <- remDr$findElement("css","button.ytp-play-button") 
pause$clickElement()

# 페이지 스크롤
# remDr$executeScript("window.scrollTo(0,500)")

webElem <- remDr$findElement("css", "body")

for(i in 1:10000){
  i <- i+1
webElem$sendKeysToElement(list(key = "end"))
}
  
## 현재페이지 정보 읽어오기
frontpage <- remDr$getPageSource()[[1]]
body <- frontpage %>% read_html() 

Sys.sleep(time = 1)

## 댓글
댓글_ytb <- body %>% 
  html_nodes("yt-formatted-string#content-text") %>%
  html_text()  

