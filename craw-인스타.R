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

##cmd 관리자권한으로 실행
# cd C:\r_selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445

remDr <- remoteDriver(remoteServerAddr="localhost", 
                      port=4445L, 
                      browserName="chrome")
remDr$open()

# 로그인 및 해시태그 검색:
remDr$navigate("https://www.instagram.com/accounts/login/")
# gmail.com / zl5tmslglf

# 로그인 정보 입력
remDr$findElement(using = "name", value = "username")$sendKeysToElement(list("hanmir1125@gmail.com"))
remDr$findElement(using = "name", value = "password")$sendKeysToElement(list("zl5tmslglf", key = "enter"))
Sys.sleep(5)

# 나중에 하기 누르기
try({
나중1 <- remDr$findElement(using = "xpath", "//*[@id='mount_0_0_VD']/div/div/div[2]/div/div/div[1]/div[1]/div[2]/section/main/div/div/div/div/div") 
나중1$clickElement()
}, silent = TRUE)

try({
  나중2 <- remDr$findElement(using = "xpath", "/html/body/div[4]/div[1]/div/div[2]/div/div/div/div/div[2]/div/div/div[3]/button[2]") 
  나중2$clickElement()
}, silent = TRUE)
Sys.sleep(5)

# 특정 해시태그 검색
remDr$navigate("https://www.instagram.com/explore/tags/summer/")
Sys.sleep(5)

# 게시글 수집:
posts <- remDr$findElements(using = "css selector", value = "article div div div div a")

post_links <- sapply(posts, function(post) {
  post$getElementAttribute("href")
})



