install.packages('tidyverse')
install.packages('rvest')
library(tidyverse)
library(rvest)

base_url='https://search.naver.com/search.naver?where=news&query=%EC%84%9C%EA%B0%95%EB%8C%80%ED%95%99%EA%B5%90%20%EC%9D%B4%EC%83%81%EA%B7%BC&sm=tab_srt&sort=1&photo=0&field=0&reporter_article=&pd=0&ds=&de=&docid=&nso=so%3Add%2Cp%3Aall%2Ca%3Aall&mynews=0&refresh_start=0&related=0'
use_url='&refresh_start=0'

urls=NULL
for (x in 0:9) {
  urls = c(urls, paste(base_url, x*10+1, use_url, sep=''))
}
urls

html=read_html(urls[1])
html1=html_nodes(html, 'ul.list_news > li > div > div > div > div > a')
html_href=html_attr(html1, 'href')
html_href
href_naver=html_attr[grep("news.naver.com", html_attr)]
href_naver

read_html(news_links_naver[1])%>%
  html_nodes('td>div>div>div>h3#articleTitle')%>%
  html_text()

read_html(news_links_naver[1])%>%
  html_nodes('div>div>div#articleBodyContents')%>%
  html_text()

urls[1] %>% read_html() %>%
  html_nodes('ul.list_news > li > div > div > div > div > a') %>%
  html_attr('href')


news_links <- NULL
for (url in urls) {
  html = read_html(url)
  news_links=c(news_links, html %>%
                 html_nodes('ul.list_news > li > div > div > div > div > a') %>%
                 html_attr('href'))
  news_links_naver=news_links[grep('news.naver.com', news_links)]
}

contents=NULL
title=NULL
date=NULL
date1=NULL
for (link in news_links_naver) {
  html=read_html(link)
  contents=c(contents, html %>% html_nodes('div#articleBodyContents') %>%
               html_text())
  title = c(title, html %>% html_nodes('h3#articleTitle') %>%
              html_text())
}

contents1 = data.frame(contents)
title1 = data.frame(title)
naver_news=cbind(title1, contents1)
view(naver_news)

read_html(news_links_naver[1]) %>% 
  html_nodes('div > div > div#articleBodyContents') %>%
  html_text()


# Package Load
library(tidyverse)
library(reshape2)
rm_flash <- 'flash 오류를 우회하기 위한 함수 추가\nfunction _flash_removeCallback()'
rm_video <- '동영상 뉴스'
body = naver_news$contents %>% str_remove(rm_flash) %>% str_remove(rm_video)
write.table(body, 'contents.txt', row.names=F)
doc = readLines('contents.txt')

install.packages('rJava')
install.packages('memoise')
install.packages('devtools')
install.packages('tibble')
install.packages('wordcloud2')
devtools::install_github('haven-jeon/KoNLP')

library(rJava)
library(memoise)
library(devtools)
library(tibble)
library(KoNLP)
library(KoNLP)
library(wordcloud2)

word_sp=SimplePos09(doc)
view(word_sp)
word_spm=word_sp %>% 
  melt %>%
  as_tibble

word_spm %>% 
  select(3,1) %>%
  mutate(noun=str_match(value, '([가-힣]+|[a-zA-Z]+)/N')[,2]) %>%
  na.omit(noun) %>%
  count(noun, sort=T) %>%
  filter(str_length(noun)>=2) %>%
  filter(noun !='삼성전자', noun !='억원', noun !='지난해', noun !='분기') %>%
  filter(noun !='전년', noun !='확대', noun !='대비', noun !='조원', noun !='무단전제' , noun !='영업이익') %>%
  filter(n>=n[100]) %>%
  wordcloud2()
