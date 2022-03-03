### Week10 Assignment

###1. 날씨 크롤링 

#install.packages("rvest")
library(rvest) #데이터 크롤링을 하기 위한 함수들이 들어있는 패키지 
library(dplyr) #데이터프레임의 전처리를 하는데 유용한 함수가 들어있는 패키지 #여기선 %>%함수 사용 
library(lubridate)

basic_url<-"https://weather.naver.com/period/pastWetrMain.nhn?naverRgnCd=04113&ym="

urls<-NULL
for(x in 0:8){
  urls[x+1]<-paste0(basic_url,as.character(201804+x))
}
for(x in 0:2){
  urls[x+10]<-paste0(basic_url,as.character(201901+x))
}
urls
first_url<-urls[1]
#read_html(): rvest 패키지, url 첫번째 페이지에 있는 모든 html(정보)를 읽어옴
html<-read_html(first_url)
html

#html_node() : 매칭되는 첫번째 요소를 가져옴, id 반환할 때 사용 
#html_nodes() : 모든 요소를 반환, tag, class 반환할 때 사용 
#.= : class를 뜻함(생략도 가능) -> 모든 class를 가지고 오겠다 
#html_attr() : 하이퍼링크(파란색 글씨)를 가져오는 함수, href, src를 반환할 때 사용(하이퍼링크와 자주 사용되는 class들)
#unique() : 겹치는 값을 없애주는 함수 #그냥 쓰는게 좋아 
html %>% html_nodes('tbody') %>% html_nodes('tr') %>% html_nodes('td') %>% html_nodes('strong') %>% html_text() %>% unique()

html %>% html_nodes('h4.st_cal') %>% html_nodes('strong') %>% html_text()
html %>% html_nodes('tbody') %>% html_nodes('tr') %>% html_nodes('p.text') %>% html_nodes('span.temp_mn') %>% html_text()

weather <- NULL
year<-NULL
month<-NULL
day<-NULL
max<-NULL
min<-NULL
weather<-NULL
yearnmonth<-NULL
dday<-NULL
Day<-NULL
for(url in urls){
  html<-read_html(url)
  yearnmonth<-html %>% html_nodes('h4.st_cal') %>% html_text() %>% unique()
  year<-substr(yearnmonth,13,16)
  month<-substr(yearnmonth,19,20)
  month<-gsub("월","",month)
  month<-gsub(" ","",month)
  if(as.numeric(month)<10){
    month<-paste0('0',month)
  } 
  day<-c(day,html %>% html_nodes('tbody') %>% html_nodes('tr') %>% html_nodes('td') %>% html_text() %>% unique())
  day<-day[-which(day=="")]
  day<-gsub("\r"," ",day)
  day<-gsub("\n"," ",day)
  day<-gsub("\t"," ",day)
  dday<-substr(day,7,8)
  for(i in 1:length(dday)){
    if(as.numeric(dday[i])<10){
      dday[i]<-paste0('0',dday[i])
    }}
  for(i in 1:6){
    for(j in 1:7){
      weather <- c(weather, html %>% html_nodes(xpath=paste0('//*[@id="print_content"]/table[1]/tbody/tr[', as.character(i), ']/td[', as.character(j), ']/p[1]/img')) %>% html_attr('alt'))
    }
  }
  for(i in 1:length(dday)){
    Day<-c(Day,paste0(year,"-",month,"-",dday[i])) 
  }
  dday<-NULL
  day<-NULL
  min<-c(min,html %>% html_nodes('tbody') %>% html_nodes('tr') %>% html_nodes('p.text') %>% html_nodes('span.temp_mn') %>% html_text())
  max<-c(max,html %>% html_nodes('tbody') %>% html_nodes('tr') %>% html_nodes('p.text') %>% html_nodes('span.temp_mx') %>% html_text())
}
#날짜 만들기 
yearnmonth
Day
Day<-as.Date(Day)
Week<-week(Day)
Weekdata<-data.frame(Day, Week)
View(Weekdata)
class(Weekdata$Week)
Weekdata$num<-c(1:365)
Weekdata[,2]<-as.numeric(Weekdata[,2])
###########주차#############
Weekdata[275,2]<-1
for(i in 276:365){
  if(Weekdata[i,3]%%7==2)Weekdata[i,2]<-Weekdata[i,2]+1
}
######2018-1#########
for(i in 1:78){
  Weekdata[i,2]<-Weekdata[i,2]-8
}
Weekdata2<-Weekdata
######2018-2######여기 다시 
for(i in 142:260){
  Weekdata[i,2]<-Weekdata[i,2]-34
}
######2019-1######여기 다시 
for(i in 324:365){
  Weekdata[i,2]<-Weekdata[i,2]-8
}
######여름방학######
for(i in 79:141){
  Weekdata[i,2]<-"여름방학"
}
######겨울방학#####
for(i in 261:323){
  Weekdata[i,2]<-"겨울방학"
}
Weekdata$Week<-paste(Weekdata$Week,"주차")
write.csv(weather, "weather.csv")
write.csv(Weekdata, "week.csv")
