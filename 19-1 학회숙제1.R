library(arules)
library(arulesViz)
library(readxl)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
############데이터 하나로 만들기###########
dir<-("C:\\Users\\mivision\\Desktop\\R\\매점 매출 데이터")
list<-list.files(dir)
data<-list()
for(file in list){
  print(file)
  temp<-read_excel(paste(dir,sep="\\",file))
  data<-rbind(data, temp)
}
"""
for(i in 1:9){
  assign(paste0("df"", as.character(i+3)),read_excel(paste0("18년", as.character(i+3),"월 매출테이터.xlsx")))
}
################데이터 전처리##############
View(data)
head(data)
data$전표<-suppressWarnings(data$전표)#numeric값으로 변환
data$Date<-as.Date(data$전표)#Date
TransTime<-format(data$판매시간, "%H:%M:%S")#시간 형식 
data<-cbind(data, TransTime)
del<-substr(data$전표,11,11)#서점 데이터 삭제
data<-data[-which(del==3|del==5),]#796379
#index[!(조건문),],-로도 가능, 조건문을 만족하는 행 삭
##################연관 분석-transaction만들기###################
transactionData<- ddply(data, c("전표","Date"),
                        function(df1)paste(df1$품명,
                                           collapse=","))
#방법1 팜내시간만 잘라내는 법:format(), str했을 때POsIX형태여야 format으로 시:분:초 분리가능
#방법2: 판매시간만 잘라내는 법:substr(): 문자열을 자르는 함수 

head(transactionData)
transactionData$전표<-NULL
transactionData$Date<-NULL

colnames(transactionData)<-c("items")
View(transactionData)

write.csv(transactionData, "market_basket_transaction.csv",quote=FALSE, row.names = TRUE)
tr<- read.transactions("market_basket_transaction.csv", format='basket', sep=",",rm.duplicates = TRUE)
#################연관분석-inspect하기#########################
library(RColorBrewer)
itemFrequencyPlot(tr, topN=20, type="absolute", col=brewer.pal(8, 'Pastel2'), main="Absolute Item Frequency Plot")

buyer_rules<-NULL

buyer_rules<-apriori(tr,parameter=list(supp=0.0003,conf=0.0005,maxlen=10))
#연관규칙은 규칙이 적게 나오는 것이 좋음


inspect(buyer_rules)

View(inspect(buyer_rules))

inspect(sort(buyer_rules,by="lift")[1:5])

inspect(sort(buyer_rules,by="lift",decreasing=F)[1:5])

View(inspect(sort(buyer_rules[687:805],by="count")[1:30]))

plot(sort(buyer_rules[687:805],by="count")[1:50], method="graph", control=list(type="items") )
