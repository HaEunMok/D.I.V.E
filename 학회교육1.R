install.packages("arules")#연관도출을 시켜주는 패키지
library(arules)
install.packages("arulesViz")
library(arulesViz)
install.packages("readxl")#엑셀을 읽어주는 패키지
library(readxl)
install.packages("tidyverse")#아래 패키지가 다 들어있는 패키지
library(tidyverse)
install.packages("ggplot2")#시각화
library(ggplot2)
install.packages("lubridate")#시간데이터
library(lubridate)
install.packages("plyr")#데이터 프레임 전처리 
library(plyr)
install.packages("dplyr")
library(dplyr)

retail<-read_excel('Online_Retail.xlsx')
View(retail)
sum(is.na(retail))
retail<-retail[complete.cases(retail),]#na값을 없앤것만 행으로 출력을 함
#########NA값 전처리################
str(retail)
retail$InvoiceNo<- as.numeric(retail$InvoiceDate)#ch값을 num으로 바꾸어줌
retail$Date<-as.Date(retail$InvoiceNo)#열추가
TransTime<-format(retail$InvoiceDate, "%H:%M:%S")#format함수: 뒤의 형식으로 데이터를 바꾸어 주겠다. 
retail<- cbind(retail, TransTime)
#############연월전처리###############
transactionData<- ddply(retail, c("InvoiceNo","Date"),
                        function(df1)paste(df1$Description,
                                           collapse=","))
head(transactionData)
transactionData$InvoiceNo<-NULL
transactionData$Date<-NULL

colnames(transactionData)<-c("items")

View(transactionData)

write.csv(transactionData, "market_basket_transaction.csv",quote=FALSE, row.names = TRUE)
tr<- read.transactions("market_basket_transaction.csv", format='basket', sep=",",rm.duplicates = TRUE)
library(RColorBrewer)
itemFrequencyPlot(tr, topN=20, type="absolute", col=brewer.pal(8, 'Pastel2'), main="Absolute Item Frequency Plot")

buyer_rules<- apriori(tr, parameter=list(supp=0.001, conf=0.8, maxlen=10))

"""
좋은 규칙을 찾거나 불필요한 규칙들을 지우기 위해 parameter들을 조절(노가다가 필요)

"""
inspect(buyer_rules[1:20])

inspect(sort(buyer_rules, by="lift")[1:5])
inspect(sort(buyer_rules, by="lift", decreasing = FALSE)[1:5])

rule_subset<-subset(buyer_rules, items%in%c("WRAP","WHITE TEA"))
inspect(rule_subset[1:5])

#supp, conf의 값을 바꾸어 보면서 결과를 도출해보기 
