

combat<-read.csv("train_combat.csv")
label<-read.csv("train_label.csv")
pay<-read.csv("train_payment.csv")
act<-read.csv("train_activity.csv")

pay<-subset(pay, select=c(2,3))
lab<-subset(label, select=c(1,2))
data<-subset(combat,select=c(2,3,6,9))
act<-subset(act, select = c(1,2,4,5,11,14,15))

data<-merge(lab, data, by="acc_id",all=FALSE)
#data<-subset(data, data$survival_time!=64)

library(ggplot2)
plot(data$level,data$random_defender_cnt)

data$level2<-cut(data$level,breaks=c(-Inf, 10, 14, Inf),labels=c("A","B","C"))
data<-merge(data, pay, by="acc_id",all=FALSE)
View(data)


low<-subset(data, level2=="A")
mid<-subset(data, level2=="B")
high<-subset(data, level2=="C")

plot(mid$survival_time)
plot(data$level2, data$survival_time,mean)




plot(data$survival_time,data$amount_spent)
plot(low$playtime,low$survival_time)