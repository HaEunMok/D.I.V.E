library(dplyr)
combat<-read.csv("train_combat.csv")
label<-read.csv("train_label.csv")
pay<-read.csv("train_payment.csv")
act<-read.csv("train_activity.csv")
label<-subset(label,select=c(1,2), survival_time!=64)

#총 더한 playtime
act1<-subset(act, select = c(2,5))
act1<- act %>%
  group_by(acc_id) %>%
  summarise(whole_playtime = sum(playtime))

#max playtime
act2 <- act %>%  
  group_by(acc_id) %>%
  summarise(playtime = max(playtime))

act3<-merge(act1,act2, by=c("acc_id"),all=FALSE)

#유저의 출석일수
A<-subset(act, select=c(1,2))
date<-A%>%unique()%>%group_by(acc_id)%>%count(acc_id)

act3<-merge(act3, date, by="acc_id",all = FALSE)
act3$mean_playtime<-act3$whole_playtime/act3$n

act4 <- act %>%  
  group_by(acc_id,char_id) %>%
  summarise(playtime = max(playtime))%>% unique()

act6<-merge(act3, act4, by=c("acc_id","playtime"),all=FALSE)%>%unique()
#######################################################################
act_1<-subset(act, select=c(2,3,4,5,11,14,15,16))
act7<-merge(act6, act_1, by=c("acc_id","char_id","playtime"),all=FALSE)%>%unique()