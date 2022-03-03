library(lubridate)
trade<-read.csv("train_trade.csv")
#시간 뽑기
trade$hour<-as.character(trade$time,'%Y')
trade$hour<-substr(trade$time,1,2)
plot(trade$hour)
#hour, target_acc_id 뽑아내기
trade_df<-subset(trade, select=c(hour, target_acc_id))
library(dplyr) 

count<-trade_df%>%unique()%>%group_by(hour)%>%count(hour)
View(count)
View(trade_df)
library(ggplot2)
plot(count$hour,count$n)
trade_df$acc_id<-trade_df$target_acc_id
trade_df$target_acc_id<-NULL

combat<-read.csv("train_combat.csv") 
combat<-subset(combat, select=c(pledge_cnt,random_attacker_cnt,same_pledge_cnt,acc_id))
ct_df<-merge(trade_df, combat, by="acc_id",all=FALSE)
activity<-read.csv("train_activity.csv") 
act<-subset(activity, select=c(acc_id, npc_kill))
act_df<-merge(ct_df, act, by="acc_id",all=FALSE)
