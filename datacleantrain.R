setwd("P:/2017 Fall/5800/project")
data<-read.csv("Train.csv", header = TRUE, sep = ",")
colnames(data) <- c("id","tenure","claim","adult","children","gender","marital","premium","sales","coverage","dwelling","len","credit","color","age","year","zip","cancel")
data$cancel<- gsub(-1,NA,data$cancel, fixed = TRUE)
data1<-data[complete.cases(data), ]
data1<- unique(data1)
data1$gender<- factor(data1$gender, levels=c("M","F"), labels=c(0,1))
data1$gender<-as.numeric(data1$gender)
data1$sales<- factor(data1$sales, levels=c("Broker","Online","Phone"), labels=c(0,1,2))
data1$coverage<- factor(data1$coverage, levels=c("A","B","C"), labels=c(0,1,2))
data1$dwelling<- factor(data1$dwelling, levels=c("Condo","House","Tenant"), labels=c(0,1,2))
data1$credit<- factor(data1$credit, levels=c("low","medium","high"), labels=c(1,2,3))
data1$color<- factor(data1$color, levels=c("blue","yellow","red","white"), labels=c(0,1,2,3))
data1<-data1[!(data1$age>=100),]
data1$year<- factor(data1$year, levels=c("2013","2014","2015","2016"), labels=c(0,1,2,3))
library(tidyr)
data2<-separate(data1,zip, into = c('fzip', 'other'), sep = 2)
data2$other<- NULL
data2$zip<-NULL
data2$fzip<- factor(data2$fzip, levels=c(15,20,50,80,85,98), labels=c(0,1,2,3,4,5))
data2$id<-NULL
data2$color<-NULL
data2$year<-NULL
write.csv(data2,file='zeroclean.csv',row.names=F)