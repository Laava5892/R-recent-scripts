setwd("P:/2017 Fall/5800/project")
data1<-read.csv("Test.csv", header = TRUE, sep = ",")
colnames(data1) <- c("id","tenure","claim","adult","children","gender","marital","premium","sales","coverage","dwelling","len","credit","color","age","year","zip")
data1$gender<- factor(data1$gender, levels=c("M","F"), labels=c(0,1))
data1$gender<-as.numeric(data1$gender)
data1$sales<- factor(data1$sales, levels=c("Broker","Online","Phone"), labels=c(0,1,2))
data1$coverage<- factor(data1$coverage, levels=c("A","B","C"), labels=c(0,1,2))
data1$dwelling<- factor(data1$dwelling, levels=c("Condo","House","Tenant","Landlord"), labels=c(0,1,2,3))
data1$credit<- factor(data1$credit, levels=c("low","medium","high"), labels=c(1,2,3))
data1$color<- factor(data1$color, levels=c("blue","yellow","red","white"), labels=c(0,1,2,3))
library(dplyr)
data1$age[data1$age>100]=NA
data1$dwelling[data1$dwelling==3]=NA
data1$year<- factor(data1$year, levels=c("2017"), labels=c(5))
library(tidyr)
data2<-separate(data1,zip, into = c('fzip', 'other'), sep = 2)
data2$other<- NULL
data2$zip<-NULL
data2$fzip<- factor(data2$fzip, levels=c(15,20,50,80,85,98), labels=c(0,1,2,3,4,5))
data2$id<-NULL
data2$color<-NULL
data2$year<-NULL
library(mice)
data3<-mice(data2,m=5,maxit=10,meth='pmm')
miceOutput <- complete(data3)  
anyNA(miceOutput)

write.csv(miceOutput,file='zerotest.csv',row.names=F)
