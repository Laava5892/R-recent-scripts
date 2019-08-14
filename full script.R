setwd("~/Desktop/FINAL_REVIEW/")
# raw_data <- read.csv("clean_data.csv") load raw data
# run data cleaning script and 

data<-read.csv("clean_data.csv") 


# Spend Rate difference plot

data_pre_spend <- subset(data, IDENTIFIER=="Pre")
data_post_spend <- subset(data, IDENTIFIER=="Post")

library(dplyr)

d1 <- dplyr::group_by(data_pre_spend, PACKAGE_ID, IDENTIFIER=="Pre")

d2 <- dplyr::summarise(d1, sum_net_spend_pre = sum(TOTAL_NET_SPEND, na.rm = TRUE),sum_pkg_budget_pre = sum(PACKAGE_BUDGET,na.rm = TRUE), sum_rev=sum(AMO_REV,na.rm=TRUE))

d2 <- d2[,-2]

d2 <- dplyr::mutate(d2, sr_pre=sum_net_spend_pre/sum_pkg_budget_pre)

d3 <- dplyr::group_by(data_post_spend, PACKAGE_ID, IDENTIFIER=="Post")

d4 <- dplyr::summarise(d3, sum_net_spend_post = sum(TOTAL_NET_SPEND,na.rm = TRUE),sum_pkg_budget_post = sum(PACKAGE_BUDGET,na.rm = TRUE), sum_post_rev=sum(AMO_REV,na.rm=TRUE))
d4 <- d4[,-2]
d4 <- mutate(d4, sr_post = sum_net_spend_post/sum_pkg_budget_post)

data_comparison_sr <- merge (d2, d4, by="PACKAGE_ID")

data_comparison_sr$diff <- (data_comparison_sr$sr_post) - (data_comparison_sr$sr_pre)

library(ggplot2)
data_comparison_sr$PACKAGE_ID <- as.factor(data_comparison_sr$PACKAGE_ID)
ggplot(data_comparison_sr, aes(x=PACKAGE_ID, y=diff)) + geom_point()

data_comparison_sr$comparison <- paste(data_comparison_sr$sr_post, data_comparison_sr$sr_pre, sep=",")


#Ben's Table daily unpaired


names(data)
data <- mutate(data, DAILY_SPEND = TOTAL_NET_SPEND/PACKAGE_BUDGET)

data <- mutate(data, BUDGET_CHANGE = ((MAX_BUDGET-MIN_BUDGET)/MAX_BUDGET))

data_summary_grp <- dplyr::group_by(data, PACKAGE_ID, IDENTIFIER)
data_summary <- dplyr::summarise(data_summary_grp, sum_net_spend = sum(TOTAL_NET_SPEND,na.rm = TRUE),sum_imp = sum(IMPRESSIONS,na.rm=TRUE), sum_pkg_budget = sum(PACKAGE_BUDGET,na.rm = TRUE),avg_spend=mean(TOTAL_NET_SPEND,na.rm=TRUE),avg_imp = mean(IMPRESSIONS,na.rm=TRUE),sum_rev = sum(AMO_REV, na.rm = TRUE),avg_rev = mean(AMO_REV, na.rm = TRUE), avg_pkg_budget = mean(PACKAGE_BUDGET,na.rm = TRUE))


data_sub <- subset(data, data$PACKAGE_ID!= "196587" & data$PACKAGE_ID!= "196844")

quantile(data_sub$BUDGET_CHANGE,0.9,na.rm = TRUE)
# 90% 
# 0.212148 

data_sub_1 <- subset(data_sub, data_sub$BUDGET_CHANGE <= 0.21)

data_pre <- subset(data_sub_1, data_sub_1$IDENTIFIER=="Pre")
data_post <- subset(data_sub_1, data_sub_1$IDENTIFIER=="Post")

t.test(data_post$DAILY_SPEND, data_pre$DAILY_SPEND, paired=FALSE)

# Welch Two Sample t-test
# 
# data:  data_pre$DAILY_SPEND and data_post$DAILY_SPEND
# t = 0.36042, df = 285.72, p-value = 0.7188
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.07787669  0.11278964
# sample estimates:
#   mean of x mean of y
# 0.8056025 0.7881461


data_pre_1 <- subset(data_sub, data_sub$IDENTIFIER=="Pre")
data_post_1 <- subset(data_sub, data_sub$IDENTIFIER=="Post")

t.test(data_post_1$TOTAL_NET_SPEND, data_pre_1$TOTAL_NET_SPEND, paired=FALSE)

# Welch Two Sample t-test
# 
# data:  data_post_1$TOTAL_NET_SPEND and data_pre_1$TOTAL_NET_SPEND
# t = -1.5538, df = 373.17, p-value = 0.1211
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -635.01054   74.41207
# sample estimates:
#   mean of x mean of y 
# 908.4256 1188.7249

t.test(data_post_1$IMPRESSIONS, data_pre_1$IMPRESSIONS, paired=FALSE)
# Welch Two Sample t-test
# 
# data:  data_post_1$IMPRESSIONS and data_pre_1$IMPRESSIONS
# t = -2.0682, df = 268.7, p-value = 0.03958
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -275705.496   -6783.213
# sample estimates:
#   mean of x mean of y 
# 184426.3  325670.7 


t.test(data_post_1$AMO_REV, data_pre_1$AMO_REV, paired=FALSE)

# Welch Two Sample t-test
# 
# data:  data_post_1$AMO_REV and data_pre_1$AMO_REV
# t = 2.3459, df = 237.66, p-value = 0.01981
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   53.86257 618.46720
# sample estimates:
#   mean of x mean of y 
# 566.0673  229.9024 



data_sub$cpmn<-as.numeric(data_sub$CPM)
data_sub$nets<-as.numeric(data_sub$TOTAL_NET_SPEND)
data_sub$impn<-as.numeric(data_sub$IMPRESSIONS)

data_clean2 <- subset(data_sub, data_sub$IDENTIFIER=="Pre")

sumdata_clean2<-dplyr::group_by(data_clean2,PACKAGE_ID,IDENTIFIER=='Pre')
sumspend_clean2<-dplyr::summarise(sumdata_clean2,
                           mean_cpm = mean(cpmn, na.rm=TRUE), sum_cost=sum(nets,na.rm = TRUE), sum_imp=sum(impn,na.rm = TRUE))

data_clean3 <- merge(data_sub, sumspend_clean2, by="PACKAGE_ID")
data_clean3 <- data_clean3[, -33]

data_clean3<-mutate(data_clean3,
                    avgcpm=cpmn/mean_cpm, avgcpm_norm_1=sum_cost/sum_imp*1000)
data_clean3<-mutate(data_clean3,
                    avgcpm_2=cpmn/avgcpm_norm_1)

cpmpre_data_clean <- subset(data_clean3,data_clean3$IDENTIFIER=="Pre")
cpmpost_data_clean <- subset(data_clean3,data_clean3$IDENTIFIER=="Post")
t.test(cpmpost_data_clean$avgcpm_2, cpmpre_data_clean$avgcpm_2, paired=FALSE)

# Welch Two Sample t-test
# 
# data:  cpmpost_data_clean$avgcpm_2 and cpmpre_data_clean$avgcpm_2
# t = -3.7965, df = 268.26, p-value = 0.0001815
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.17843212 -0.05656344
# sample estimates:
#   mean of x mean of y 
# 0.8952306 1.0127284 

library(plyr)
cdat <- plyr::ddply(data_clean3, "IDENTIFIER", summarise, rating.mean=mean(avgcpm_2, na.rm = TRUE))
cdat

# IDENTIFIER rating.mean
# 1       Post   0.8952306
# 2        Pre   1.0127284

ggplot(data_clean3, aes(avgcpm_2, fill = IDENTIFIER)) + geom_density(alpha = 0.2) + geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=IDENTIFIER),
                                                                                               linetype="dashed", size=1)

data_sub$revn<-as.numeric(data_sub$AMO_REV)
data_sub$nets<-as.numeric(data_sub$TOTAL_NET_SPEND)
data_sub$roas<-as.numeric(data_sub$ROAS)

library(dplyr)
sumdata<-dplyr::group_by(data_sub,PACKAGE_ID,IDENTIFIER)

sumrev<-dplyr::summarise(sumdata,
                  totalspend=sum(TOTAL_NET_SPEND,na.rm = TRUE),
                  totalrev=sum(AMO_REV,na.rm = TRUE),
                  avgroasnorm1=mean(ROAS, na.rm = TRUE))
roas<-dplyr::mutate(sumrev,
             avgroasnorm2=totalrev/totalspend)

roaspre <- subset(roas,IDENTIFIER=='Pre')

data2<-merge(data_sub,roaspre,by.x = "PACKAGE_ID", by.y = "PACKAGE_ID")
names(data2)
data3<-mutate(data2,avgroas1=ROAS/avgroasnorm1,avgroas2=ROAS/avgroasnorm2)


# hist(data3$avgroas1)
ggplot(data3,aes(x=avgroas2))+geom_histogram()+facet_grid(~IDENTIFIER.x)+theme_bw()

quantile(data3$avgroas2,0.9,na.rm = TRUE)
# 90% 
# 6.760785   

ecdf_fun <- function(x,perc) ecdf(x)(perc)
ecdf_fun(data3$avgroas2,1.5)
# 0.627451
ecdf_fun(data3$avgroas2,2)
# 0.6911765
ecdf_fun(data3$avgroas2,3)
# 0.7696078
ecdf_fun(data3$avgroas2,4)
# 0.8161765
ecdf_fun(data3$avgroas2,5)
# 0.8504902


data3<-mutate(data3, 
              roascap1 = ifelse(avgroas2 < 1.5, avgroas2, 1.5), 
              roascap2 = ifelse(avgroas2 < 2, avgroas2, 2),
              roascap3 = ifelse(avgroas2 < 3, avgroas2, 3),
              roascap4 = ifelse(avgroas2 < 4, avgroas2, 4),
              roascap5 = ifelse(avgroas2 < 5, avgroas2, 5))


# t-test
names(data3)
predata <- subset(data3, IDENTIFIER.x=='Pre')
postdata <- subset(data3, IDENTIFIER.x=='Post')  

names(predata)


t.test(postdata$roascap1,predata$roascap1,paired=FALSE)
# Welch Two Sample t-test
# 
# data:  postdata$roascap1 and predata$roascap1
# t = 5.2671, df = 405.57, p-value = 2.255e-07
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.1985243 0.4349593
# sample estimates:
#   mean of x mean of y 
# 1.0224656 0.7057239 


#Lift: 36.21

t.test(postdata$roascap2,predata$roascap2,paired=FALSE)

# Welch Two Sample t-test
# 
# data:  postdata$roascap2 and predata$roascap2
# t = 6.6971, df = 397.44, p-value = 7.275e-11
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.3612369 0.6614468
# sample estimates:
#   mean of x mean of y 
# 1.2881279 0.7767861 

#Lift 54.48%

t.test(postdata$roascap3,predata$roascap3,paired=FALSE)
# Welch Two Sample t-test
# 
# data:  postdata$roascap3 and predata$roascap3
# t = 8.1744, df = 370.71, p-value = 4.767e-15
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.661745 1.080961
# sample estimates:
#   mean of x mean of y 
# 1.7326386 0.8612855 


#Lift 86.19%


t.test(postdata$roascap4,predata$roascap4,paired=FALSE)
# Welch Two Sample t-test
# 
# data:  postdata$roascap4 and predata$roascap4
# t = 8.958, df = 340.47, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.9364732 1.4634369
# sample estimates:
#   mean of x mean of y 
# 2.1006044 0.9006494 

#Lift: 116.20

t.test(postdata$roascap5,predata$roascap5,paired=FALSE)
# Welch Two Sample t-test
# 
# data:  postdata$roascap5 and predata$roascap5
# t = 9.2859, df = 318.29, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1.165981 1.792892
# sample estimates:
#   mean of x mean of y 
# 2.4053060 0.9258694 


#Lift: 142.03

t.test(postdata$avgroas2,predata$avgroas2,paired=FALSE)
# Welch Two Sample t-test
# 
# data:  postdata$avgroas2 and predata$avgroas2
# t = 7.2712, df = 248.84, p-value = 4.61e-12
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   2.161503 3.767482
# sample estimates:
#   mean of x mean of y 
# 4.002228  1.037735 


#Lift: 207.56


library(ggplot2)
library(plyr)
cdat <- plyr::ddply(data3, "IDENTIFIER.x", summarise, rating.mean=mean(avgroas2, na.rm = TRUE))
cdat
# IDENTIFIER.x rating.mean
# 1         Post    4.002228
# 2          Pre    1.037735
ggplot(data3, aes(avgroas2, fill = IDENTIFIER.x)) + geom_density(alpha = 0.2) + geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=IDENTIFIER.x),
                                                                                           linetype="dashed", size=1)


cdat <- plyr::ddply(data3, "IDENTIFIER.x", summarise, rating.mean=mean(roascap2, na.rm = TRUE))
cdat
# IDENTIFIER.x rating.mean
# 1         Post   1.2881279
# 2          Pre   0.7767861

ggplot(data3, aes(roascap2, fill = IDENTIFIER.x)) + geom_density(alpha = 0.2) + geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=IDENTIFIER.x),
                                                                                           linetype="dashed", size=1)


#spend Rate box plot

means_actual_remove <- aggregate(DAILY_SPEND~IDENTIFIER, data_sub_1, mean)
means_actual_remove$DAILY_SPEND <- round(means_actual_remove$DAILY_SPEND, digits=2)

library(ggplot2)
data_sub_1$IDENTIFIER <- factor(data_sub_1$IDENTIFIER, levels=c('Pre','Post'))
ggplot(data=data_sub_1, aes(x=IDENTIFIER, y=DAILY_SPEND, fill=IDENTIFIER)) + geom_boxplot() + 
  geom_text(data = means_actual_remove, aes(label = DAILY_SPEND))

boxplot(x=data_sub_1$IDENTIFIER, y=data_sub_1$DAILY_SPEND)


####Paired tests

data_1_grp <- dplyr::group_by(data_sub_1, PACKAGE_ID, IDENTIFIER)
data_1_sum <- dplyr::summarise(data_1_grp, sum_net_spend_post = sum(TOTAL_NET_SPEND,na.rm = TRUE),sum_pkg_budget_post = sum(PACKAGE_BUDGET,na.rm = TRUE),avg_daily_spend=mean(TOTAL_NET_SPEND,na.rm=TRUE))
data_1_sum <- dplyr::mutate(data_1_sum, sr=sum_net_spend_post/sum_pkg_budget_post)

library(plyr)
cdat <- ddply(data_1_sum, "IDENTIFIER", summarise, rating.mean=mean(sr, na.rm=TRUE))
cdat

# IDENTIFIER rating.mean
# 1        Pre   0.7871173
# 2       Post   0.7780162

ggplot(data_1_sum, aes(sr, fill = IDENTIFIER)) + geom_density(alpha = 0.2) + geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=IDENTIFIER),
                                                                                        linetype="dashed", size=1)


data_1_sum_pre <- subset(data_1_sum, data_1_sum$IDENTIFIER=="Pre")
data_1_sum_post <- subset(data_1_sum, data_1_sum$IDENTIFIER=="Post")

wilcox.test(data_1_sum_post$sr, data_1_sum_pre$sr, paired=TRUE,conf.int = TRUE)

# Wilcoxon signed rank test
# 
# data:  data_1_sum_post$sr and data_1_sum_pre$sr
# V = 40, p-value = 0.7354
# alternative hypothesis: true location shift is not equal to 0
# 95 percent confidence interval:
#   -0.1041242  0.1014055
# sample estimates:
#   (pseudo)median 
# -0.01426072  



data_1_grp_1 <- dplyr::group_by(data_sub, PACKAGE_ID, IDENTIFIER)
data_1_sum_1 <- dplyr::summarise(data_1_grp_1, sum_net_spend = sum(TOTAL_NET_SPEND,na.rm = TRUE),sum_imp = sum(IMPRESSIONS,na.rm=TRUE))
data_1_sum_1 <- dplyr::mutate(data_1_sum_1, cpm_1=sum_net_spend/sum_imp)

library(plyr)
cdat <- plyr::ddply(data_1_sum_1, "IDENTIFIER", summarise, rating.mean=mean(cpm_1, na.rm=TRUE))
cdat

# IDENTIFIER rating.mean
# 1       Post 0.007413155
# 2        Pre 0.009759427

ggplot(data_1_sum_1, aes(cpm_1, fill = IDENTIFIER)) + geom_density(alpha = 0.2) + geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=IDENTIFIER),
                                                                                        linetype="dashed", size=1)


data_1_sum_pre <- subset(data_1_sum_1, data_1_sum_1$IDENTIFIER=="Pre")
data_1_sum_post <- subset(data_1_sum_1, data_1_sum_1$IDENTIFIER=="Post")

wilcox.test(data_1_sum_post$cpm_1, data_1_sum_pre$cpm_1, paired=TRUE,conf.int = TRUE, alternative = "less")

# Wilcoxon signed rank test
# 
# data:  data_1_sum_post$cpm_1 and data_1_sum_pre$cpm_1
# V = 19, p-value = 0.03406
# alternative hypothesis: true location shift is less than 0
# 95 percent confidence interval:
#   -Inf -0.0004268307
# sample estimates:
#   (pseudo)median 
# -0.002302403  



data_1_grp_2 <- dplyr::group_by(data_sub, PACKAGE_ID, IDENTIFIER)
data_1_sum_2 <- dplyr::summarise(data_1_grp_2, sum_net_spend = sum(TOTAL_NET_SPEND, na.rm = TRUE),sum_rev = sum(AMO_REV, na.rm = TRUE))
data_1_sum_2 <- dplyr::mutate(data_1_sum_2, roas_1=sum_rev/sum_net_spend)

library(plyr)
cdat <- plyr::ddply(data_1_sum_2, "IDENTIFIER", summarise, rating.mean=mean(roas_1, na.rm=TRUE))
cdat

# IDENTIFIER rating.mean
# 1       Post   1.1031835
# 2        Pre   0.5174002

ggplot(data_1_sum_2, aes(roas_1, fill = IDENTIFIER)) + geom_density(alpha = 0.2) + geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=IDENTIFIER),
                                                                                             linetype="dashed", size=1)


data_1_sum_pre <- subset(data_1_sum_2, data_1_sum_2$IDENTIFIER=="Pre")
data_1_sum_post <- subset(data_1_sum_2, data_1_sum_2$IDENTIFIER=="Post")

wilcox.test(data_1_sum_post$roas_1, data_1_sum_pre$roas_1, paired=TRUE,conf.int = TRUE, alternative="greater")

# Wilcoxon signed rank test
# 
# data:  data_1_sum_post$roas_1 and data_1_sum_pre$roas_1
# V = 86, p-value = 0.001221
# alternative hypothesis: true location shift is greater than 0
# 95 percent confidence interval:
#   0.0009429078          Inf
# sample estimates:
#   (pseudo)median 
# 0.03284093  


