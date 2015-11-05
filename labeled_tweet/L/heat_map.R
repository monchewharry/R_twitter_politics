B<-read.csv("../L/liberal top50# freq.csv",header = T)
library(ggplot2)
library(plyr)
library(dplyr)
## ggplot2
b<-B[c(-1,-3)]

b$time_index<-as.Date(b$time_index,format = "%m/%d/%y")
library(reshape2)
b1<-melt(b,id.vars = c("time_index"))

b1<-cbind(b1,period=c(rep(1,18),rep(2,19),rep(3,18)))  
b2<-b1[-1]
head(b2)
value2<-group_by(b2,variable,period)

heat_mat<-summarise(value2,period_sum=sum(value))  
dim(heat_mat)  

heat_m<-matrix(heat_mat$period_sum, ncol = 3,byrow = T)
colnames(heat_m)<-c("1","2","3")
rownames(heat_m)<-levels(heat_mat$variable)
heat_m<-as.data.frame(heat_m)
heat_m$Name<-rownames(heat_m)
head(heat_m)
nba.m <- melt(heat_m)


nba.m<-rename(nba.m,period=variable)

###########  

library(reshape2, ggplot2)
dat <- nba.m
png("../L/heat_liberal.png",width = 1500,height = 1000)
p1 <- ggplot(dat, aes(period, Name, group=Name)) +
  geom_tile(aes(fill = value),colour = "black") +
  geom_text(aes(fill = dat$value, label = as.character(round(dat$value, 1)))) +
  scale_fill_gradient(low = "white", high = "red") 
p1
dev.off()


name<-c("teaparty","tcot","scotus","p2","obamacare","ilikeobamacare"
        ,"healthcare","hcr","gop","fullrepeal","aca")
nba.m2<- filter(nba.m,Name %in%name)
dat <- nba.m2
png("../L/heat_liberal_top.png",width = 1500,height = 1000)
p1 <- ggplot(dat, aes(period, Name, group=Name)) +
  geom_tile(aes(fill = value),colour = "black") +
  geom_text(aes(fill = dat$value, label = as.character(round(dat$value, 1)))) +
  scale_fill_gradient(low = "white", high = "red") 
p1
dev.off()


