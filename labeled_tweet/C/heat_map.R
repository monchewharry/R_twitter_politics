B<-read.csv("convserv top50# freq.csv",header = T)
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
heat_m<-matrix(heat_mat$period_sum, ncol = 3,byrow = T)
colnames(heat_m)<-c("1","2","3")
rownames(heat_m)<-levels(heat_mat$variable)
heat_m<-as.data.frame(heat_m)
heat_m$Name<-rownames(heat_m)
head(heat_m)
nba.m <- melt(heat_m)


###########  
library(ggplot2)
library(scales)
png("heat.png")
nba.m <- ddply(nba.m, .(variable), transform,
                     rescale = rescale(value))
(p <- ggplot(nba.m, aes(variable, Name)) +
  geom_tile(aes(fill = rescale),colour = "black") + 
  scale_fill_gradient(low = "blue",high = "red"))
dev.off()
