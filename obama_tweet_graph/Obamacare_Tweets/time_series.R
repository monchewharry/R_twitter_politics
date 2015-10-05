load("/Users/CDX/Google\ Drive/twitter_politics_data/obamacare.RData")# The data list is saved
# str(obamacare[[100]])
# obamacare[[100]]$created_at  
library(ggplot2);library(dplyr)

#### 1. build the volume time series####
load("volume.RData") 
# x1 <- strptime(obamacare[[1]]$created_at
#                ,format="%a %b %d %H:%M:%S %z %Y",tz = "UTC")#see ?strftime for %
# time_stamp<-sapply(obamacare,FUN = function(x) return(as.POSIXct(x$created_at,
#                                               format="%a %b %d %H:%M:%S %z %Y",
#                                               tz = "UTC")))
# 
# time_stamp<-time_stamp+(x1-time_stamp[1])##origin "1970-01-01 GMT"
# time_stamp<-strftime(time_stamp, format="%D")#day/month/year
# time_index<-time_stamp#copy
# 
# 
# (volume<-data.frame(table(time_index)))
# volume$time_index<-as.character(volume$time_index)
#save(volume,file = "volume.RData")

tweet_volume<-as.xts(volume$Freq,as.Date(volume$time_index,format = "%m/%d/%y"))

#### volume plot #####
library(quantmod)
# png("tweet volume1.png")
# nf <- layout(matrix(c(1,1,1,2,3,4),2,3,byrow = TRUE), c(1,1,1), c(2,1), TRUE)
# 
# plot(tweet_volume,main="tweet volume over 3 periods")
# plot(tweet_volume["/2012-04-03"],main="1st period")
# plot(tweet_volume["2012-06-19::2012-07-07"],main="2nd period")
# plot(tweet_volume["2012-10-22::2012-11-08"],main="3rd peirod")
# dev.off()
# 
# png("tweet volume2.png")
# chartSeries(tweet_volume,type="line")
# dev.off()
# 
# png("volume ~2012-04-03.png")
# chartSeries(tweet_volume,type="line",
#             subset="/2012-04-03")
# dev.off()
# png("volume 2012-06-19~2012-07-07.png")
# chartSeries(tweet_volume,type="line",
#             subset="2012-06-19::2012-07-07")
# dev.off()
# png("volume 2012-10-22~2012-11-08.png")
# chartSeries(tweet_volume,type="line",
#             subset="2012-10-22::2012-11-08")
# dev.off()

#### 2. daily top50 hashtag ####
A<-read.csv("daily top5 #.csv",header = T)

# cut_point<-which(!duplicated(time_index))[-1]-1 
# cut_point<-c(0,cut_point,length(time_index))#the point where the date changes  
# 
# 
# 
# get_hash<-function(x){
#   sapply(x$entities$hashtags,function(y) return(y$text))
# }

#lapply(obamacare[1:5], FUN = get_hash)

# hash_tags<-list()
# for(i in seq(length(cut_point)-1)){#collect hashtags for each tweet on each day
#   hash_period<-lapply(obamacare[cut_point[i]+1:cut_point[i+1]]
#                       ,FUN = get_hash)
#   hash_tags[[i]]<-hash_period
# }
# #save(hash_tags,file = "hash_tags.RData")
# 
# top5<-function(x){
#   freq<-table(unlist(x))
#   sorted<-names(freq)[order(freq,decreasing = T)]
#   if(length(sorted>5)) return(sorted[1:5])
#   else return(sorted[1])
# }
# 
# tophash<-t(sapply(hash_tags,FUN = top5))
# colnames(tophash)<-c("top5_1","top5_2","top5_3","top5_4","top5_5")
# tophash<-as.data.frame(tophash,stringsAsFactors = F)
# 
# A<-cbind(volume,tophash)#to merge the time series
# library(dplyr)
# A<-rename(A,volume=Freq)
# write.csv(A,file="daily top5 #.csv")

#### 3. top_50 hashtag's daily freq #####
B<-read.csv("ts_list2.csv")

load("hash_tags.RData") 
freq<-table(unlist(hash_tags))
top_50<-names(freq)[order(freq,decreasing = T)][1:50] # the top 5 # over the whole period,two of them are the same

dailyfreq<-function(x){
  freq50<-NULL
  for(tag in top_50){
    freq50<-c(freq50,sum(unlist(x)==tag))
  }
  freq50
}
top50_freq<-t(sapply(hash_tags,FUN = dailyfreq))

colnames(top50_freq)<-top_50
top50_freq<-as.data.frame(top50_freq,stringsAsFactors = F)
head(top50_freq)

top50_freq<-select((mutate(top50_freq,Obamacare=Obamacare+ObamaCare)),-ObamaCare)# merge the same hashtags

(B<-cbind(volume,top50_freq))#to load volume first!
B<-rename(B,volume=Freq)
write.csv(B,file="ts_list2.csv")


# overlaid picture
library(xts)
Obamacare<-as.xts(B$Obamacare,as.Date(B$time_index,format = "%m/%d/%y"))
tcot<-as.xts(B$tcot,as.Date(B$time_index,format = "%m/%d/%y"))
SCOTUS<-as.xts(B$SCOTUS,as.Date(B$time_index,format = "%m/%d/%y"))
p2<-as.xts(B$p2,as.Date(B$time_index,format = "%m/%d/%y"))
healthcare<-as.xts(B$healthcare,as.Date(B$time_index,format = "%m/%d/%y"))

## ggplot2
b<-B[c(-2)]
b$time_index<-as.Date(b$time_index,format = "%m/%d/%y")
b1<-melt(b,id.vars = c("time_index"))
b1<-cbind(b1,period=c(rep(1,18),rep(2,19),rep(3,18)))
png("daily freq of top50#.png",width=1500,height=850)
ggplot(b1) + geom_line(aes(x=time_index, y=value, colour=variable)) +
  scale_colour_manual(values=seq(50))+
  facet_wrap( ~ period,scale="free_x")+
  labs( title = "daily freq of top5#")
dev.off()

# respective picture  without unuseful period  

# ggplot(b, aes(time_index, Obamacare)) +xlab("period")+
#   ylab("hashtags")+
#   geom_line(lty = 1,colour = 1)#delete useless period
period1<-"2012-03-17::2012-04-03"
period2<-"2012-06-19::2012-07-07"
period3<-"2012-10-22::2012-11-08"

chartSeries(Obamacare,subset =period1)
chartSeries(Obamacare,subset =period2)
chartSeries(Obamacare,subset =period3)

chartSeries(tcot,subset =period1)
chartSeries(tcot,subset =period2)
chartSeries(tcot,subset =period3)

chartSeries(SCOTUS,subset =period1)
chartSeries(SCOTUS,subset =period2)
chartSeries(SCOTUS,subset =period3)

chartSeries(p2,subset =period1)
chartSeries(p2,subset =period2)
chartSeries(p2,subset =period3)

chartSeries(healthcare,subset =period1)
chartSeries(healthcare,subset =period2)
chartSeries(healthcare,subset =period3)

