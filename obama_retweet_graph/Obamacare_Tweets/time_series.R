load("/Users/CDX/Google\ Drive/twitter_politics_data/obamacare.RData")# The data list is saved
str(obamacare[[100]])
obamacare[[100]]$created_at

#### build the volume time series####
x1 <- strptime(obamacare[[1]]$created_at
               ,format="%a %b %d %H:%M:%S %z %Y",tz = "UTC")#see ?strftime for %
time_stamp<-sapply(obamacare,FUN = function(x) return(as.POSIXct(x$created_at,
                                              format="%a %b %d %H:%M:%S %z %Y",
                                              tz = "UTC")))

time_stamp<-time_stamp+(x1-time_stamp[1])##origin "1970-01-01 GMT"
time_stamp<-strftime(time_stamp, format="%D")#day/month/year
time_index<-time_stamp#copy


(volume<-data.frame(table(time_index)))
volume$time_index<-as.character(volume$time_index)
save(volume,file = "volume.RData")

library(quantmod)
tweet_volume<-as.xts(volume$Freq,as.Date(volume$time_index,format = "%m/%d/%y"))
#### volume plot #####
png("tweet volume1.png")
nf <- layout(matrix(c(1,1,1,2,3,4),2,3,byrow = TRUE), c(1,1,1), c(2,1), TRUE)

plot(tweet_volume,main="tweet volume over 3 periods")
plot(tweet_volume["/2012-04-03"],main="1st period")
plot(tweet_volume["2012-06-19::2012-07-07"],main="2nd period")
plot(tweet_volume["2012-10-22::2012-11-08"],main="3rd peirod")
dev.off()

png("tweet volume2.png")
chartSeries(tweet_volume,type="line")
dev.off()

png("volume ~2012-04-03.png")
chartSeries(tweet_volume,type="line",
            subset="/2012-04-03")
dev.off()
png("volume 2012-06-19~2012-07-07.png")
chartSeries(tweet_volume,type="line",
            subset="2012-06-19::2012-07-07")
dev.off()
png("volume 2012-10-22~2012-11-08.png")
chartSeries(tweet_volume,type="line",
            subset="2012-10-22::2012-11-08")
dev.off()

#### top 5 hashtag each day ####  
cut_point<-which(!duplicated(time_index))[-1]-1 
cut_point<-c(0,cut_point,length(time_index))#the point where the date changes  



get_hash<-function(x){
  sapply(x$entities$hashtags,function(y) return(y$text))
}

#lapply(obamacare[1:5], FUN = get_hash)

hash_tags<-list()
for(i in seq(length(cut_point)-1)){#collect hashtags for each tweet on each day
  hash_period<-lapply(obamacare[cut_point[i]+1:cut_point[i+1]]
                      ,FUN = get_hash)
  hash_tags[[i]]<-hash_period
}
#save(hash_tags,file = "hash_tags.RData")

top5<-function(x){
  freq<-table(unlist(x))
  sorted<-names(freq)[order(freq,decreasing = T)]
  if(length(sorted>5)) return(sorted[1:5])
  else return(sorted[1])
}

tophash<-t(sapply(hash_tags,FUN = top5))
colnames(tophash)<-c("top5_1","top5_2","top5_3","top5_4","top5_5")
tophash<-as.data.frame(tophash,stringsAsFactors = F)

A<-cbind(volume,tophash)#to merge the time series
library(dplyr)
A<-rename(A,volume=Freq)
write.csv(A,file="ts_list.csv")

#### top_5 hashtag freq #####  
load("hash_tags.RData")
freq<-table(unlist(hash_tags))
top_5<-names(freq)[order(freq,decreasing = T)][1:6] # the top 5 # over the whole period,two of them are the same

dailyfreq<-function(x){
  freq5<-NULL
  for(tag in top_5){
    freq5<-c(freq5,sum(unlist(x)==tag))
  }
  freq5
}
top5_freq<-t(sapply(hash_tags,FUN = dailyfreq))
colnames(top5_freq)<-top_5
top5_freq<-as.data.frame(top5_freq,stringsAsFactors = F)
head(top5_freq)
library(dplyr)
top5_freq<-select((mutate(top5_freq,Obamacare=Obamacare+ObamaCare)),-ObamaCare)# merge the same hashtags

(B<-cbind(volume,top5_freq))#to merge the time series
B<-rename(B,volume=Freq)
write.csv(B,file="ts_list2.csv")
library(xts)
plot(as.xts(B$Obamacare,as.Date(B$time_index,format = "%m/%d/%y")))
lines(as.xts(B$tcot,as.Date(B$time_index,format = "%m/%d/%y")))


