load("tweet_liberal.RData")
library(ggplot2);library(dplyr)  

##### 1.volume_liberal#####
x1 <- strptime(tweet_liberal[[1]]$created_at
               ,format="%a %b %d %H:%M:%S %z %Y",tz = "UTC")#see ?strftime for %
time_stamp<-sapply(tweet_liberal,FUN = function(x) return(as.POSIXct(x$created_at,
                                              format="%a %b %d %H:%M:%S %z %Y",
                                              tz = "UTC")))

time_stamp<-time_stamp+(x1-time_stamp[1])##origin "1970-01-01 GMT"
time_stamp<-strftime(time_stamp, format="%D")#day/month/year
time_index<-time_stamp#copy
# 
# 
# (volume<-data.frame(table(time_index)))
# volume$time_index<-as.character(volume$time_index)
# volume_liberal<-volume
# save(volume_liberal,file = "volume_liberal.RData")
load("volume_liberal.RData")

##plot
# library(quantmod)
# liberal_volume<-as.xts(volume_liberal$Freq,as.Date(volume_liberal$time_index,format = "%m/%d/%y"))
# png("image/liberal volume1.png")
# nf <- layout(matrix(c(1,1,1,2,3,4),2,3,byrow = TRUE), c(1,1,1), c(2,1), TRUE)
# 
# plot(liberal_volume,main="liberal tweet volume over 3 periods")
# plot(liberal_volume["/2012-04-03"],main="1st period")
# plot(liberal_volume["2012-06-19::2012-07-07"],main="2nd period")
# plot(liberal_volume["2012-10-22::2012-11-08"],main="3rd peirod")
# dev.off()
# 
# png("image/liberal tweet volume2.png")
# chartSeries(liberal_volume,type="line")
# dev.off()
# 
# png("image/liberal tweet volume ~2012-04-03.png")
# chartSeries(liberal_volume,type="line",
#             subset="/2012-04-03")
# dev.off()
# png("image/liberal tweet volume 2012-06-19~2012-07-07.png")
# chartSeries(liberal_volume,type="line",
#             subset="2012-06-19::2012-07-07")
# dev.off()
# png("image/liberal tweet volume 2012-10-22~2012-11-08.png")
# chartSeries(liberal_volume,type="line",
#             subset="2012-10-22::2012-11-08")
# dev.off()

#### 2. daily top5 hashtag ####
# cut_point<-which(!duplicated(time_index))[-1]-1 
# cut_point<-c(0,cut_point,length(time_index))#the point where the date changes  
# 
# 
# get_hash<-function(x){
#   sapply(x$entities$hashtags,function(y) return(y$text))
# }
# 
# lapply(tweet_liberal[1:5], FUN = get_hash)
# 
# hash_tags<-list()
# for(i in seq(length(cut_point)-1)){#collect hashtags for each tweet on each day
#   hash_period<-lapply(tweet_liberal[cut_point[i]+1:cut_point[i+1]]
#                       ,FUN = get_hash)
#   hash_tags[[i]]<-hash_period
# }
# liberal_hashtags<-hash_tags
# save(liberal_hashtags,file = "liberal_hashtags.RData")
load("liberal_hashtags.RData")

top5<-function(x){
  freq<-table(unlist(x))
  sorted<-names(freq)[order(freq,decreasing = T)]
  if(length(sorted>5)) return(sorted[1:5])
  else return(sorted[1])
}

tophash<-t(sapply(liberal_hashtags,FUN = top5))
colnames(tophash)<-c("top5_1","top5_2","top5_3","top5_4","top5_5")
tophash<-as.data.frame(tophash,stringsAsFactors = F)

A<-cbind(volume_liberal,tophash)#to merge the time series
A<-rename(A,volume_liberal=Freq)
write.csv(A,file="liberal daily top5 #.csv")

#### 3. top_50 hashtag's daily freq #####

# load("liberal_hashtags.RData") 
# freq<-table(unlist(liberal_hashtags))
# top_50<-names(freq)[order(freq,decreasing = T)][1:50] # the top 5 # over the whole period,two of them are the same
# 
# dailyfreq<-function(x){
#   freq50<-NULL
#   for(tag in top_50){
#     freq50<-c(freq50,sum(unlist(x)==tag))
#   }
#   freq50
# }
# top50_freq<-t(sapply(liberal_hashtags,FUN = dailyfreq))
# 
# colnames(top50_freq)<-top_50
# top50_freq<-as.data.frame(top50_freq,stringsAsFactors = F)
# head(top50_freq)
# 
# top50_freq<-select((mutate(top50_freq,Obamacare=Obamacare+ObamaCare+obamacare+OBAMACARE)),-c(ObamaCare,obamacare,OBAMACARE))# merge the same hashtags
# 

(B<-cbind(volume_liberal,top50_freq))#to load volume first!
write.csv(B,file="liberal top50# freq.csv")


## ggplot2
b<-B[-1]
b$time_index<-as.Date(b$time_index,format = "%m/%d/%y")
library(reshape2)
b1<-melt(b,id.vars = c("time_index"))

b1<-cbind(b1,period=c(rep(1,18),rep(2,19),rep(3,18)))
png("image/daily freq of liberal top50#.png",width=1500,height=850)
ggplot(b1) + geom_line(aes(x=time_index, y=value, colour=variable)) +
  scale_colour_manual(values=seq(50))+
  facet_wrap( ~ period,scale="free_x")+
  labs( title = "daily freq of liberal top50#")
dev.off()

#### 4. static statistics#####
l<-duplicated(sapply(tweet_liberal,FUN = function(x) x$id_str))#delete duplicated tweet
#which(l==T)

tweet_liberal<-tweet_liberal[-which(l==T)]

##### 4.1 the top 50 most retweeted tweets (and # times retweeted)#####


total_id<-lapply(tweet_liberal,FUN = function(x) x$retweeted_status$id_str)
id_length<-sapply(total_id, FUN=function(x) length(x))
which(id_length>1)# varify all the same length

null_loc<-sapply(total_id,FUN=function(x) is.null(x))
retweet_id<-total_id[-which(null_loc==T)]
class(retweet_id)
retweet_freq<-table(unlist(retweet_id))
top50_retweet<-data.frame(id=names(retweet_freq)[order(retweet_freq,decreasing = T)][1:50]
                          ,freq=retweet_freq[order(retweet_freq,decreasing = T)][1:50]
                          ,stringsAsFactors = F)


id_text<-t(sapply(tweet_liberal, function(x) c(x$id_str,x$text)))
colnames(id_text)<-c("id","text")
id_text<-as.data.frame(id_text,stringsAsFactors = F)

# careful that some original tweet isnot in the dataset
tweetid<-sapply(tweet_liberal,function(x) x$id_str)
loc<-(top50_retweet$id %in% unlist(retweet_id)[unlist(retweet_id) %in% tweetid==FALSE])
(top50_retweet$id[loc])

for(i in seq(length(top50_retweet$id[loc]))){
  txt<-sapply(tweet_liberal
              ,FUN= function(x) if(identical(x$retweeted_status$id_str==top50_retweet$id[loc][i]
                                             ,TRUE)) return(x$text) else return(NULL)) 
  id_text<-rbind(id_text,c(top50_retweet$id[loc][i],unlist(txt)[1]))
}

id_text<-distinct(id_text)

for(i in seq(length(top50_retweet$id))){
  top50_retweet$text[i]<-id_text$text[id_text$id==top50_retweet$id[i]]
}
rownames(top50_retweet)<-NULL

write.csv(top50_retweet,file = "liberal's top50_retweet.csv")  

###### the top 5 (or 10) most @mentioned users (and number of times mentioned)#####
# user_mentioned<-lapply(tweet_liberal, function(x) x$entities$user_mentions)
# mentioned<-lapply(user_mentioned, function(x) sapply(x, function(x) c(x$id_str,x$name) ) )
# null.loc<-sapply(mentioned,function(x) is.null(unlist(x)))
# mentioned<-mentioned[!null.loc]
# mentioned<-matrix(unlist(mentioned),nrow=2,byrow=F)
# rownames(mentioned)<-c("id","name")
# mentioned<-as.data.frame(t(mentioned),stringsAsFactors = F)
# 
# freq<-data.frame(table(mentioned$id))
# freq<-arrange(freq,desc(Freq))
# names(freq)<-c("id","freq")
# freq<-merge(freq,mentioned)
# freq<-freq[!duplicated(freq$id),]
# freq<-arrange(freq,desc(freq))
# mentioned_freq<-freq
#write.csv(mentioned_freq,file = "mentiomentioned_freq.csv")
mentioned_freq<-read.csv("most_mentioned_freq.csv",colClasses=c("NULL",NA,NA,NA),stringsAsFactors = F)
head(mentioned_freq)


# the top 5 (or 10) URLs mentioned (and # times mentioned)
url_mentioned<-sapply(tweet_liberal, function(x) x$entities$urls)

# the top 5 (or 10) locations (and # tweets from each)
user_location<-sapply(tweet_liberal, function(x) x$user$location)

