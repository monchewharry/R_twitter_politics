library(dplyr)
load("/Users/CDX/Google\ Drive/twitter_politics_data/obamacare.RData")
l<-duplicated(sapply(obamacare,FUN = function(x) x$id_str))#delete duplicated tweet
#which(l==T)
identical(obamacare[[329877]],obamacare[[329878]])
obamacare<-obamacare[-which(l==T)]


##### the top 5 (or 10) most retweeted tweets (and # times retweeted)#####
# str(obamacare[[101010]])
# obamacare[[101010]]$id_str
# obamacare[[101010]]$retweeted_status$id_str

total_id<-lapply(obamacare,FUN = function(x) x$retweeted_status$id_str)
id_length<-sapply(total_id, FUN=function(x) length(x))
which(id_length>1)# varify all the same length

null_loc<-sapply(total_id,FUN=function(x) is.null(x))
retweet_id<-total_id[-which(null_loc==T)]
class(retweet_id)
retweet_freq<-table(unlist(retweet_id))
top10_retweet<-data.frame(id=names(retweet_freq)[order(retweet_freq,decreasing = T)][1:10]
           ,freq=retweet_freq[order(retweet_freq,decreasing = T)][1:10]
           ,stringsAsFactors = F)


id_text<-t(sapply(obamacare, function(x) c(x$id_str,x$text)))
colnames(id_text)<-c("id","text")
id_text<-as.data.frame(id_text,stringsAsFactors = F)

# careful that some original tweet isnot in the dataset
tweetid<-sapply(obamacare,function(x) x$id_str)
loc<-(top10_retweet$id %in% unlist(retweet_id)[unlist(retweet_id) %in% tweetid==FALSE])
(top10_retweet$id[loc])

for(i in seq(length(top10_retweet$id[loc]))){
  txt<-sapply(obamacare
              ,FUN= function(x) if(identical(x$retweeted_status$id_str==top10_retweet$id[loc][i]
                                                      ,TRUE)) return(x$text) else return(NULL)) 
  id_text<-rbind(id_text,c(top10_retweet$id[loc][i],unlist(txt)[1]))
}
id_text<-distinct(id_text)

for(i in seq(length(top10_retweet$id))){
  top10_retweet$text[i]<-id_text$text[id_text$id==top10_retweet$id[i]]
}
rownames(top10_retweet)<-NULL

write.csv(top10_retweet,file = "top10_retweet.csv")
###### the top 5 (or 10) most @mentioned users (and number of times mentioned)#####
# the top 5 (or 10) URLs mentioned (and # times mentioned)
# the top 5 (or 10) locations (and # tweets from each)
