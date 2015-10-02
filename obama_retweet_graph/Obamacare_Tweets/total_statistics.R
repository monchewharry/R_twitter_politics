load("/Users/CDX/Google\ Drive/twitter_politics_data/obamacare.RData")
l<-duplicated(sapply(obamacare,FUN = function(x) x$id_str))#delete duplicated tweet
#which(l==T)
identical(obamacare[[329877]],obamacare[[329878]])
obamacare<-obamacare[-which(l==T)]
# the top 5 (or 10) most retweeted tweets (and # times retweeted)
# the top 5 (or 10) most @mentioned users (and number of times mentioned)
# the top 5 (or 10) URLs mentioned (and # times mentioned)
# the top 5 (or 10) locations (and # tweets from each)
str(obamacare[[101010]])
obamacare[[101010]]$id_str
obamacare[[101010]]$retweeted_status$id_str


retweet_id<-lapply(obamacare,FUN = function(x) x$retweeted_status$id_str)
id_length<-sapply(retweet_id, FUN=function(x) length(x))
which(id_length>1)# varify all the same length
null_loc<-sapply(retweet_id,FUN=function(x) is.null(x))
retweet_id<-retweet_id[-which(null_loc==T)]
freq<-table(retweet_id)
save(freq,file = "retweet_freq.RData")

