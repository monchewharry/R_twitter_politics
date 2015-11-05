setwd("~/R_twitter_politics/des_table")
library(dplyr)

load("/Users/dcao28/R_twitter_politics/labeled_tweet/C/volume_conserv.RData")
load("/Users/dcao28/R_twitter_politics/labeled_tweet/L/volume_liberal.RData")

###### new iden(add id&elite to unique user) ########
# load("~/Downloads/obamacare_labeled.RData")
# obamacare<-obamacare_labeled
# vertices_list<-t(sapply(obamacare,FUN = function(x) c(x$user$id_str,x$user$screen_name)))
# 
# vertices2<-function(x){
#   if(!identical(NULL,x$retweeted_status)) return(c(x$retweeted_status$user$id_str,x$retweeted_status$user$screen_name))
#   else return(c(NA,NA))
# }
# 
# vertices_list2<-t(sapply(obamacare,FUN = vertices2))
# vertices_list2<-vertices_list2[!is.na(vertices_list2[,1]),]
# 
# vertices_list<-rbind(vertices_list,vertices_list2)
# colnames(vertices_list)<-c("id","screen_name")
# vertices_list<-as.data.frame(vertices_list,stringsAsFactors = F)
# getid<- vertices_list[!duplicated(vertices_list$screen_name),]
# 
# new_iden<-merge(iden,getid,all.x = TRUE,by="screen_name")
# new_iden<-select(new_iden,c(id,screen_name,name,ideology,followers_count,retweet_count))
# write.csv(new_iden,"new_iden.csv")
# 
# 
# elites1<-read.csv("../labeled_tweet/elite/elite_t.csv",header = T,)
# elites2<-read.csv("../labeled_tweet/elite/elite_f.csv",header = T)
# elite<-rbind(elites1[c(2,6)],elites2[c(3,2)]);head(elite)
# names(elite)<-c("screen_name","ideology")
# elite$elite<-TRUE
# head(elite)
# head(new_iden)
# new<-merge(new_iden,elite,all.x = T,by = "screen_name")
# new$elite[is.na(new$elite)]<-FALSE
# 
# filter(new,is.na(id) & elite==TRUE)#check
# write.csv(new,"new_iden.csv")  
# 
iden<-read.csv("new_iden.csv" ,header = TRUE)
iden<-select(iden,-(ideology.y))
iden<-rename(iden,ideology=ideology.x)  
head(iden)
iden<-filter(iden,elite==TRUE)

##### new tweet_c/l (add elite)#######
load("~/Downloads/tweet_conserv.RData")
load("~/Downloads/tweet_liberal.RData")

# add_elite<-function(x){
#   if((x$user$screen_name %in% iden$screen_name) ||
#      (x$user$id_str %in% iden$id)){
#     x$elite<-TRUE
#   }else{
#     x$elite<-FALSE
#   }
#   return(x)
# }
# 
# tweet_conserv<-lapply(tweet_conserv, FUN = add_elite)
# tweet_liberal<-lapply(tweet_liberal, FUN = add_elite)

# tweet_conserv_elite<- sapply(tweet_conserv, function(x) x$elite)
# save(tweet_conserv_elite,file = "conserv_elite.RData")
load("conserv_elite.RData")
tweet_conserv_elite<-tweet_conserv[tweet_conserv_elite]

# tweet_liberal_elite<-sapply(tweet_liberal, function(x) x$elite)
# save(tweet_liberal_elite,file = "liberal_elite.RData")
load("liberal_elite.RData")
tweet_liberal_elite<-tweet_liberal[tweet_liberal_elite]

######new volume for elites #######
# x1 <- strptime(tweet_liberal_elite[[1]]$created_at
#                ,format="%a %b %d %H:%M:%S %z %Y",tz = "UTC")#see ?strftime for %
# 
# time_stamp<-sapply(tweet_liberal_elite,FUN = function(x) return(as.POSIXct(x$created_at,
#                                                                      format="%a %b %d %H:%M:%S %z %Y",
#                                                                      tz = "UTC")))
# 
# time_stamp<-time_stamp+(x1-time_stamp[1])##origin "1970-01-01 GMT"
# time_stamp<-strftime(time_stamp, format="%D")#day/month/year
# time_index<-time_stamp#copy
# 
# 
# (volume<-data.frame(table(time_index)))
# volume$time_index<-as.character(volume$time_index)
# volume_liberal<-volume
# save(volume_liberal,file = "volume_liberal.RData")

load("volume_conserv.RData")
load("volume_liberal.RData")
dim(volume_conserv);dim(volume_liberal)
volume_conserv<-rename(volume_conserv,Freq.c=Freq)
volume_liberal<-rename(volume_liberal,Freq.l=Freq)
head(volume_conserv);head(volume_liberal)

volume<-merge(volume_conserv,volume_liberal,all = T)
volume[is.na(volume)]<-0
(volume<-mutate(volume,sum=Freq.c+Freq.l))

###############table 1 ################## 

head(volume)
head(iden)
tweet_conserv_elite[[123]]$retweeted_status

#conserv retweet tweets percentage
total_id<-lapply(tweet_conserv_elite,FUN = function(x) x$retweeted_status$id_str)
null_loc<-sapply(total_id,FUN=function(x) is.null(x))
(length(null_loc)-sum(null_loc))/length(null_loc)
#liberal retweet tweets percentage
total_id<-lapply(tweet_liberal_elite,FUN = function(x) x$retweeted_status$id_str)
null_loc<-sapply(total_id,FUN=function(x) is.null(x))
(length(null_loc)-sum(null_loc))/length(null_loc)

#convert average retweet per tweet  
ret_num1<-lapply(tweet_conserv_elite,FUN = function(x) x$retweet_count)
sum(unlist(ret_num1))/length(tweet_conserv_elite)
#liberal average retweet per tweet  
ret_num2<-lapply(tweet_liberal_elite,FUN = function(x) x$retweet_count)
sum(unlist(ret_num2))/length(tweet_liberal_elite)  

(sum(unlist(ret_num1))+sum(unlist(ret_num2)))/(length(tweet_liberal_elite)+length(tweet_conserv_elite))

# conserv mention percentage  
total_id<-lapply(tweet_conserv_elite,FUN = function(x) x$entities$user_mentions)
null_loc1<-sapply(total_id,FUN=function(x) is.null(unlist(x)))
(length(null_loc1)-sum(null_loc1))/length(null_loc1)

# liberal mention percentage  
total_id<-lapply(tweet_liberal_elite,FUN = function(x) x$entities$user_mentions)
null_loc2<-sapply(total_id,FUN=function(x) is.null(unlist(x)))
(length(null_loc2)-sum(null_loc2))/length(null_loc2)

((length(null_loc1)-sum(null_loc1))+(length(null_loc2)-sum(null_loc2)))/(length(null_loc1)+length(null_loc2))


# converv word average  
coun<-function(x){
  sapply(gregexpr("\\W+", x$text), length) + 1
}

word_len<-sapply(tweet_conserv_elite, FUN = coun)
mean(word_len)
# liberal word average  
word_len2<-sapply(tweet_liberal_elite, FUN = coun)
mean(word_len2)

mean(c(word_len,word_len2))

# conserv hashtags percentage  
total_id<-lapply(tweet_conserv,FUN = function(x) x$entities$hashtags)
null_loc1<-sapply(total_id,FUN=function(x) is.null(unlist(x)))
(length(null_loc1)-sum(null_loc1))/length(null_loc1)

# liberal hashtags percentage  
total_id<-lapply(tweet_liberal,FUN = function(x) x$entities$hashtags)
null_loc2<-sapply(total_id,FUN=function(x) is.null(unlist(x)))
(length(null_loc2)-sum(null_loc2))/length(null_loc2)

((length(null_loc1)-sum(null_loc1))+(length(null_loc2)-sum(null_loc2)))/(length(null_loc1)+length(null_loc2))



# conserv top10uses's tweet  
id1<-sapply(tweet_conserv, function(x) x$user$id_str)  


id2<-sapply(tweet_liberal, function(x) x$user$id_str) 

## cross retweet  
id_l<-filter(iden,ideology=="L")$screen_name
id_c<- filter(iden,ideology=="C")$screen_name
cross<-function(x){
  if(is.null(x$retweeted_status)) return(FALSE)
  else{
    if(x$retweeted_status$user$screen %in% id_c) return(TRUE)
  }
}

a<-sapply(tweet_liberal, FUN =cross)
sum(unlist(a))

###############for elite########


des<-data.frame(
  user_num=c(nrow(iden), nrow(filter(iden,ideology=="C")),nrow(filter(iden,ideology=="L"))) 
                 followers_num= c(sum(iden$followers_count),sum(filter(iden,ideology=="C")$followers_count),sum(filter(iden,ideology=="L")$followers_count))
                 following_num= rep(NA,3)
  
                 total_tweets=c(length(tweet_conserv_elite)+length(tweet_conserv_elite),length(tweet_conserv_elite),length(tweet_liberal_elite)) 
  
                 total_tweet1=c(sum(volume[1:18,4]),sum(volume[1:18,2]),sum(volume[1:18,3])) 
                total_tweet2=c(sum(volume[19:37,4]),sum(volume[19:37,2]),sum(volume[19:37,3])) 
                 total_tweet3=c(sum(volume[38:55,4]),sum(volume[38:55,2]),sum(volume[38:55,3]))  
  
                tweetsbytop10_perc=c(sum(sort(table(c(id1,id2)),decreasing = T)[1:round(0.1*length(c(table(id1),table(id2))))])/(length(tweet_conserv)+length(tweet_liberal))
                                      sum(sort(table(id1),decreasing = T)[1:round(0.1*length(table(id1)))]) / length(tweet_conserv)
                                      sum(sort(table(id2),decreasing = T)[1:round(0.1*length(table(id2)))]) / length(tweet_liberal)
                 tweetsbytop1_perc=c(sum(sort(table(c(id1,id2)),decreasing = T)[1:round(0.01*length(c(table(id1),table(id2))))])/(length(tweet_conserv)+length(tweet_liberal)),
                                       sum(sort(table(id1),decreasing = T)[1:round(0.01*length(table(id1)))]) / length(tweet_conserv),
                                       sum(sort(table(id2),decreasing = T)[1:round(0.01*length(table(id2)))]) / length(tweet_liberal))



