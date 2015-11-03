#https://docs.google.com/spreadsheets/d/1YbL62mtxF-vRbRvDjIZxS9waxc2QaIWPxcHz9sTo4Ng/edit?ts=562454d1#gid=1708562460
options(java.parameters = "-Xmx4000m")#solve the memory problem
library(xlsx)
library(dplyr)
iden<-read.xlsx2("~/R_twitter_politics/labeled_tweet/elite/sorted_unique_users.xlsx",1,
                 colIndex = c(3,5:9,12)
                 ,colClasses =c("character","character","character","logical"
                                ,"numeric","numeric","character"),stringsAsFactors=F)
load("~/Downloads/tweet_conserv.RData")
load("~/Downloads/tweet_liberal.RData")


load("/Users/dcao28/R_twitter_politics/labeled_tweet/C/volume_conserv.RData")
load("/Users/dcao28/R_twitter_politics/labeled_tweet/L/volume_liberal.RData")

###############table 1 ################## 
volume<-cbind(volume_conserv$Freq,volume_liberal$Freq,volume_conserv$Freq+volume_liberal$Freq)
head(volume)
head(iden)
tweet_conserv[[123]]$retweeted_status

#conserv retweet tweets percentage
total_id<-lapply(tweet_conserv,FUN = function(x) x$retweeted_status$id_str)
null_loc<-sapply(total_id,FUN=function(x) is.null(x))
(length(null_loc)-sum(null_loc))/length(null_loc)

#liberal retweet tweets percentage
total_id<-lapply(tweet_liberal,FUN = function(x) x$retweeted_status$id_str)
null_loc<-sapply(total_id,FUN=function(x) is.null(x))
(length(null_loc)-sum(null_loc))/length(null_loc)

#convert average retweet per tweet  
ret_num1<-lapply(tweet_conserv,FUN = function(x) x$retweet_count)
sum(unlist(ret_num1))/length(tweet_conserv)


#liberal average retweet per tweet  
ret_num2<-lapply(tweet_liberal,FUN = function(x) x$retweet_count)
sum(unlist(ret_num2))/length(tweet_liberal)  

(sum(unlist(ret_num1))+sum(unlist(ret_num2)))/(length(tweet_liberal)+length(tweet_conserv))

# conserv mention percentage  
total_id<-lapply(tweet_conserv,FUN = function(x) x$entities$user_mentions)
null_loc1<-sapply(total_id,FUN=function(x) is.null(unlist(x)))
(length(null_loc1)-sum(null_loc1))/length(null_loc1)

# liberal mention percentage  
total_id<-lapply(tweet_liberal,FUN = function(x) x$entities$user_mentions)
null_loc2<-sapply(total_id,FUN=function(x) is.null(unlist(x)))
(length(null_loc2)-sum(null_loc2))/length(null_loc2)

((length(null_loc1)-sum(null_loc1))+(length(null_loc2)-sum(null_loc2)))/(length(null_loc1)+length(null_loc2))


# converv word average  
coun<-function(x){
  sapply(gregexpr("\\W+", x$text), length) + 1
}

word_len<-sapply(tweet_conserv, FUN = coun)
mean(word_len)

# liberal word average  
word_len2<-sapply(tweet_liberal, FUN = coun)
mean(word_len2)

mean(c(word_len,word_len2))

# conserv url percentage  
total_id<-lapply(tweet_conserv,FUN = function(x) x$entities$urls)
null_loc1<-sapply(total_id,FUN=function(x) is.null(unlist(x)))
(length(null_loc1)-sum(null_loc1))/length(null_loc1)

# liberal url percentage  
total_id<-lapply(tweet_liberal,FUN = function(x) x$entities$urls)
null_loc2<-sapply(total_id,FUN=function(x) is.null(unlist(x)))
(length(null_loc2)-sum(null_loc2))/length(null_loc2)

((length(null_loc1)-sum(null_loc1))+(length(null_loc2)-sum(null_loc2)))/(length(null_loc1)+length(null_loc2))



# conserv top10uses's tweet  
id1<-sapply(tweet_conserv, function(x) x$user$id_str)  
sort(table(id1),decreasing = T)[1:(0.1*length(id1))]

id2<-sapply(tweet_liberal, function(x) x$user$id_str)  
sort(table(id2),decreasing = T)[1:(0.1*length(id2))]

sort(table(c(id1,id2)),decreasing = T)[1:(0.1*length(c(id1,id2)))]


des<-data.frame(user_num=c(nrow(iden), nrow(filter(iden,ideology=="C")),nrow(filter(iden,ideology=="L"))) 
           , followers_num= c(sum(iden$followers_count),sum(filter(iden,ideology=="C")$followers_count),sum(filter(iden,ideology=="L")$followers_count))
           , following_num= rep(NA,3)
           , total_tweets=c(length(tweet_conserv)+length(tweet_conserv),length(tweet_liberal),length(tweet_liberal)) 
           , total_tweet1=c(sum(volume[1:18,3]),sum(volume[1:18,1]),sum(volume[1:18,2])) 
           , total_tweet2=c(sum(volume[19:37,3]),sum(volume[19:37,1]),sum(volume[19:37,2])) 
           , total_tweet3=c(sum(volume[38:55,3]),sum(volume[38:55,1]),sum(volume[38:55,2]))  
           , retweets_perc=c(44.20,43.34,45.58)
           , retweets_aver=c(46,38,58) 
           , mentions_per= c(65.83,65.23,66.79)
           , word_aver=c(19.55,19.35,19.89) 
           , URL_perc= c(50.93,51.17,50.55)
           , elite_perc=c((536+537)/nrow(iden),536/nrow(filter(iden,ideology=="C")),537/nrow(filter(iden,ideology=="L")))
           ,tweetsbytop10_perc=c(sum(sort(table(c(id1,id2)),decreasing = T)[1:round(0.1*length(c(table(id1),table(id2))))])/(length(tweet_conserv)+length(tweet_liberal))
                                 ,sum(sort(table(id1),decreasing = T)[1:round(0.1*length(table(id1)))])/length(tweet_conserv)
                                 ,sum(sort(table(id2),decreasing = T)[1:round(0.1*length(table(id2)))])/length(tweet_liberal)) )


View(des)  

#################table 3################  
library(dplyr)
## all the three time
weight_c<-read.csv("/Users/CDX/R_twitter_politics/labeled_tweet/C/conserv\ retweet\ weight.csv",header = T,stringsAsFactors = F)
weight_l<-read.csv("/Users/CDX/R_twitter_politics/labeled_tweet/L/liberal\ retweet\ weight.csv",header = T,stringsAsFactors = F)
weight_l<-select(weight_l,c(id,name,weight))#the name is screen_name
weight_l<-rename(weight_l,weight1=weight)
weight_c<-select(weight_c,c(id,name,weight))
weight_c<-rename(weight_c,weight2=weight)

total_weight<-merge(weight_c,weight_l)
head(total_weight)
total_weight<-mutate(total_weight,weight=weight1+weight2)
total_weight<-select(total_weight,c(name,id,weight))
head(total_weight)

ide<-iden[c(2,1)]#screen_name,id  
ide<-rename(ide,name=screen_name)
head(ide)

total_weight<-merge(ide,total_weight)
sum(duplicated(total_weight$name))
tail(total_weight)

total_weight<-arrange(total_weight,desc(weight))
total_weightl<-filter(total_weight,ideology=="L")
total_weightc<-filter(total_weight,ideology=="C")

weight10_c<-total_weightc[1:10,]
weight10_l<-total_weightl[1:10,]

View(weight10_l)
View(weight10_c)

## tweet_total
load("~/Downloads/obamacare_labeled.RData")# The result list is saved
obamacare<-obamacare_labeled
id<-sapply(obamacare, function(x) x$user$id_str)#key
id_freq<-table(id)  
rm(obamacare)
rm(obamacare_labeled)

tweet_total_c<-c()
for(i in seq(10)){
  tweet_total_c[i]<- id_freq[names(id_freq)==weight10_c$id[i]]
}
View(data.frame(tweet_total_c))

tweet_total_l<-c()
for(i in seq(10)){
  tweet_total_l[i]<- id_freq[names(id_freq)==weight10_l$id[i]]
}
View(data.frame(tweet_total_l))

## total followers/text  check(iden)
head(iden)
text<-c()
for(i in 1:length(weight10_l$id)){
  loc<-iden$screen_name==weight10_l$name[i]
  text[i]<-(iden$text[loc])
}
write.csv(data.frame(id=1:10,text),file = "text.csv")

## cross retweet  

cross_num<-c()
for(i in 1:10){
  cross<-function(x){
    if(is.null(x$retweeted_status)) return(FALSE)
    else{
      if(x$retweeted_status$user$id_str==weight10_c$id[i]) return(TRUE)
    }
  }
  
  a<-sapply(tweet_liberal,cross)
  cross_num[i]<-sum(unlist(a))
}  

View(data.frame(cross_num))  





##################
ideoOBAMA<-sapply(tweet_conserv, 
                  function(x) if(x$user$id_str == "11866582") return(x$ideology))
unlist(ideoOBAMA)
ideoOBAMA2<-sapply(tweet_liberal, 
                  function(x) if(x$user$id_str == "11866582") return(x$ideology))
unlist(ideoOBAMA2)
