library(igraph)
library(stringr)
library(dplyr)
library(base64enc)

load("/Users/CDX/Google\ Drive/twitter_politics_data/obamacare.RData")# The result list is saved

# data structure
str(obamacare[[101010]])
attributes(obamacare[[101010]])
obamacare[[101010]]$text
obamacare[[101010]]$user$screen_name# @screen_name unique
obamacare[[101010]]$user$id
obamacare[[101010]]$user$name# may be duplicated
obamacare[[101010]]$retweeted_status$user$screen_name# retweet from
obamacare[[101010]]$retweeted_status$user$name   
obamacare[[101010]]$retweeted_status$user$id
# i=0
# while(T){
#   i=i+1
#   if(identical(obamacare[[i]]$user$screen_name,"mattklewis")
#      &identical(obamacare[[i]]$text,"If ObamaCare is overturned, George W. Bush's greatest legacy might be ... Roberts and Alito. http://t.co/y0WxraPM")){
#     print(i)
#   }else{if(i=length(obamacare)) break}
# }
str(obamacare[[100904]])
str(obamacare[[105505]])

obamacare[[100904]]$retweeted_status#original (=NULL)

retweeter_poster<-list(id_post=NULL,id_retweet=NULL)
i=0
while(T){
  i=i+1
  if(identical(NULL,obamacare[[i]]$retweeted_status)) next
  else{
    retweeter_poster<-rbind(retweeter_poster,
                            c(obamacare[[i]]$user$id,obamacare[[i]]$retweeted_status$user$id))
  }
  
  if(i==length(obamacare)) break
}
length(retweeter_poster)#many of them are original



