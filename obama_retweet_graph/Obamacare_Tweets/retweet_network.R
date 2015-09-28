library(igraph)
library(stringr)
library(dplyr)

load("/Users/CDX/Google\ Drive/twitter_politics_data/obamacare.RData")# The result list is saved
length(obamacare)
# data structure
str(obamacare[[101010]])
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



#### edge ####
edge<-function(x){
  if(identical(NULL,x$retweeted_status)) return(c(NA,NA))
  else{
    return(c(x$user$id,x$retweeted_status$user$id))
  }
}

edge_list<-t(sapply(obamacare,FUN = edge))
colnames(edge_list)<-c("post","retweet_from")
edge_list<-edge_list[!is.na(edge_list[,1]),]
edge_list<-as.data.frame(edge_list,stringsAsFactors = F)

#### vertices ####
vertices_list<-t(sapply(obamacare,FUN = function(x) c(x$user$id_str,x$user$name)))

vertices2<-function(x){
  if(!identical(NULL,x$retweeted_status)) return(c(x$retweeted_status$user$id_str,x$retweeted_status$user$name))
  else return(c(NA,NA))
}

vertices_list2<-t(sapply(obamacare,FUN = vertices2))
vertices_list2<-vertices_list2[!is.na(vertices_list2[,1]),]
head(vertices_list2)

vertices_list<-rbind(vertices_list,vertices_list2)
colnames(vertices_list)<-c("id","name")
vertices_list<-as.data.frame(vertices_list,stringsAsFactors = F)

library(dplyr)
vertices_list<-distinct(select(vertices_list,id,name)) 
vertices_list<-vertices_list[!duplicated(vertices_list$id),]#delete the duplicated item

setdiff(union(edge_list$post,edge_list$retweet_from),vertices_list$id)#varify the completeness

#weight<-table(edge_list$retweet_from)

#### network graph ####  
library(igraph)
net<-graph.data.frame(d=edge_list,vertices = vertices_list,directed=T)
V(net)$name
fruch = layout.fruchterman.reingold(net)
circle=layout.circle(net)

png(filename = "retweet_graph.png")
par(bg="gray15",mar=c(0,0,0,0))
plot(net,layout=circle
     ,vertex.label=V(net)$name
     ,vertex.size=0.2
     ,vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5)
     ,vertex.label.family="mono"
     ,edge.arrow.size=.4
     ,edge.color=hsv(h=.35, s=1, v=.7, alpha=0.4))
# add title
title("\nTweets with 'obamacare':  Who retweets whom",
      cex.main=1, col.main="gray95",family="mono") 
dev.off()

