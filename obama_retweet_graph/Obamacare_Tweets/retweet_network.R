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

weight<-table(edge_list$retweet_from)
weight<-as.data.frame(weight,stringsAsFactors = F);names(weight)<-c("id","weight")
head(weight)
vertices_list<-merge(vertices_list,weight,all.x=T)

vertices_list[is.na(vertices_list$weight),3]<-0
head(vertices_list)  

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



#### network graph ####  
library(igraph)
net<-graph.data.frame(d=edge_list,vertices = vertices_list,directed=T)
V(net)$name
fruch = layout.fruchterman.reingold(net)
circle=layout.circle(net)

png(filename = "retweet_graph.png")
par(mar=c(0,0,0,0))
plot(net,layout=circle
     ,vertex.label=NA
     ,vertex.size=0.1
     ,edge.arrow.size=.4)

# add title
title("\nTweets with 'obamacare':  Who retweets whom",
      cex.main=1, col.main="gray95",family="mono") 
dev.off()

#TEST
e<-sample_n(edge_list, 100)
id<-as.character(union(e$post,e$retweet_from))
v<-merge(id,vertices_list,all.x = T)

net<-graph.data.frame(e,v,directed = T)
par(mar=c(0,0,0,0))
plot(net,
     vertex.label=NA,
     vertex.size=V(net)$weight/2027 +1,
     edge.arrow.size=.3)

