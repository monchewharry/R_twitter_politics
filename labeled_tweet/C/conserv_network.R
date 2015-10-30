setwd("/Users/CDX/R_twitter_politics/labeled_tweet/C")
library(igraph)
library(dplyr)
#load("tweet_conserv.RData")# The data list is saved

#### make edge list ####
edge<-function(x){
  if(identical(NULL,x$retweeted_status)) return(c(NA,NA))
  else{
    return(c(x$user$id,x$retweeted_status$user$id))
  }
}

edge_list<-t(sapply(tweet_conserv,FUN = edge))
colnames(edge_list)<-c("post","retweet_from")
edge_list<-edge_list[!is.na(edge_list[,1]),]
edge_list<-as.data.frame(edge_list,stringsAsFactors = F)

weight<-table(edge_list$retweet_from)
weight<-as.data.frame(weight,stringsAsFactors = F);names(weight)<-c("id","weight")


#### make vertices list ####
vertices_list<-t(sapply(tweet_conserv,FUN = function(x) c(x$user$id_str,x$user$screen_name)))

vertices2<-function(x){
  if(!identical(NULL,x$retweeted_status)) return(c(x$retweeted_status$user$id_str,x$retweeted_status$user$screen_name))
  else return(c(NA,NA))
}

vertices_list2<-t(sapply(tweet_conserv,FUN = vertices2))
vertices_list2<-vertices_list2[!is.na(vertices_list2[,1]),]


vertices_list<-rbind(vertices_list,vertices_list2)
colnames(vertices_list)<-c("id","name")
vertices_list<-as.data.frame(vertices_list,stringsAsFactors = F)

vertices_list<-distinct(select(vertices_list,id,name)) 
vertices_list<-vertices_list[!duplicated(vertices_list$id),]#delete the duplicated item

setdiff(union(edge_list$post,edge_list$retweet_from),vertices_list$id)#varify the completeness

#add attr weight
vertices_list<-merge(vertices_list,weight,all.x=T)
vertices_list[is.na(vertices_list$weight),3]<-0# set no weight as 0
vertices_list<-(arrange(vertices_list,desc(weight)))
write.csv(vertices_list,file="conserv retweet weight.csv" )


#### network graph ####  
suppressMessages(library(igraph))

#deal with the name strings
library(stringr)
vertices_list$name[is.na(str_length(vertices_list$name))]#multi-byte strings  
head(iconv(vertices_list$name, "latin1", "ASCII", sub=""))  
vertices_list$name<-iconv(vertices_list$name, "latin1", "ASCII", sub="")
#simplify the vertices:delete isolated point
v<-vertices_list[vertices_list$id %in% c(edge_list$post,edge_list$retweet_from),]

# GRAPH OBJECT
#layout

net<-graph.data.frame(d=edge_list,v=v ,directed=T) 
fruch = layout.fruchterman.reingold(net)
circle=layout.circle(net)


png(filename = "image/conserv retweet_graph2.png",width=1400,height=850)
par(mar=c(0,0,0,0))
plot(net, edge.arrow.size=.2, edge.color="orange"
     ,vertex.label = ifelse(V(net)$weight>1000,V(net)$name,NA)
     ,vertex.color="red"
     ,vertex.size=ifelse(V(net)$weight>1000,1.2,0.2)
     ,vertex.frame.color=rgb(.25, .5, .3,alpha=ifelse(V(net)$weight>1000,1,0.2))
     ,vertex.label=NA, vertex.label.color="black") 

# add title
title("\nconserv tweeters:  Who retweets whom",
      cex.main=1, col.main="black",family="mono") 

dev.off()


