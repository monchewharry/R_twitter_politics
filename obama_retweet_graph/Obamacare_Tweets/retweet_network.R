library(igraph)
library(stringr)
library(dplyr)

load("/Users/dcao28/Downloads/obamacare.RData")# The result list is saved
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

#### convert data into data.frame by package: data.table#####  
install.packages("data.table")
library(data.table)

#### edge ####
edge_list<-list(id_post=NULL,id_retweet=NULL)
i=0
while(T){
  i=i+1
  if(identical(NULL,obamacare[[i]]$retweeted_status)) next
  else{
    edge_list<-rbind(edge_list,
                            c(obamacare[[i]]$user$id
                              ,obamacare[[i]]$retweeted_status$user$id))
  }
  
  if(i==length(obamacare)) break
}
edge_list<- edge_list[-1,]

load("edge_list.RData")

#### vertices ####
vertices_list<-list(id=NULL,name=NULL,weight=NULL)  
i=0
system.time(while(T){
  i=i+1
  vertices_list<-rbind(vertices_list
                       ,c(obamacare[[i]]$user$id
                          ,obamacare[[i]]$user$name
                          ,NA))
  if(i==length(obamacare)) break
}
)
i*3
save(vertices_list,file = "vertices_list.RData")


