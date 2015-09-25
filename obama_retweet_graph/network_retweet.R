library(twitteR)
library(igraph)
library(stringr)
library(base64enc)

#### twitter api ##### 
Consumer_key<- "xxxx"
Consumer_Secret <-"xxxx"
Access_Token<- "xxxxx"
Access_Secret <-	"xxxx"

setup_twitter_oauth(Consumer_key,Consumer_Secret,Access_Token,Access_Secret)

#### Step 2: Harvest tweets about "Obamacare" ####

# obamacare_tweets = searchTwitter("Obamacare", n=500, lang="en") # tweets in english containing "obamacare"
# save(obamacare_tweets,file="obamacare_tweets.RData")
load("obamacare_tweets.RData")
obamacare_txt = sapply(obamacare_tweets, function(x) x$getText())# get text


#### Step 3: Identify retweets #####

grep("(RT|via)((?:\\b\\W*@\\w+)+)", obamacare_tweets, ignore.case=TRUE, value=TRUE) # regular expressions to find retweets

# which tweets are retweets
rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)", 
                   obamacare_txt, ignore.case=TRUE)

# show retweets (these are the ones we want to focus on)
obamacare_txt[rt_patterns] 


#### Step 4: Collect who retweeted and who posted #####
# We'll use these results to form an edge list in order to create the graph
# create list to store user names
who_retweet = as.list(1:length(rt_patterns))
who_post = as.list(1:length(rt_patterns))

# for loop
for (i in 1:length(rt_patterns))
{ 
# get tweet with retweet entity
twit = obamacare_tweets[[rt_patterns[i]]]
# get retweet source 
poster = str_extract_all(twit$getText(),
"(RT|via)((?:\\b\\W*@\\w+)+)") 
#remove ':'
poster = gsub(":", "", unlist(poster)) 
# name of retweeted user
who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE) 
# name of retweeting user 
who_retweet[[i]] = rep(twit$getScreenName(), length(poster)) 
}

# unlist
who_post = unlist(who_post)
who_retweet = unlist(who_retweet)


#### Create an edglist #### 
retweeter_poster = cbind(who_post,who_retweet)# edge list matrix

#rt_graph = graph.edgelist(retweeter_poster,directed = T)# generate graph

#ver_labs = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))# get vertex names

#### create an vertices list showing the most popular poster####
length(unique(as.character(retweeter_poster)))# total users
length(unique(retweeter_poster[,1]))# be retweeted 
vertices_weight<-data.frame(table(retweeter_poster[,1]))
names(vertices_weight)<-c("users","weight")
onlyretweet<-setdiff(unique(retweeter_poster[,2]),unique(retweeter_poster[,1]))# not be retweeted
vertices_weight<-rbind(vertices_weight,data.frame(users=onlyretweet,weight=0))

#### Step 6: Let's plot the graph ####

retweeter_poster<-as.data.frame(retweeter_poster)
net<-graph.data.frame(d=as.data.frame(retweeter_poster)
                      , vertices = vertices_weight, directed=T)
V(net)$weight

# choose some layout
fruch = layout.fruchterman.reingold(net)
circle=layout.circle(net)

png(filename = "obama_retweet_graph.png")
par(bg="gray15",mar=c(0,0,0,0))
plot(net,layout=fruch
     ,vertex.size=V(net)$weight*0.2+1
     ,vertex.color="red",
     ,vertex.label=ifelse(V(net)$weight>2,V(net)$name,NA)
     ,vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5)
     ,vertex.label.family="mono"
     ,edge.arrow.size=.4
     ,edge.color=hsv(h=.35, s=1, v=.7, alpha=0.4))
# add title
title("\nTweets with 'obamacare':  Who retweets whom",
      cex.main=1, col.main="gray95",family="mono") 
dev.off()

