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


#### Step 5: Create graph from an edglist #### 
# two column matrix of edges
retweeter_poster = cbind(who_post,who_retweet)# edge list matrix

# generate graph
rt_graph = graph.edgelist(retweeter_poster,directed = T)

# get vertex names
ver_labs = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))

#### create an vertices list ####
length(unique(retweeter_poster[,1]))
vertices_weight<-data.frame(table(retweeter_poster[,1]))
names(vertices_weight)<-c("who_post","weight")

#### Step 6: Let's plot the graph ####
# choose some layout
fruch = layout.fruchterman.reingold(rt_graph)
circle=layout.circle(rt_graph)

# plot
png(filename = "obama_retweet_graph.png")
par(mar=c(0,0,0,0))
plot(rt_graph, layout=fruch,
     vertex.color="gray25",
     vertex.size=10,
     vertex.label=NA,
     vertex.label.family="sans",
     vertex.shape="none",
     vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),
     vertex.label.cex=0.85,
     edge.arrow.size=0.8,
     edge.arrow.width=0.5,
     edge.width=3,
     edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5))
# add title
title("\nTweets with 'obamacare':  Who retweets whom",
      cex.main=1, col.main="red") 
dev.off()


#### Step 7: Let's try to give it a more bio look ####
# another plot
png(filename = "obama_retweet_graph2.png")
par(bg="gray15", mar=c(1,1,1,1))
plot(rt_graph, layout=glay,
vertex.color=hsv(h=.35, s=1, v=.7, alpha=0.1),
vertex.frame.color=hsv(h=.35, s=1, v=.7, alpha=0.1),
vertex.size=5,
vertex.label=ver_labs,
vertex.label.family="mono",
vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),
vertex.label.cex=0.85,
edge.arrow.size=0.8,
edge.arrow.width=0.5,
edge.width=3,
edge.color=hsv(h=.35, s=1, v=.7, alpha=0.4))
# add title
title("\nTweets with 'obamacare':  Who retweets whom",
cex.main=1, col.main="gray95", family="mono")
dev.off()
