setwd("/Users/dcao28/R_twitter_politics/obama_retweet_graph/Obamacare_Tweets")
library(RJSONIO)
con<-file("obamacare 100000.JSON",open="r")
linn=readLines(con)
close(con)
obama10w<-vector("list",length(linn))
for (i in 1:length(linn)){
  obama10w[[i]]<-fromJSON(linn[i])
}



