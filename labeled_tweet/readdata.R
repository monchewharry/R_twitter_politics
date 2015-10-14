con<-file("obamacare_labeled.json","r")
#install.packages("RJSONIO")
library(RJSONIO)
obamacare<-vector("list")
i=0
while(T){
  i<-i+1
  temp<-readLines(con,1)
  if(length(temp)==0){
    print("reach the end")
    break
  }
  obamacare[[i]]<-fromJSON(temp)
}
close(con)
obamacare_labeled<-obamacare
#save(obamacare_labeled,file="obamacare_labeled.RData")
load("obamacare_labeled.RData")# The result list is saved
obamacare_labeled[[1]]$ideology#new label field

#####split data by ideology #####  
tweet_liberal<-lapply(obamacare_labeled, FUN= function(x) if(identical(x$ideology,"L")) return(x))
null.loc<-sapply(tweet_liberal, FUN= function(x) if(is.null(x)) return(FALSE) else return(TRUE))
tweet_liberal<-tweet_liberal[null.loc]
#str(tweet_liberal[[1]])  

tweet_conserv<-obamacare_labeled[null.loc==FALSE] #find the conservation
#str(tweet_conserv[[1]])  

length(obamacare_labeled)==length(tweet_conserv)+length(tweet_liberal)

save(tweet_conserv,file = "tweet_conserv.RData")
save(tweet_liberal ,file = "tweet_liberal.RData")
