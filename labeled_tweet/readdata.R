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
