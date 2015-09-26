library(RJSONIO)
#1w sample json data: whole one time
system("tar zxvf obamacare100000.JSON.zip")
con1<-file("obamacare 100000.JSON",open="r")
linn=readLines(con1)
close(con1)
obama10w<-vector("list",length(linn))
for (i in 1:length(linn)){
  obama10w[[i]]<-fromJSON(linn[i])
}  
#save(obama10w,file="obama10w.RData")
#load("obama10w.RData")# The result list is saved
length(obama10w)

#total json data: line by line
system("tar zxvf obamacare_sample_json.zip")#decompress
con <- file("obamacare sample json.json",open = "r")
obamacare<-vector("list")
i=0
while(T){
  i<-i+1
  temp<-readLines(con,1)
  if(length(temp)==0){
    print("reach the end")
    break
  } else temp<-sub('.*\\t', '',temp, perl = TRUE)#get rid of the series ahead of json format
  obamacare[[i]]<-fromJSON(temp)
}
close(con)
#save(obamacare,file="obamacare.RData")
#load("obamacare.RData")# The result list is saved
length(obamacare)



