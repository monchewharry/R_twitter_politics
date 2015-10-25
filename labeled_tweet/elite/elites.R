options(java.parameters = "-Xmx4000m")#solve the memory problem
library(xlsx)
library(dplyr)
iden<-read.xlsx2("sorted_unique_users.xlsx",1,
                 colIndex = c(3,5:9)
                 ,colClasses =c("character","character","character","logical"
                                ,"numeric","numeric"),stringsAsFactors=F)
#faster than read.xlsx
head(iden)
summary(select(iden,c(followers_count,retweet_count)))

## verified elites
elite_t<-filter(iden,verified==TRUE);dim(elite_t)
elite_t<-rename(elite_t,Username=screen_name,Real_Name=name) 
elite_t<-arrange(elite_t,desc(followers_count))
dim(elite_t)
write.csv(elite_t,file = "elite_t.csv")

######
origin<-read.xlsx2("labeled elites.xlsx",1,colIndex =1:3,colClasses =rep("character",3),stringsAsFactors=F)
origin<-origin[-c(591:1002),]
dim(origin)
origin<-rename(origin,Real_name=Real.Name,Username=Userame)

head(origin)
head(elite_t)
elite_t<-select(elite_t,c(Username,Real_Name,ideology,followers_count))

head(elite_t)
head(origin)
mergenew<-merge(origin,elite_t,by="Username")
write.csv(mergenew,file = "mergenew.csv")


x <- data.frame(k1 = c(NA,NA,3,4,5), k2 = c(1,NA,NA,4,5), data = 1:5)
y <- data.frame(k1 = c(NA,2,NA,4,5), k2 = c(NA,NA,3,4,5), data = 1:5)
merge(x, y, by = c("k1","k2")) # NA's match
merge(x, y, by = "k1") # NA's match, so 6 rows
merge(x, y, by = "k2", incomparables = NA) # 2 rows

# > sum(iden[iden$verified==T,]$ideology=="L")
# [1] 229
# > sum(iden[iden$verified==T,]$ideology=="C")
# [1] 362
#############unverified elites################  

elite_f<-filter(iden,verified==F);dim(elite_f)
elite_f<-rename(elite_f,Username=screen_name,Real_Name=name) 
elite_f<-arrange(elite_f,desc(followers_count))
sum(iden[iden$verified==F,]$ideology=="L")
sum(iden[iden$verified==F,]$ideology=="C")


elite_f_l<-filter(elite_f,ideology=="L" 
                  & elite_f$followers_count > quantile(elite_f$followers_count, 0.97))
dim(elite_f_l)
elite_f_c<-filter(elite_f,ideology=="C" 
                  & elite_f$followers_count > quantile(elite_f$followers_count, 0.985))
dim(elite_f_c)

non_verified<-rbind(elite_f_c,elite_f_l)
write.csv(non_verified,file = "elite_f.csv")







