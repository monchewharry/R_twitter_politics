options(java.parameters = "-Xmx4000m")#solve the memory problem
library(xlsx)
library(dplyr)
iden<-read.xlsx2("sorted_unique_users.xlsx",1
                 ,colClasses =c("character","character","character","character"
                                ,"logical","numeric","numeric","numeric"))
#faster than read.xlsx
head(iden)
class(iden$verified)
iden<- select(iden,-created_at)
summary(select(iden,-(id:name)))
## elites: media accounts, politicians, organizations, pundits, and celebrities  
elite<-filter(iden,
              verified==TRUE & followers_count>658)
elite<-rename(elite,Username=screen_name,Real_Name=name) 

write.csv(elite,"elite.csv") 

