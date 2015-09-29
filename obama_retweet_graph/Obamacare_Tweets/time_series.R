load("/Users/CDX/Google\ Drive/twitter_politics_data/obamacare.RData")# The data list is saved
str(obamacare[[100]])
obamacare[[100]]$created_at

#### build the volume time series####
x1 <- strptime(obamacare[[1]]$created_at
               ,format="%a %b %d %H:%M:%S %z %Y",tz = "UTC")#see ?strftime for %
time_stamp<-sapply(obamacare,FUN = function(x) return(as.POSIXct(x$created_at,
                                              format="%a %b %d %H:%M:%S %z %Y",
                                              tz = "UTC")))#origin "1970-01-01 GMT"
time_stamp<-time_stamp+(x1-time_stamp[1])#
time_stamp<-strftime(time_stamp, format="%D")#day/month/year
time_index<-time_stamp#copy

(volume<-data.frame(table(time_index)))
volume$time_index<-as.character(volume$time_index)

#### volume plot #####
library(quantmod)
tweet_volume<-as.xts(volume$Freq,as.Date(volume$time_index,format = "%m/%d/%y"))
png("tweet volume1.png")
nf <- layout(matrix(c(1,1,1,2,3,4),2,3,byrow = TRUE), c(1,1,1), c(2,1), TRUE)

plot(tweet_volume,main="tweet volume over 3 periods")
plot(tweet_volume["/2012-04-03"],main="1st period")
plot(tweet_volume["2012-06-19::2012-07-07"],main="2nd period")
plot(tweet_volume["2012-10-22::2012-11-08"],main="3rd peirod")
dev.off()

png("tweet volume2.png")
chartSeries(tweet_volume,type="line")
dev.off()

png("volume ~2012-04-03.png")
chartSeries(tweet_volume,type="line",
            subset="/2012-04-03")
dev.off()
png("volume 2012-06-19~2012-07-07.png")
chartSeries(tweet_volume,type="line",
            subset="2012-06-19::2012-07-07")
dev.off()
png("volume 2012-10-22~2012-11-08.png")
chartSeries(tweet_volume,type="line",
            subset="2012-10-22::2012-11-08")
dev.off()

