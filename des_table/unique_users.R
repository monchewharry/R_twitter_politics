load("~/Downloads/obamacare_labeled.RData")# The result list is saved
obamacare<-obamacare_labeled

created_at <- sapply(obamacare, with, created_at)
ideology <- sapply(obamacare, with, ideology)
id <- sapply(obamacare, with, id)#key

screen_name <- sapply(obamacare, with, user$screen_name)
name <- sapply(obamacare, with, user$name)
verified <- sapply(obamacare, with, user$verified)
followers_count <- sapply(obamacare, with, user$followers_count)
retweet_count <- sapply(obamacare, with, retweet_count)
listed_count <- sapply(obamacare, with, user$listed_count)
text <- sapply(obamacare, with, text)
retweet_user <- 
  sapply(obamacare, function(x) return(x$retweeted_status$user$screen_name))
retweet_user <- 
  sapply(retweet_user, function(x) ifelse(is.null(x), NA, x)) # list to vector

# get_re = function(x) return (x$retweeted_status$user$screen_name)
# retweet_user = sapply(obamacare, FUN = get_re)


d <- data.frame(created_at, ideology, id, screen_name, name, verified,
                followers_count, retweet_count, retweet_user, listed_count)
rm(created_at); rm(ideology); rm(id); rm(screen_name); rm(name); rm(verified);
rm(followers_count); rm(retweet_count); rm(retweet_user); rm(listed_count); rm(text)


d$time <- 
  as.Date(as.POSIXct(d$created_at, format = "%a %b %d %H:%M:%S %z %Y", tz = "UTC"),
          format = "%m %d")


# Created three different dataframes by time period

d1 <- subset(d,as.Date(d$time) >= "2012-03-17" & as.Date(d$time) <= "2012-04-03")
d2 <- subset(d,as.Date(d$time) >= "2012-06-19" & as.Date(d$time) <= "2012-07-07")
d3 <- subset(d,as.Date(d$time) >= "2012-10-22" & as.Date(d$time) <= "2012-11-08")
table(d$time)


# Create three different data lists by time period
oc1 <- obamacare[as.Date(as.POSIXct(d$created_at, 
                                    format = "%a %b %d %H:%M:%S %z %Y", tz = "UTC"),
                         format = "%m %d") >= "2012-03-17" &
                   as.Date(as.POSIXct(d$created_at,
                                      format = "%a %b %d %H:%M:%S %z %Y", tz = "UTC"),
                           format = "%m %d") <= "2012-04-03"]
oc2 <- obamacare[as.Date(as.POSIXct(d$created_at, 
                                    format = "%a %b %d %H:%M:%S %z %Y", tz = "UTC"),
                         format = "%m %d") >= "2012-06-19" &
                   as.Date(as.POSIXct(d$created_at,
                                      format = "%a %b %d %H:%M:%S %z %Y", tz = "UTC"),
                           format = "%m %d") <= "2012-07-07"]
oc3 <- obamacare[as.Date(as.POSIXct(d$created_at, 
                                    format = "%a %b %d %H:%M:%S %z %Y", tz = "UTC"),
                         format = "%m %d") >= "2012-10-22" &
                   as.Date(as.POSIXct(d$created_at,
                                      format = "%a %b %d %H:%M:%S %z %Y", tz = "UTC"),
                           format = "%m %d") <= "2012-11-08"]

rm(d1);rm(d2);rm(d3)

# sort data by several variable
# sorted.data_t1 <- 
#   d1[order(-d1$verified, -d1$followers_count, d1$name), ]
# sorted.data_t2 <- 
#   d2[order(-d2$verified, -d2$followers_count, d2$name), ]
# sorted.data_t3 <- 
#   d3[order(-d3$verified, -d3$followers_count, d3$name), ]
# 
# # export as csv
# write.csv(sorted.data_t1, "sorted.data.text_t1.csv")
# write.csv(sorted.data_t2, "sorted.data.text_t2.csv")
# write.csv(sorted.data_t3, "sorted.data.text_t3.csv")