## since the old oauth way is not valid anymore and the function twListToDF() is rewritten, 
## I revise the R script by JungHwan Yang to get certification to use twitter api. 
## Any code commented is no longer needed.

#### one user data #####
#--- 1. Install packages and set working directory

library(twitteR);

# Set up a working directory
setwd("/Users/CDX/R_workspace/R_project/R_twitter_politics")

# #--- 2. Getting a curl certification
# download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#--- 3. Setting the certification from Twitter(dev.twitter.com/apps)

consumerKey <- "xxxx"

consumerSecret <- "xxxx"

access_token <- "xxxx"

access_secret <- "xxxx"

#--- 4. Creating the authorization object by calling function OAuthFactory

setup_twitter_oauth(consumer_key=consumerKey, consumer_secret=consumerSecret
                                , access_token=access_token, access_secret=access_secret)





# We got all we need now. Let's mining Twitter!!!
# 
# #--- 1. Load your credential
# 
# load("twitteR_credentials")
# registerTwitterOAuth(twitCred)



#--- 2. Get a particular user's 100 most recent Twitter updates from Twitter.

timeline.User1 <- userTimeline('@Dan__Cao',n=100, maxID=NULL, sinceID=NULL)


#--- 3. Save collected data in a dataframe.

df.User1 <- twListToDF(timeline.User1)

# Press Enter or ESC if it asks further entry.


#--- 4. Extract the properties you want to collect (e.g., username, time, text, location, link, etc.).
# Here, I chose to save time, user ID, screen name, and tweet texts.

tweets.User1 <- df.User1[c("text", "created", "id", "screenName")]

#--- 5. Save as .csv file.
write.csv(tweets.User1, "file.User1.csv")


#### to collect data from many Twitter users #####

# Make sure you have a text file with Twitter usernames line by line (e.g., 'names.txt'). 

# You need to read usernames line by line and execute Tweet-collecting script within the loop. 

names <- file('names.txt', 'r') # Place 'names.txt' file under working directory
lines <- readLines(names, 3) # Specify number of usernames you have in 'names.txt' file
tweetsCombined <- data.frame (text=character(0), created=character(0), id=character(0), screenName=character(0))

for (i in 1:length(lines)){
  userTimeline <- userTimeline(lines[i],n=100, maxID=NULL, sinceID=NULL) # n='number of tweets you want to collect from each user'
  df.userTweets <- twListToDF(userTimeline)
  tweets <- df.userTweets[c("text", "created", "id", "screenName")] # Define properties you want to collect
  tweetsCombined <- rbind(tweetsCombined, tweets)
}
write.csv(tweetsCombined, "fileTweets.csv")
close(names)
