library(dplyr)
library(lubridate)
setwd("c:/users/josep/dropbox/berkeley/Twitter/")
load("twits.Rdata")

tweets <- rbind(tweets, temp)

# make plaintext csv for liwc ---------------------------------------------
tweetstext <- tweets$plainText
write.csv(tweetstext, "tweetstext2.csv")

# clean for analysis ------------------------------------------------------
#find the primary retweet for each person
targetTweet <- "Don't be afraid to say no to things, do things that keep ur confidence high"

table(grepl(targetTweet, tweets$plainText)) #good. 85 matches, 1 for each person who retweeted the target Tweet

##find time stamp of target tweet for each person
tweets$created_at[grepl(targetTweet, tweets$plainText)]
tweets$status_id[grepl(targetTweet, tweets$plainText)]

# identify each tweet as pre or post target tweet -------------------------

##next, I should identify whether each tweet is before or after the target retweet for each individual. I remember there being some group-by functions that apply functions based on some grouping factor. I should ask about that. 

tmp <- with(tweets,
            by(tweets, screen_name,
               function(x) sum(x$status_id < tweets$status_id[grepl(targetTweet, tweets$plainText)])))
tmp


#dplyr form
tweets <- ungroup(tweets)
tweets <- tweets %>% group_by(user_id)

#make column that identifies that users target status_id

targetstatus_id <- tweets$status_id[grepl(targetTweet, tweets$plainText)] #collect target status by user
names(targetstatus_id) <- unique(tweets$user_id) #name each target status by its associated user
tweets$target_id <- "null" #initialize the target_id column
tweets$target_id <- targetstatus_id[as.character(tweets$user_id)] #index target id by user
tweets$prepost <- "null" #initialize the pre-post column
tweets$prepost <- ifelse(tweets$status_id > tweets$target_id, "after", "before") #assign after to tweets after target id

table(tweets$prepost)

# load liwc output and save to tweets --------------------------------------------------------
LIWC1 <- read.csv("LIWC2015 Results (tweetstext.csv).csv", header = T)
LIWC2 <- read.csv("LIWC2015 Results (tweetstext2.csv).csv", header = T)
LIWC <- rbind(LIWC1, LIWC2)
mydata <- cbind(tweets, LIWC)

mydata$A <- NULL
mydata$B <- NULL

# Prepare the data ----------------------------------------------------------
#clean up the work space
rm(user, tweetstext, tmp, i, happywords, tweets, retweeters, LIWC, happyTweets, tweet, targetstatus_id, LIWC1, LIWC2,temp,limits)

factorfycolumns <- c("status_id","user_id","screen_name","prepost")
mydata[factorfycolumns] <- lapply(mydata[factorfycolumns], factor)
mydata$prepost <- relevel(mydata$prepost, ref = "before")
#sapply(mydata, class) #check the format of each column
mydata$prepost

# split time columns --------------------------------------------------------
mydata$date <- as_date(mydata$created_at)
mydata$year <- year(mydata$created_at)
mydata$month <- month(mydata$created_at)
mydata$week <- week(mydata$created_at)
mydata$day <- day(mydata$created_at)
mydata$hour <- hour(mydata$created_at)
mydata$min <- minute(mydata$created_at)
mydata$sec <- second(mydata$created_at)


# remove repeat tweets ----------------------------------------------------
length(mydata$plainText)
length(unique(mydata$plainText))
length(mydata$status_id)
length(unique(mydata$status_id))
table(mydata$prepost)

mydata <- mydata[!duplicated(mydata$status_id),]

table(mydata$prepost)
length(unique(mydata$target_id))

# add total tweets per person ---------------------------------------------

totTweets <- by(mydata, mydata$user_id, nrow)
mydata$totaltweets <- as.numeric(totTweets[mydata$user_id])
rm(totTweets)


# remove target tweet -----------------------------------------------------
mydata$prepost[mydata$status_id==mydata$target_id] <- NA
