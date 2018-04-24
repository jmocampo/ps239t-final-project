install.packages("dplyr","lubridate")
library(dplyr)
library(lubridate)

#so, in order for anything to work, I need to use the original tweet objects. I saved my environment at this stage of data collection, and so I'm loading the twitter objects below
load("data/03-cleaningEnv.Rdata")

# make plaintext csv for liwc ---------------------------------------------
tweetstext <- tweets$plainText
write.csv(tweetstext, "data/04-tweetstext2.csv")

#separately, I loaded the texts into LIWC for analysis. Only later did I realize tweets and temp actually have many overlapping tweets (because both times I ran get_timeline, I got the users entire tweet history)

#so later on in this code, I remove duplicates.

# clean for analysis ------------------------------------------------------
#find the primary retweet for each person
targetTweet <- "Don't be afraid to say no to things, do things that keep ur confidence high"

#sanity check: does the target retweet appear 90 times (once for each user)?
table(grepl(targetTweet, tweets$plainText)) #first indication I have duplicates, 175 targets

##find time stamp of target tweet for each person
tweets$created_at[grepl(targetTweet, tweets$plainText)] #useful later on for cleaning
tweets$status_id[grepl(targetTweet, tweets$plainText)]

# identify each tweet as pre or post target tweet -------------------------

##next, I should identify whether each tweet is before or after the target retweet for each individual. 

#this line gets a count of tweets for each user. It takes awhile to run and isn't particularly necessary.
tmp <- with(tweets,
            by(tweets, screen_name,
               function(x) sum(x$status_id < tweets$status_id[grepl(targetTweet, tweets$plainText)])))
tmp


#dplyr form
tweets <- tweets %>% group_by(user_id)

#make column that identifies that users target status_id

targetstatus_id <- tweets$status_id[grepl(targetTweet, tweets$plainText)] #collect target status by user
names(targetstatus_id) <- unique(tweets$user_id) #name each target status by its associated user
tweets$target_id <- "null" #initialize the target_id column
tweets$target_id <- targetstatus_id[as.character(tweets$user_id)] #index target id by user
tweets$prepost <- "null" #initialize the pre-post column
tweets$prepost <- ifelse(tweets$status_id > tweets$target_id, "after", "before") #assign after to tweets after target id

table(tweets$prepost)

tweets <- ungroup(tweets)

# load liwc output and save to tweets -------------------------------------------------------

#load up LIWC output 1 and LIWC output 2 (pre target tweet and post target tweet)
#when i originally did it, I made two separate files since it was over the course of two weeks. But you could also just load up a single LIWC output file
LIWC <- read.csv("data/05-LIWC2015-Results-(tweetstext2).csv", header = T)

#add LIWC columns to the data
mydata <- cbind(tweets, LIWC)
rm(LIWC)

#there's some columns that don't need to exist in the set
mydata$A <- NULL
mydata$B <- NULL

# Prepare the data ----------------------------------------------------------
#clean up the work space
rm(user, tweetstext, tmp, i, happywords, tweets, retweeters, LIWC, happyTweets, tweet, targetstatus_id, LIWC1, LIWC2,temp,limits)

#treat some columsn as factors
factorfycolumns <- c("status_id","user_id","screen_name","prepost")
mydata[factorfycolumns] <- lapply(mydata[factorfycolumns], factor)

#set the reference time to "before" the target tweet
mydata$prepost <- relevel(mydata$prepost, ref = "before")

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
length(mydata$status_id) #there are 347,811 tweets
length(unique(mydata$status_id)) #but only 187,668 unique tweet IDs... those are probably duplicates!

#remove duplicated status_ids
mydata <- mydata[!duplicated(mydata$status_id),]

# add total tweets per person ---------------------------------------------

totTweets <- by(mydata, mydata$user_id, nrow)
mydata$totaltweets <- as.numeric(totTweets[mydata$user_id])
rm(totTweets)


# remove target tweet -----------------------------------------------------
mydata$prepost[mydata$status_id==mydata$target_id] <- NA #don't want to treat the target tweet as positivity in the users tweet history

#trim out the non-English labeled tweets
mydata <- mydata[mydata$lang=="en",]
