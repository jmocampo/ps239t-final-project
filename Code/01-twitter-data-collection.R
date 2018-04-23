#setwd to the main github folder
setwd("C:/users/josep/onedrive/documents/GitHub/ps239t-final-project/")

#download required packages for data collection
install.packages("rtweet", "dplyr", "tidyr")

library(rtweet)
library(dplyr)
library(tidyr)

#create token authorizes rtweet to access twitter API using your credentials. I would rather not upload mine, so I left them blank. 
create_token(app = "", consumer_key = "", consumer_secret = "")

# check rate limits -------------------------------------------------------
#this is for checking rate limit info so I know how many calls I have left. 
limits <- rate_limit()
limits
limits[limits$query=="statuses/retweeters/ids",] #specific call function limits
limits[limits$query=="application/rate_status",]
limits[limits$query=="statuses/user_timeline",]

# some functions in rtweets -----------------------------------------------

#stream_tweets() runs continuously and collects tweets in real time
#get_retweeters() gets 100 retweeters of a tweet ID
#search_tweets() #only 6 to 9 days of the previous tweets, curated by twitter

# target tweet search terms -----------------------------------------------

#the search terms I'm entering to identify a good target tweet
happywords <- "happy OR happiness OR joy OR joyous OR great OR good OR amazing OR elated 
OR ecstatic OR cheerful OR cheery OR merry OR merriment OR love OR loved OR cheers OR hurray 
OR yay OR fantastic OR wonderful OR awesome"

#tweets sorted by popularity by twitter, excluding retweets.
#since the standard api only has the past 7 days, you won't be able to recreate my search. But here is an example search
happyTweets <- search_tweets(happywords, n = 1000, type = "popular", include_rts = F, lang = "en")

#save the potential target tweets
write_as_csv(happyTweets, "data/happyTweetstemp.csv")

#i manually read the tweets. The real tweeet file is loaded in the next line
happyTweets <- read.csv("data/01-targettweets.csv")

##like this tweet, status ID 980918677083058176
#this line also won't work because the original object it worked on was a "tweet object" from rtweet. Once you save/load it as a csv, it changes the structure and the columns. 
happyTweets[happyTweets$status_id == 980918677083058176, ]$user_id 
happyTweets[happyTweets$status_id == 980918677083058176, ]$favorite_count #
happyTweets[happyTweets$status_id == 980918677083058176, ]$retweet_count


# identify retweeters -----------------------------------------------------

#this also can't recreate what I did since it only goes back 7 days.
retweeters <- get_retweeters(status_id = "980918677083058176", n = 100)

#i have the same problem here where I can reload the csv data, but it isn't the same thing as the original tweet object so my code below won't really work for the csv.

retweeters$user_id[2] #example retweeter

# get retweeters tweet history --------------------------------------------

#in a for-loop, I sequentially gather the retweeters tweet history. I meant only to get the past 7 days, but turns out get_timeline gives you everything.

#initialize the temporary tweet object to store sequential twitter users
temp <- get_timeline(retweeters$user_id[1], n=10000, check = T, retryonratelimit=T)

#for users 2 thru 90, get their tweets and add them to the tweet object
#If the process gets interrupted by rate-limit, just re-run it from the last added twitter user to 90.
for(i in 2:90){
  #let the user know what twitter user is being processed
  a <- paste("currently processing user #",i,sep="")
  print(a)
  #rbind the previous collected tweets and the next twitter id
  temp <- rbind(temp, get_timeline(retweeters$user_id[i], n = 10000, check = T, retryonratelimit=T))
  #let the user know the user was processed successfully
  b <- paste("finished processing user #",i,sep="")
  print(b)
  #remove message a and message b
  rm(a,b)
}

#check how many tweets each user has
table(temp$screen_name)

#convert tweets to plain text (strips out symbols)
temp$plainText <- plain_tweets(temp$text)
tweets <- temp
rm(temp)