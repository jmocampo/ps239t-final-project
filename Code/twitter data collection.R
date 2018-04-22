library(rtweet)
library(dplyr)
library(tidyr)

create_token(app = "joresearch", consumer_key = "xCgUHqfopAHLhsKOxZ0uSXRMy", 
             consumer_secret = "3Rnohkha5PgQfByRTNP1JnTwaAaA9Oo11VSbuwRszXIaKgznBr")

search_tweets()
# check rate limits -------------------------------------------------------
limits <- rate_limit()
limits
limits[limits$query=="statuses/retweeters/ids",]
limits[limits$query=="application/rate_status",]
limits[limits$query=="statuses/user_timeline",]

# some functions in rtweets -----------------------------------------------

#stream_tweets()
#get_retweeters()
#search_tweets() #only 6 to 9 days

# target tweet search terms -----------------------------------------------

happywords <- "happy OR happiness OR joy OR joyous OR great OR good OR amazing OR elated 
OR ecstatic OR cheerful OR cheery OR merry OR merriment OR love OR loved OR cheers OR hurray 
OR yay OR fantastic OR wonderful OR awesome"

happyTweets <- search_tweets(happywords, n = 1000, type = "popular", include_rts = F, lang = "en")

#save the potential target tweets
write_as_csv(happyTweets, "happyTweets.csv")

##like this tweet, status ID 980918677083058176
happyTweets[happyTweets$status_id == 980918677083058176, ]$user_id
happyTweets[happyTweets$status_id == 980918677083058176, ]$favorite_count
happyTweets[happyTweets$status_id == 980918677083058176, ]$retweet_count


# identify retweeters -----------------------------------------------------

retweeters <- get_retweeters(status_id = "980918677083058176", n = 100)
retweeters$user_id[2]


# get retweeters tweet history --------------------------------------------

temp <- get_timeline(retweeters$user_id[1], n=10000, check = T, retryonratelimit=T)
for(i in 16:90){
  a <- paste("currently processing user #",i,sep="")
  print(a)
  temp <- rbind(temp, get_timeline(retweeters$user_id[i], n = 10000, check = T, retryonratelimit=T))
  b <- paste("finished processing user #",i,sep="")
  print(b)
  rm(a,b)
}

table(temp$screen_name)
class(temp)

temp$plainText <- plain_tweets(temp$text)
tweets <- temp
rm(temp)

##I'll need to collect tweets that occur after the tweets I just collected using since_id and the same function from before
sinceid <- max(tweets$status_id)

temp <- get_timeline(retweeters$user_id[1], n = 10000, check = T, since_id = sinceid)
for(i in 2:90){
  temp <- rbind(temp, get_timeline(retweeters$user_id[i], n = 10000, check = T, since_id = sinceid))
}



