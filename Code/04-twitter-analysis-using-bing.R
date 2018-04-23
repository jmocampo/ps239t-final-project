
# sentiment analysis using bing ------------------------------------------
install.packages("tidytext", "tidyr", "psych","ggplot2","dplyr")
library(tidytext)
library(tidyr)
library(psych)
library(ggplot2)
library(dplyr)

# text to df --------------------------------------------------------------

#Change to character so when I run the dplyr and tidyr functions, they don't change
mydata$user_id <- as.character(mydata$user_id)
mydata$status_id <- as.character(mydata$status_id)

#create df of just the necessary info for text analysis
df <- as.data.frame(with(mydata, cbind(user_id, status_id, plainText)))

#add a row-number per each user to act as tweet identifier
#I didn't end up needing this haha
df <- df %>%
  group_by(user_id) %>%
  mutate(tweetnumber = row_number()) %>%
  ungroup()
head(df)

#convert things back to characters where needed
df$plainText <- as.character(df$plainText)

#tokenize the text
tweetTokens <- unnest_tokens(df,word, plainText)
tweetTokens[1:10,]

#example of word counts by sentiment
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

#frequency by person in the emotion
tweetTokens %>%
  group_by(user_id) %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

#calculate tweet sentiment
tweet_sentiment <- tweetTokens %>%
  inner_join(get_sentiments("bing")) %>%
  #bing assigns pos or neg to each token
  #then I can count for each user by status_id how much is pos or neg
  count(user_id, index = status_id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  #sentiment = positive - negative counts
  mutate(sentiment = positive - negative)

#initialize the sentiment vector in mydata
mydata$sentiment <- NA

#where the index ID in sentiments matches the status_id in mydata, add the sentiment to the row
mydata$sentiment[mydata$status_id%in%tweet_sentiment$index] <- tweet_sentiment$sentiment

class(mydata$sentiment)
describe(mydata$sentiment)


# plot --------------------------------------------------------------------

ggplot(mydata, aes(sentiment)) + 
  geom_density(lwd=2) +
  print(ggtitle("Density Plot of Sentiment in Tweets")) + 
  print(labs(x = "Sentiment Score", y = "Density")) +
  xlim(-5,5) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20,face = "bold", hjust=.5))

#percentage of data as 0, 1 and -1
sum(mydata$sentiment==0, na.rm = T)/length(na.omit(mydata$sentiment))
sum(mydata$sentiment==1, na.rm = T)/length(na.omit(mydata$sentiment))
sum(mydata$sentiment==-1, na.rm = T)/length(na.omit(mydata$sentiment))

#user1's time series of positive emotion in tweets across ENTIRE user history
ggplot(subset(mydata, user_id==users[1]), aes(y=sentiment, x=created_at)) + 
  geom_point(alpha=.5, cex = 2) +
  geom_smooth(lwd = 2) +
  print(ggtitle("Positive Emotion Time Series for User 1")) + 
  print(labs(x = "Time", y = "Positive Emotion Score")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 26,face = "bold", hjust=.5))

#zooms in to user 1 between Mar 20 and April 10 but doesn't change the gam fit
ggplot(subset(mydata, user_id==users[1]), aes(y=sentiment, x=created_at)) + 
  geom_point(alpha=.5, cex = 2) +
  geom_smooth(lwd = 2) +
  coord_cartesian(xlim = c(as.POSIXct("2018-04-10 13:40:52",tz="UTC"),
                                            as.POSIXct("2018-03-20 13:40:52",tz="UTC"))) +
  print(ggtitle("Positive Emotion Time Series for User 1")) + 
  print(labs(x = "Time", y = "Positive Emotion Score")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 26,face = "bold", hjust=.5))

#zooms in AND changes the gam fit
ggplot(subset(mydata, user_id==users[1]&mydata$year==2018&mydata$month>2), aes(y=sentiment, x=created_at)) + 
  geom_point(alpha=.5, cex = 2) +
  geom_smooth(lwd = 2) +
  print(ggtitle("Positive Emotion Time Series for User 1")) + 
  print(labs(x = "Time", y = "Positive Emotion Score")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 26,face = "bold", hjust=.5))

#zooms in, changes gam it AND adds the target tweet line (dashed)
ggplot(subset(mydata, user_id==users[1]&mydata$year==2018&mydata$month>2), aes(y=sentiment, x=created_at)) + 
  geom_vline(xintercept=mydata$created_at[mydata$status_id==mydata$target_id&mydata$user_id==users[1]], lwd = 2, lty=2, col = "dodgerblue")+
  geom_point(alpha=.5, cex = 2) +
  geom_smooth(lwd = 2) +
  xlim(as.POSIXct("2018-03-26 00:00:00",tz="UTC"),
       as.POSIXct("2018-04-10 13:40:52",tz="UTC")) +
  print(ggtitle("Positive Emotion Time Series for User 1")) + 
  print(labs(x = "Time", y = "Positive Emotion Score")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 26,face = "bold", hjust=.5))

#zooms in, changes gam it AND adds the target tweet line (dashed) for all users across time
#i zoomed into -5 to 5 on sentiment for better visual
ggplot(subset(mydata, mydata$year==2018&mydata$month>2), aes(y=sentiment, x=created_at)) + 
  geom_vline(xintercept=mydata$created_at[mydata$status_id==mydata$target_id&mydata$user_id==users[1]], lwd = 2, lty=2, col = "dodgerblue")+
  geom_jitter(height=.2,width=0,alpha=.2, cex = 2) +
  geom_smooth(lwd = 2) +
  xlim(as.POSIXct("2018-03-26 00:00:00",tz="UTC"),
       as.POSIXct("2018-04-10 13:40:52",tz="UTC")) +
  coord_cartesian(ylim=c(-5,5))+
  print(ggtitle("Positive Emotion Time Series for all Users")) + 
  print(labs(x = "Time", y = "Positive Emotion Score")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 26,face = "bold", hjust=.5))
#wow, you can actually see a potential fixed effect according to the gam fitted lin (slight bump post-tweet)

#to calculate average positivity per user, I used dplyr's group_by feature, summarise and mutate
emotionbyuser <- mydata %>% group_by(user_id) %>%
  summarise(mean_sentiment = mean(sentiment,na.rm=T), se_sentiment = sd(sentiment, na.rm=T)/sqrt(length(na.omit(sentiment)))) %>%
  mutate(se_sentiment_lower = mean_sentiment - 1.96*se_sentiment, se_sentiment_upper = mean_sentiment + 1.96*se_sentiment)
#give unique identifier to users
emotionbyuser$user_id <- as.factor(1:90)

#sort by biggest pos emo to smallest pos emo
emotionbyuser <- arrange(emotionbyuser, desc(mean_sentiment))
#create column to identify order
emotionbyuser$plotorder <- 1:90
#relevel to the plotorder
emotionbyuser$user_id <- factor(emotionbyuser$user_id, 
                                levels = emotionbyuser$user_id[order(emotionbyuser$plotorder)])

#plot for pos emo
ggplot(emotionbyuser, aes(x = user_id, y = mean_sentiment)) +
  geom_errorbar(aes(ymax=se_sentiment_upper, ymin=se_sentiment_lower, width=1)) + 
  geom_point(size = 3) +
  print(ggtitle("Mean & 95CI Positive Emotion by User")) + 
  print(labs(x = "User #", y = "Mean Sentiment")) +
  theme(axis.text=element_text(size=26),
        axis.title=element_text(size=26,face="bold"),
        plot.title = element_text(size = 28,face = "bold", hjust=.5),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

# models ------------------------------------------------------------------

#a null model averaging positive valence of tweets nested within users and time
m1 <- lmer(sentiment ~ 1 + (1|user_id/year/month/week), data = mydata)
summary(m1)

#potential fixed effect after target tweet
m1 <- lmer(sentiment ~ prepost + (1|user_id/year/month/week), data = mydata)
summary(m1) #a significant effect? noooooo wai
#but the errors are a bit funkily distributed

describe(residuals(m1)) #kurtosis = 1.07

#just out of curiosity, I subsetted my data to be just a two-week period

mydata.2weeks <- subset(mydata, mydata$created_at>as.POSIXct("2018-03-26 00:00:00",tz="UTC")
                        &mydata$created_at<as.POSIXct("2018-04-10 13:40:52",tz="UTC"))
library(lme4)
library(lmerTest)

#how do the models fit now?
m1 <- lmer(sentiment ~ prepost + (1|user_id), mydata.2weeks)
summary(m1) #nope

#so, I think the take-away is that yes, with a large enough sample size, there is a very, very small effect of retweeting some positive tweet on the average valence of future tweets (at least within the next week)