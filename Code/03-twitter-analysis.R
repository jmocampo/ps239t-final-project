install.packages("lme4", "lmerTest", "psych", "ggplot2")
library(lme4)
library(lmerTest)
library(psych)
library(ggplot2)

#again, the issue of tweet objects v. csv's makes this more complicated. Here is my 
load("data/07-analysisenv.Rdata") #this should also be the same as the end of the environment once you complete file 02-twitter-prep-file

# Descriptive stats ---------------------------------------------------------
users <- unique(mydata$user_id)

#sample size (tweets per person)

#number of tweets
length(unique(mydata$user_id)) #twitter users
max(table(mydata$screen_name))
min(table(mydata$screen_name))
mean(table(mydata$screen_name)) #average number of tweets per user
sd(table(mydata$screen_name)) #sd of tweets per user

#a table of each user's number of tweets
tweetsPer <- as.data.frame(table(mydata$screen_name))
median(tweetsPer$Freq) #median number of tweets per user

hist(table(mydata$screen_name), main = "Distribution of # Tweets per Person", xlab = "Number of Tweets", col = "dodgerblue2", cex.lab = 2, cex.main = 2, cex.axis = 2)

ggplot(tweetsPer, aes(Freq)) + 
  geom_density(lwd=2) +
  print(ggtitle("Density Plot of Tweets Per User")) + 
  print(labs(x = "# of Tweets", y = "Density")) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20,face = "bold", hjust=.5))

# emotion and emotion plots -----------------------------------------------------------
describe(mydata$posemo)
describe(mydata$negemo)

sum(mydata$posemo==0)/nrow(mydata) #percent of 0-posemo-data
sum(mydata$negemo==0)/nrow(mydata) #percent of 0-negemo-data
sum(mydata$negemo==0&mydata$posemo==0)/nrow(mydata) #percent of 0-emotional data

##positive emotion density plots

#very skewwed density plot showing most of my data is 0
ggplot(mydata, aes(posemo)) + 
  geom_density(lwd=2) +
  print(ggtitle("Density Plot of Positive Emotions in Tweets")) + 
  print(labs(x = "Positive Emotion Score", y = "Density")) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20,face = "bold", hjust=.5))

#density of positive emotions excluding 0
ggplot(subset(mydata, posemo!=0), aes(posemo)) + 
  geom_density(lwd=2) +
  print(ggtitle("Density Plot of Positive Emotions in Tweets (Excluding 0's)")) + 
  print(labs(x = "Positive Emotion Score", y = "Density")) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20,face = "bold", hjust=.5))

#density of positive emotions excluding 0 and excluding 100
ggplot(subset(mydata, posemo!=0&posemo!=100), aes(posemo)) + 
  geom_density(lwd=2) +
  print(ggtitle("Density Plot of Positive Emotions in Tweets (Excluding 0's & 100's)")) + 
  print(labs(x = "Positive Emotion Score", y = "Density")) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20,face = "bold", hjust=.5))

##positive and negative emotion density plots

#noteworthy: most of my data falls along a positive emotion = negative emotion line. 
#LIWC really isn't very good!
ggplot(subset(mydata, posemo!=0&negemo!=0), aes(x = posemo, y = negemo)) + 
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  geom_abline(a = 0, b =1, lwd = 3, lty = 2) + 
  geom_jitter(width=.2, height=.2, alpha = .1, cex = 2, col="red") +
  xlim(0,20) +
  ylim(0,20) +
  print(ggtitle("Contour Plot of Positive & Negative Emotions in Tweets")) + 
  print(labs(x = "Positive Emotion Score", y = "Negative Emotion Score")) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20,face = "bold", hjust=.5))

#47.5% of my data falls along the positive emotion = negative emotion line
sum(mydata$negemo==mydata$posemo)/nrow(mydata) 

#91.7% of the data is less than 20 on either emotion
sum(mydata$negemo<20&mydata$posemo<20)/nrow(mydata)

#35.6% of the data, positive emotion is greater then negative emotion
sum(mydata$posemo > mydata$negemo)/nrow(mydata)

#16.8% of the data, negative emotion is greater than positive emotion
sum(mydata$posemo < mydata$negemo)/nrow(mydata)


# timeseries graph --------------------------------------------------------

#user1's time series of positive emotion in tweets across ENTIRE user history
ggplot(subset(mydata, user_id==users[1]), aes(y=posemo, x=created_at)) + 
  geom_point(alpha=.5, cex = 2) +
  geom_smooth(lwd = 2) +
  print(ggtitle("Positive Emotion Time Series for User 1")) + 
  print(labs(x = "Time", y = "Positive Emotion Score")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 26,face = "bold", hjust=.5))

#zooms in to user 1 between Mar 20 and April 10 but doesn't change the gam fit
ggplot(subset(mydata, user_id==users[1]), aes(y=posemo, x=created_at)) + 
  geom_point(alpha=.5, cex = 2) +
  geom_smooth(lwd = 2) +
  coord_cartesian(ylim = c(0, 30), xlim = c(as.POSIXct("2018-04-10 13:40:52",tz="UTC"),
                                            as.POSIXct("2018-03-20 13:40:52",tz="UTC"))) +
  print(ggtitle("Positive Emotion Time Series for User 1")) + 
  print(labs(x = "Time", y = "Positive Emotion Score")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 26,face = "bold", hjust=.5))

#zooms in AND changes the gam fit
ggplot(subset(mydata, user_id==users[1]&mydata$year==2018&mydata$month>2), aes(y=posemo, x=created_at)) + 
  geom_point(alpha=.5, cex = 2) +
  geom_smooth(lwd = 2) +
  print(ggtitle("Positive Emotion Time Series for User 1")) + 
  print(labs(x = "Time", y = "Positive Emotion Score")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 26,face = "bold", hjust=.5))

#zooms in, changes gam it AND adds the target tweet line (dashed)
ggplot(subset(mydata, user_id==users[1]&mydata$year==2018&mydata$month>2), aes(y=posemo, x=created_at)) + 
  geom_vline(xintercept=mydata$created_at[mydata$status_id==mydata$target_id&mydata$user_id==users[1]], lwd = 2, lty=2, col = "dodgerblue")+
  geom_point(alpha=.5, cex = 2) +
  geom_smooth(lwd = 2) +
  coord_cartesian(ylim = c(0, 20)) +
  xlim(as.POSIXct("2018-03-26 00:00:00",tz="UTC"),
       as.POSIXct("2018-04-10 13:40:52",tz="UTC")) +
  print(ggtitle("Positive Emotion Time Series for User 1")) + 
  print(labs(x = "Time", y = "Positive Emotion Score")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 26,face = "bold", hjust=.5))

# Average positivity per user ---------------------------------------------

#to calculate average positivity per user, I used dplyr's group_by feature, summarise and mutate
emotionbyuser <- mydata %>% group_by(user_id) %>%
  summarise(mean_pos = mean(posemo), se_pos = sd(posemo)/sqrt(length(posemo)), mean_neg = mean(negemo), se_neg = sd(negemo)/sqrt(length(posemo))) %>%
  mutate(se_pos_lower = mean_pos - 1.96*se_pos, se_pos_upper = mean_pos + 1.96*se_pos, 
         se_neg_lower = mean_neg - 1.96*se_neg, se_neg_upper = mean_neg + 1.96*se_neg)
#give unique identifier to users
emotionbyuser$user_id <- as.factor(1:90)

#sort by biggest pos emo to smallest pos emo
emotionbyuser <- arrange(emotionbyuser, desc(mean_pos))
#create column to identify order
emotionbyuser$plotorder <- 1:90
#relevel to the plotorder
emotionbyuser$user_id <- factor(emotionbyuser$user_id, 
                                 levels = emotionbyuser$user_id[order(emotionbyuser$plotorder)])

#plot for pos emo
ggplot(emotionbyuser, aes(x = user_id, y = mean_pos)) +
  geom_errorbar(aes(ymax=se_pos_upper, ymin=se_pos_lower, width=1)) + 
  geom_point(size = 3) +
  print(ggtitle("Mean & 95CI Positive Emotion by User")) + 
  print(labs(x = "User #", y = "Mean Positive Emotion")) +
  theme(axis.text=element_text(size=26),
        axis.title=element_text(size=26,face="bold"),
        plot.title = element_text(size = 28,face = "bold", hjust=.5),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

#sort by biggest neg emo to smallest neg emo
emotionbyuser <- arrange(emotionbyuser, desc(mean_neg))
#create column to identify order
emotionbyuser$plotorder <- 1:90
#relevel to the plotorder
emotionbyuser$user_id <- factor(emotionbyuser$user_id, 
                                levels = emotionbyuser$user_id[order(emotionbyuser$plotorder)])

ggplot(emotionbyuser, aes(x = user_id, y = mean_neg)) +
  geom_errorbar(aes(ymax=se_neg_upper, ymin=se_neg_lower, width=1)) + 
  geom_point(size = 3) +
  print(ggtitle("Mean & 95CI Negative Emotion by User")) + 
  print(labs(x = "User #", y = "Mean Negative Emotion")) +
  theme(axis.text=element_text(size=26),
        axis.title=element_text(size=26,face="bold"),
        plot.title = element_text(size = 28,face = "bold", hjust=.5),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

# statistical models --------------------------------------------------------------------

#I just explored a ton of possibilities for fun
#posemo2 and negemo2 exist so I can use the log function
mydata$posemo2 <- mydata$posemo+1
mydata$negemo2 <- mydata$negemo+1

#I realized what I probably really care about is the total valence of the tweet.
mydata$emocomp <- mydata$posemo - mydata$negemo

#same idea here, adding 101 allows me to log the tweet's emotion
mydata$emocomp.log <- log(mydata$emocomp+101)

#a null model averaging positive valence of tweets nested within users and time
m1 <- lmer(posemo ~ 1 + (1|user_id/year/month/week), data = mydata)
summary(m1)

#potential fixed effect after target tweet
m1 <- lmer(posemo ~ prepost + (1|user_id/year/month/week), data = mydata)
summary(m1) #nope!

#maybe logging the tweet will make 
m1 <- lmer(log2(posemo2) ~ prepost + (1|user_id/year/month/week), data = subset(mydata))
summary(m1) #that didn't really fix the non-normal errors

#just out of curiosity, I subsetted my data to be just a two-week period

mydata.2weeks <- subset(mydata, mydata$created_at>as.POSIXct("2018-03-26 00:00:00",tz="UTC")
                        &mydata$created_at<as.POSIXct("2018-04-10 13:40:52",tz="UTC"))
library(lme4)
library(lmerTest)

#how do the models fit now?
m1 <- lmer(posemo ~ prepost + (1|user_id), mydata.2weeks)
summary(m1) #nope

m1 <- lmer(log2(posemo2) ~ prepost + (1|user_id), mydata.2weeks)
summary(m1) #nope

m1 <- lmer(emocomp ~ prepost + (1|user_id), mydata.2weeks)
summary(m1) #nope

m1 <- lmer(log2(emocomp+101) ~ prepost + (1|user_id), mydata.2weeks)
summary(m1) #nope

#this is fair. I didn't honestly expect anything. 
