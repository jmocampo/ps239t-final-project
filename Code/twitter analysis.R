library(lme4)
library(lmerTest)
library(psych)
library(ggplot2)
load("twits5.Rdata")

# Descriptive stats ---------------------------------------------------------
users <- unique(mydata$user_id)
mydata <- mydata[mydata$lang=="en",]

#sample size (tweets per person)

#number of tweets
length(unique(mydata$user_id)) #twitter users
length(unique(mydata$created_at))
max(table(mydata$screen_name))
min(table(mydata$screen_name))
mean(table(mydata$screen_name))
sd(table(mydata$screen_name))

tweetsPer <- as.data.frame(table(mydata$screen_name))
median(tweetsPer$Freq)

hist(table(mydata$screen_name), main = "Distribution of # Tweets per Person", xlab = "Number of Tweets", col = "dodgerblue2", cex.lab = 2, cex.main = 2, cex.axis = 2)

ggplot(tweetsPer, aes(Freq)) + 
  geom_density(lwd=2) +
  print(ggtitle("Density Plot of Tweets Per User")) + 
  print(labs(x = "# of Tweets", y = "Density")) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20,face = "bold", hjust=.5))

# emotion plots -----------------------------------------------------------
mean(mydata$posemo)
sd(mydata$posemo)
median(mydata$posemo)

mean(mydata$negemo)
sd(mydata$negemo)
median(mydata$negemo)

sum(mydata$posemo==0)/nrow(mydata)
sum(mydata$negemo==0)/nrow(mydata)
sum(mydata$negemo==0&mydata$posemo)/nrow(mydata)

##positive emotion density plots
ggplot(mydata, aes(posemo)) + 
  geom_density(lwd=2) +
  print(ggtitle("Density Plot of Positive Emotions in Tweets")) + 
  print(labs(x = "Positive Emotion Score", y = "Density")) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20,face = "bold", hjust=.5))

ggplot(subset(mydata, posemo!=0), aes(posemo)) + 
  geom_density(lwd=2) +
  print(ggtitle("Density Plot of Positive Emotions in Tweets (Excluding 0's)")) + 
  print(labs(x = "Positive Emotion Score", y = "Density")) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20,face = "bold", hjust=.5))

ggplot(subset(mydata, posemo!=0&posemo!=100), aes(posemo)) + 
  geom_density(lwd=2) +
  print(ggtitle("Density Plot of Positive Emotions in Tweets (Excluding 0's & 100's)")) + 
  print(labs(x = "Positive Emotion Score", y = "Density")) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20,face = "bold", hjust=.5))

##positive and negative emotion density plots
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

sum(mydata$negemo==mydata$posemo)/nrow(mydata)
sum(mydata$negemo<20&mydata$posemo<20)/nrow(mydata)
sum(mydata$posemo > mydata$negemo)/nrow(mydata)
sum(mydata$posemo < mydata$negemo)/nrow(mydata)


# timeseries graph --------------------------------------------------------

ggplot(subset(mydata, user_id==users[1]), aes(y=posemo, x=created_at)) + 
  geom_point(alpha=.5, cex = 2) +
  geom_smooth(lwd = 2) +
  print(ggtitle("Positive Emotion Time Series for User 1")) + 
  print(labs(x = "Time", y = "Positive Emotion Score")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 26,face = "bold", hjust=.5))

#zooms in but doesn't change the gam fit
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

ggplot(subset(mydata, user_id==users[1]&mydata$year==2018&mydata$month>2), aes(y=posemo, x=created_at)) + 
  geom_point(alpha=.5, cex = 2) +
  geom_smooth(lwd = 2) +
  print(ggtitle("Positive Emotion Time Series for User 1")) + 
  print(labs(x = "Time", y = "Positive Emotion Score")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 26,face = "bold", hjust=.5))

#for user 1
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

mydata$target_id[mydata$user_id==users[1]]

# Average positivity per user ---------------------------------------------

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
  
mean(mydata$negemo)

# lmer --------------------------------------------------------------------
library(lme4)
library(lmerTest)
mydata$posemo2 <- mydata$posemo+1
mydata$negemo2 <- mydata$negemo+1
mydata$emocomp <- mydata$posemo - mydata$negemo
mydata$emocomp.log <- log(mydata$emocomp+101)
hist(mydata$emocomp)
hist(log2(mydata$posemo+1))
hist(mydata$negemo)
hist(log2(mydata$negemo+1))
hist(mydata$emocomp+101)
hist(log2(mydata$emocomp+101), xlim = c(5.5,7.5), breaks = 120)
describe(mydata$posemo)
describe(mydata$emocomp)
describe(log2(mydata$posemo+1))
describe(log2(mydata$emocomp+101))

m1 <- lmer(posemo ~ 1 + (1|user_id/year/month/week), data = mydata)
summary(m1)

m1 <- lmer(posemo ~ prepost + (1|user_id/year/month/week), data = mydata)
summary(m1)

m1 <- lmer(posemo ~ prepost + (1|user_id/year/month/week), data = subset(mydata, posemo!=0))
summary(m1)

m1 <- lmer(log2(posemo) ~ scale(totaltweets)+prepost + (1|user_id/year/month), data = subset(mydata, posemo!=0))
summary(m1)

m1 <- lmer(emocomp ~ prepost + (1|user_id/year/month/week), data = subset(mydata, emocomp!=0))
summary(m1)

mydata$posemo2 <- mydata$posemo + 1
hist(mydata$posemo)
hist(log2(mydata$posemo2))


# log posemo --------------------------------------------------------------
m1 <- lmer(log2(posemo) ~ scale(totaltweets)+prepost + (1|user_id/year/month)+(0+scale(totaltweets)|user_id), mydata[mydata$posemo!=0,])
summary(m1)

m1 <- lmer(log2(emocomp2) ~ scale(totaltweets)+prepost + (1|user_id/year/month), mydata[mydata$posemo!=0,])
summary(m1)

m2 <- lmer(log2(posemo2) ~ prepost + (prepost|user_id/year/month), mydata)
summary(m2)
anova(m1,m2) #m2 is better

nonemotext <- mydata$plainText[mydata$posemo==0&mydata$negemo==0]
write.csv(nonemotext, "nonemotional-text.csv")

#need to quantify time since the tweet

mydata.2weeks <- subset(mydata, mydata$created_at>as.POSIXct("2018-03-26 00:00:00",tz="UTC")
                        &mydata$created_at<as.POSIXct("2018-04-10 13:40:52",tz="UTC"))
hist(mydata.2weeks$posemo[mydata.2weeks$posemo>0])
hist(log2(mydata.2weeks$posemo))
library(lme4)
library(lmerTest)

m1 <- lmer(log2(posemo+1) ~ prepost + (1|user_id), mydata.2weeks)
summary(m1)

mydata.2weeks$posemo2 <- mydata.2weeks$posemo+1

m1 <- lmer(posemo~prepost + (1|user_id), mydata.2weeks)
summary(m1)

m1 <- lmer(log2(posemo2)~prepost + (1|user_id), mydata.2weeks)
summary(m1)

m2 <- lmer(log2(posemo2)~ scale(totaltweets) + prepost + (1|user_id), mydata.2weeks)
summary(m2)
anova(m1,m2) #m2 is better

m3 <- lmer(log2(posemo2)~ scale(totaltweets) * prepost + (1|user_id), mydata.2weeks)
summary(m3)
anova(m2,m3) #m2 still better

m4 <- lmer(log2(posemo2)~ scale(totaltweets) + prepost + (scale(totaltweets)|user_id), mydata.2weeks)
summary(m4)
anova(m2,m4) #m2 fixed effects is better

m5 <- lmer(log2(posemo2)~ scale(totaltweets) + prepost + (prepost|user_id), mydata.2weeks)
summary(m5)
anova(m2,m5) #m5 better

m6 <- lmer(log2(posemo2)~ scale(totaltweets) + (scale(totaltweets)*prepost|user_id), mydata.2weeks)
summary(m5)
anova(m5,m6) #m6 better

m7 <- lmer(log2(posemo2)~ 1 + (scale(totaltweets)*prepost|user_id), mydata.2weeks)
summary(m5)
anova(m6,m7) #m6 better

m6 <- lmer(log2(posemo2)~ scale(totaltweets) + (scale(totaltweets)*prepost|user_id), mydata.2weeks)
summary(m6)
anova(m5,m6) #m6 better

coef(m6)


# english identified only --------------------------------------------------------------
mydata.2weeks$posemo2 <- log2(mydata.2weeks$posemo+1)
mydata.2weeks$negemo2 <- log2(mydata.2weeks$negemo+1)
mydata.2weeks$emocomp <- mydata.2weeks$posemo - mydata.2weeks$negemo
hist(mydata.2weeks$posemo)
hist(mydata.2weeks$negemo)
hist(mydata.2weeks$posemo2)
hist(mydata.2weeks$negemo2)
hist(mydata.2weeks$emocomp)
hist(log2(mydata.2weeks$emocomp))

mydata.2weeks$plainText[mydata.2weeks$posemo&mydata.2weeks$negemo==0][1:10]
mydata.2weeks$text[1:100]

meandata <- describeBy(mydata$posemo, list(mydata$user_id, mydata$prepost), digits=3, mat = T)


