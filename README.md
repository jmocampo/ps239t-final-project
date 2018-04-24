# ps239T-final-project

Note: I had to trim the data files because there was too much data to upload in one file. Because of this, some of the code may seem redundant or unnecessary (because I already pre-cleaned the data in order to upload it). But the results it produces should be the same as when I ran it. Also, the data collection code won't work very well because it acts on "tweet objects" from rtweet, but the data I saved was a csv, not a tweet object. In file 02 thru 04, I had saved the rData files, so the tweet objects are preserved.

# Short Description
I used the rtweet package to search popular tweets in the US. rtweet has functions that create your auth-token for the twitter API and then use the API to run the standard API commands like search and get timeline. I wrote these popular tweets that are curated by Twitter as a csv and read through them to identify a tweet with positive emotional valence. The first tweet that was positive became my target tweet. Afterward, I used rtweets get_timeline function to retrieve the tweet history of 90 twitter users that retweeted the target tweet. I ran it in a for-loop that I manually configured to print what step it is at. The way the function works, I can restart where I left off if I get rate-limited. 

After collecting the data, I cleaned it up using dplyr, tidyr and base R functions. For example, I deleted non-English language tweets, did some sanity checks to make sure I had 90 unique users, found out that I had a ton of duplicates, removed the duplicates and fixed up the time stamp data to work in a lme4. I used ggplot2 to make graphs showing some of the trend data and descriptive statistics visually. Because of the noisiness of LIWC data (0's), it was not super helpful, but the time series visual of the bing-scored sentiment was much easier to interpret. I noticed there was actually a small bump in sentiment post-target tweet when you zoomed in to a 2 week level. 

Finally, after cleaning and visualizing the data, I made multilevel models with users nested in years, nested in months, nested in weeks. This is a little overkill, I'm sure. But the basic idea is that I wanted to factor in random variability from time, but without getting too fine-grained because I did expect a fixed effect (post-target tweet) in terms of days. 

# Dependencies
LIWC2015
R, version 3.4.4 and the following packages:
dplyr
ggplot2
lme4
lmerTest
lubridate
psych
rtweet (accessing twitter API in March and April of 2018)
tidyr
tidytext

# Files
List all other files contained in the repo, along with a brief description of each one, like so:

## Data
01-targettweets.csv: a csv containing a list of curated most popular tweets around march 27 from twitter.

02-retweeters.csv: a dataset of 90 people who retweeted the target tweet

03-cleaningenv.RData: tweet object data for the first-wave of data collection and second-wave of data collection. Is large file. Contains tweet object information down to the individual tweets for the 90 users.

04-tweetstext2.csv: text file of all the tweets

05-LIWC2015 Results (tweetstext.csv): first LIWC coding of tweet texts

06-LIWC2015 Results (tweetstext2.csv): second LIWC coding of tweet texts

07-analysisenv.Rdata: the data environment I used for data analysis. Maintains the tweetobject information of most of the data.
happyTweetstemp.csv: example of how I wrote a file to view the tweets.

## Code
01-twitter-data-collection.R: how I collected my data using rtweet package.

02-twitter-prep-file.R: cbinding the two get_timeline tweet data together, adding LIWC coded information, cleaning up date information using lubridate

03-twitter-analysis.R: first wave of analysis, mostly descriptive plots of LIWC emotion data of the tweets, but also some linear mixed effects models attempting to find a fixed effect of the target tweet on tweet valence.

04-twitter-analysis-using-bing.R: did my own valence coding of each tweet using the 

## Results
A ton of graphs!
