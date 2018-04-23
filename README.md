# ps239T-final-project

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


## Code
01_collect-nyt.py: Collects data from New York Times API and exports data to the file nyt.csv
02_merge-data.R: Loads, cleans, and merges the raw Polity and NYT datasets into the Analysis Dataset.
03_analysis.R: Conducts descriptive analysis of the data, producing the tables and visualizations found in the Results directory.
Results
coverage-over-time.jpeg: Graphs the number of articles about each region over time.
regression-table.txt: Summarizes the results of OLS regression, modelling nyt on a number of covariates.
More Information
Include any other details you think your user might need to reproduce your results. You may also include other information such as your contact information, credits, etc.
