library(rtweet)
library(httpuv)
library(dplyr)
library(reshape) 
library(ggplot2)
library(data.table)
library(tidytext)
library(maps)
library(igraph)
library(qdapRegex)
library(qdap)
library(tm)
library(syuzhet)


# Enter Twitter API Tokens
# Appname
appname <- "Titans Tech-Klaus"

# API key
key <- "tjAnEPZAkAjh712YEU4ySdKcK"

# API Secret
secret <- "oCadnJdhwr1NVWRM1xyoajl5yf8XwZojL1TBgaNx1DNCfmEEx2"

twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = "1406931689385848834-Htg0WpINI3iy3u30K6voyChipYl5cx",
  access_secret = "NG0nfeqglYQHSSwhGunCMZ5BHGPTzuDp8B9RtY17X42i9")



# Extracting actual twitter text
# Search for tweets about lockdown, exclude retweets
fifa_tweets <- search_tweets("fifa 2022", n = 18000, include_rts = F, lang = 'en')


# Sentiment analysis(sa): Looks at sentiment of lockdown tweets
sa_value <- get_nrc_sentiment(fifa_tweets$text)
# View sentiment scores
sa_value[1:5, 1:7]
# Calculate sum of scores
sa_scores <- colSums(sa_value[,])
# Convert to data frame
sa_scores_df <- data.frame(sa_scores)
# Create sentiment column in the dataframe
sa_scores_final <- cbind(sentiment=row.names(sa_scores_df), 
                         sa_scores_df, row.names = NULL)
# Plot the sentiment scores
ggplot(data = sa_scores_final, aes(x = sentiment, y = sa_scores, 
                                   fill = sentiment)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Sentiment Analysis of 3000 Recent Tweets on 'FIFA World Cup', 23rd Sept 2021")


# Create new plot with ONLY negative and positive column
# Find data with only negative emotions
neg.df <- sa_scores_final[sa_scores_final$sentiment %in%
                            c("anger", "disgust",
                              "fear", "sadness", "negative"), ]
# Find data with only positive emotions
pos.df <- sa_scores_final[sa_scores_final$sentiment %in%
                            c("anticipation", "joy",
                              "positive", "surprise", "trust"), ]
# Sum the sa scores columns
neg.val <- sum(neg.df$sa_scores)
pos.val <- sum(pos.df$sa_scores)
# Create the new dataframe with negative and positive values
Sentiment <- c("Negative", "Positive")
sa.score <- c(neg.val, pos.val)
neg.pos.df <- data.frame(Sentiment, sa.score)
print(neg.pos.df)

# Plot the sentiment scores, only negative and positive scores
ggplot(data = neg.pos.df, aes(x = Sentiment, y = sa.score,
                              fill = Sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Positive/Negative Sentiments 3000 Recent Tweets on 'FIFA World Cup', 23rd Sept 2021")



# ---- VISUALISATIONS ----
# Time series analysis of the frequency of two tweets
#tweet_1 <- search_tweets("fifa world cup" , n = 5000, include_rts = FALSE)

tweet1_graph <- ts_plot(fifa_tweets,  by = "weeks" , color = "blue")
tweet_1 <- ts_data(fifa_tweets, by = 'weeks')
names(tweet_1) <- c("time" , "fifa world cup")

tweet_2 <- search_tweets("fifa 22" , n = 5000, include_rts = FALSE)
tweet_2 <- ts_data(tweet_2, by = 'weeks')
names(tweet_2) <- c("time" , "fifa 22")

merged_df <- merge(tweet_2, tweet_1, by = "time" , all = TRUE)
melt_df <- reshape::melt(merged_df, na.rm = TRUE, id.vars = "time")

ggplot(data = melt_df, aes(x = time, y = value, col = variable)) +
  geom_line(lwd = 0.8)
