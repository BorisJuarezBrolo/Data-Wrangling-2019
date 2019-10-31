library(twitteR)
library(dplyr)
library(readr)
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tw <- twitteR::searchTwitter('#DWTS', n = 1e4, since = '2019-10-21', retryOnRateLimit = 1e3)

df <- twitteR::twListToDF(tw)
df %>% head() %>% View()


write_csv(df,"tweets.csv")
