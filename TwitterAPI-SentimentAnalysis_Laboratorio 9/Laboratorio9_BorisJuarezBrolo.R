install.packages('twitteR')
library(dplyr)
library(readr)
library(twitteR)

consumer_key <- "Vvwa1DTvRV5rs4Vpr8ddrxC8G"
consumer_secret <- "CRQrvSjbQzYARUwreBKPE3t3votjfHkGZE9xT0bBCfbTJXV8IT"

access_token <- "199790612-KXpHHgMkLpKHPvcJpM6QGXZFs5FwZn7DAzQuGNpV"
access_secret <- "S2Gvk5JHfw5qZOHJsMnhxLCNn3o1UZDvYQ9nmETdDIGIi"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tw <- twitteR::searchTwitter('#WorldSeries', n = 1e4, since = '2019-10-29', retryOnRateLimit = 1e3 )

df<-twitteR::twListToDF(tw)
df%>% head()%>% View()

write_csv(df,"tweetslab9.csv")
