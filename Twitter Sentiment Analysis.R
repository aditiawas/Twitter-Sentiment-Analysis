library(rtweet)
library(ggplot2)
library(dplyr) #flexible grammar for data manipulation
library(tidytext) #text mining

consumer_key <- "9udWop8X4wNworS0eltQaTI1I"
consumer_secret <-"Sxv1gNOAVoDSlKnYGjItcdaiHTaqctMFPctUPYW9X9IwxtH0gH"
access_token <- "1194272279678484480-9MVyyiHlH7hCw7pOMY5FxXedMcLNja"
access_secret <- "4uZLHJQKlYKlmablWzuFTfFX7VODjBT8Yfx1zXXVdPh0r"
twitter_token <- create_token(consumer_key = consumer_key, consumer_secret = consumer_secret, access_token = access_token, access_secret = access_secret)

#################### basic ####################

rstats_tweets <- search_tweets(q = "#superwoman", n = 180)
View(rstats_tweets)

users <- search_users("#modi", n = 50)

users %>%
  dplyr::count(location, sort = TRUE) %>%
  mutate(location = reorder(location,n)) %>%
  na.omit() %>%
  top_n(20) %>%
  ggplot(aes(x = location,y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Location",
       y = "Count",
       title = "Twitter users - unique locations ")

## plot time series of tweets
rstats_tweets %>%
  ts_plot("3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #modi Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals"
  )

tmls <- get_timelines(c("cnn", "BBCWorld"), n = 70)

tmls %>%
  dplyr::group_by(screen_name) %>%
  ts_plot(by="hours") +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by news organization",
    subtitle = "Twitter status (tweet) counts aggregated by hour",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#################### cleanup ####################

climate_tweets <- search_tweets(q = "#trump", n = 10, lang = "en", include_rts = FALSE)
#data cleanup, remove http elements
# remove punctuation, convert to lowercase, add id for each tweet - from tidytext
climate_tweets_clean <- climate_tweets %>% 
  mutate(tweet_text = gsub("http://*|https://*|http*|https*)", "", text)) %>% 
  dplyr::select(tweet_text) %>% 
  unnest_tokens(word, tweet_text)

#remove stop words
data("stop_words")
cleaned_tweet_words <- climate_tweets_clean %>% anti_join(stop_words) %>% filter(!word %in% c("rt", "t.co"))

cleaned_tweet_words %>%
  dplyr::count(word) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")

#Explore Networks of Words
library(devtools)
library(widyr)

climate_tweets$stripped_text <- gsub("http.*","",  climate_tweets$text)
climate_tweets$stripped_text <- gsub("https.*","", climate_tweets$stripped_text)

climate_tweets_paired_words <- climate_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

library(tidyr)
climate_tweets_separated_words <- climate_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

library(dplyr)
climate_tweets_filtered <- climate_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

climate_words_counts <- climate_tweets_filtered %>%
  dplyr::count(word1, word2, sort = TRUE)

#plot word network
library(igraph)
library(ggraph)
climate_words_counts %>%
  filter(n >= 1) %>%  # check value of n first
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag",
       x = "", y = "")

tweet_data <- data.frame(date_time = climate_tweets$created_at,
                         username = climate_tweets$screen_name,
                         tweet_text = climate_tweets$text)

start_date <- as.POSIXct('2019-11-16 00:00:00')
end_date <- as.POSIXct('2019-11-18 00:00:00')

clim_tweets <- tweet_data %>%
  mutate(date_time = as.POSIXct(date_time, format = "%a %b %d %H:%M:%S +0000 %Y")) %>% 
  filter(date_time >= start_date & date_time <= end_date )

#################### maps ####################

#geocoding
library(rjson)
library(httr)
options(stringsAsFactors = FALSE)
climate_tweets2 <- search_tweets(q = "#trump", n = 100, lang = "en")
data <- paste0("[",paste(paste0("\"",climate_tweets2$location,"\""),collapse=","),"]")
url  <- "http://www.datasciencetoolkit.org/street2coordinates"
response <- POST(url,body=data)
json     <- fromJSON(content(response,type="text"))
geocode  <- do.call(rbind,sapply(json,
                                 function(x) c(long=x$longitude,lat=x$latitude)))
geocode
geocode<-cbind(rownames(geocode),geocode)
colnames(geocode)[1]<-"location"
geocode<-as.data.frame(geocode)
library(plyr)
climate_tweets2<-join(climate_tweets2,geocode,by="location")

climate_tweets2<-climate_tweets2%>%
  mutate(date_time = as.POSIXct(created_at, format = "%a %b %d %H:%M:%S +0000 %Y"))

tweet_locations <- data.frame(climate_tweets2$text,climate_tweets2$date_time,climate_tweets2$long,climate_tweets2$lat) %>% na.omit()
colnames(tweet_locations)<-c("text","date_time","long","lat")
tweet_locations$long <- as.numeric(tweet_locations$long)
tweet_locations$lat <- as.numeric(tweet_locations$lat)

#simple map
library(ggthemes)
world_basemap <- ggplot() + borders("world", colour = "gray85", fill = "gray80") + theme_map()
world_basemap +
  geom_point(data = tweet_locations, aes(x = long, y = lat),
             colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8),
                        breaks = c(250, 500, 750, 1000)) +
  labs(title = "Tweet Locations")

#interactive map
library(leaflet)
site_locations <- leaflet(tweet_locations) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~long, lat = ~lat, popup = ~text,
                   radius = 4, stroke = FALSE)
site_locations

#group by minute
library(lubridate)
tweet_locations_grp <- tweet_locations %>%
  mutate(minute = minute(date_time),
         long_round = round(long, 2),
         lat_round = round(lat, 2)) %>%
  group_by(minute, long_round, lat_round) %>%
  dplyr::summarise(total_count = dplyr::n())

grouped_tweet_map <- world_basemap + 
  geom_point(data = tweet_locations_grp,
             aes(long_round, lat_round, size = total_count),
             color = "purple", alpha = .5) + coord_fixed() +
  labs(title = "Twitter Activity")
grouped_tweet_map

#################### sentiment analysis ####################

## bing

bing_word_counts <- cleaned_tweet_words %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiments",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#another example
library(lubridate)
library(zoo)
tweets <- climate_tweets %>%
  mutate(date_time = as.POSIXct(created_at, format = "%a %b %d %H:%M:%S +0000 %Y")) %>%
  mutate(tweet_text = gsub("http://*|https://*)", "", text),
         month = as.yearmon(date_time))

tweet_clean <- tweets %>%
  dplyr::select(text, month) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("rt", "t.co"))

bing_sentiment <- tweet_clean %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, month, sort = TRUE) %>%
  group_by(sentiment) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  group_by(month, sentiment) %>%
  top_n(n = 5, wt = n) %>%
  mutate(sent_date = paste0(month, " - ", sentiment)) %>%
  arrange(month, sentiment, n)


bing_sentiment$sent_date <- factor(bing_sentiment$sent_date,
                                   levels = unique(bing_sentiment$sent_date))

bing_sentiment %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sent_date, scales = "free_y", ncol = 2) +
  labs(title = "Sentiment",
       y = "Number of Times Word Appeared in Tweets",
       x = NULL) +
  coord_flip()

## NRC

nrc_word_counts <- cleaned_tweet_words %>%
  inner_join(get_sentiments("nrc")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  ungroup()

nrc_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiments",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#another example
library(lubridate)
library(zoo)
tweets <- climate_tweets %>%
  mutate(date_time = as.POSIXct(created_at, format = "%a %b %d %H:%M:%S +0000 %Y")) %>%
  mutate(tweet_text = gsub("http://*|https://*)", "", text),
         month = as.yearmon(date_time))

tweet_clean <- tweets %>%
  dplyr::select(text, month) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(!word %in% c("rt", "t.co"))

nrc_sentiment <- tweet_clean %>%
  inner_join(get_sentiments("nrc")) %>%
  dplyr::count(word, sentiment, month, sort = TRUE) %>%
  group_by(sentiment) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  group_by(month, sentiment) %>%
  top_n(n = 5, wt = n) %>%
  mutate(sent_date = paste0(month, " - ", sentiment)) %>%
  arrange(month, sentiment, n)


nrc_sentiment$sent_date <- factor(nrc_sentiment$sent_date,
                                  levels = unique(nrc_sentiment$sent_date))

nrc_sentiment %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sent_date, scales = "free_y", ncol = 2) +
  labs(title = "Sentiment",
       y = "Number of Times Word Appeared in Tweets",
       x = NULL) +
  coord_flip()
