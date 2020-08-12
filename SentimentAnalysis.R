#' ---
#' title: "Project 3 : Sentiment Analysis of tweets related to Siri"
#' author: "Aparna Cleetus (axc190011), Obuli Vignesh Rangasamy (oxr170630)"
#' ---


# Import all the libraries
library(rtweet)
library(sentimentr)
library(ndjson)
library(ggplot2)
library(tidyverse)
library(tidytext)

# Get Siri related tweets using search_stream()
siri_tweets <-search_tweets(q="#siri", n =10000, include_rts =FALSE, lang="en")
siri_tweets$stripped_text <- gsub("http.*","",  siri_tweets$text)
siri_tweets$stripped_text <- gsub("https.*","", siri_tweets$stripped_text)
siri_tweets$stripped_text <- gsub("#*","", siri_tweets$stripped_text)
siri_tweets$stripped_text <- gsub("@*","", siri_tweets$stripped_text)

# Get the approximate sentiment (polarity) of each tweet 
siri_sentiment <-sentiment_by(siri_tweets$stripped_text)
head(siri_sentiment)
summary(siri_sentiment$ave_sentiment)

# Plot the approximate sentiment (polarity) for each tweet 
ggplot(siri_sentiment, aes(x=ave_sentiment)) +
      geom_histogram() +
      labs(x ="ave_sentiment",y ="Tweets related to siri", title = "Approximate sentiment (polarity) for each tweet")

# Siri tweets analysis
# Get the most frequent words
siri_tokens <-siri_tweets %>%
      select(text) %>%
      unnest_tokens(word, text)

#plot the top ten frequent words in the tweets
siri_tokens %>%
  group_by(word) %>%
  summarise(count =n()) %>%
  anti_join(stop_words) %>%
  filter(!word %in%c('https', 't.co', 'siri99', 'siri', 'amp', 'gt')) %>%
  arrange(desc(count)) %>%top_n(10) %>%
  mutate(word =reorder(word, count)) %>%
  ggplot(aes(x =word, y =count)) +geom_col() +
  theme(axis.text.x =element_text(angle=45, hjust=1)) +
  labs(title ="Top ten most frequent words among the tweets")

# Understand Siri tweets emotions
siri_emotions <-emotion_by(get_sentences(siri_tweets$stripped_text))

siri_emotions <-siri_emotions %>%
      group_by(element_id) %>%
      filter(emotion_count ==1) 

#plot of the emotions observed for the tweets
siri_emotions %>%
  group_by(emotion_type) %>%
  summarise(count =n()) %>%
  top_n(10) %>%
  mutate(emotion_type =reorder(emotion_type, count)) %>%
  ggplot(aes(x=emotion_type, y =count)) +
  geom_col() +theme(axis.text.x =element_text(angle=45, hjust=1)) +
  labs(x="Emotion", y= "Count of tweets",title ="Various emotions observed in the tweets")

# Generic statistics about the tweets
siri_tweets %>%
  group_by(source) %>%
  summarise(count =n()) %>%
  top_n(5) %>%arrange(desc(count)) %>%
  ggplot(aes(x=source, y =count)) +
  geom_col() +theme(axis.text.x =element_text(angle=45, hjust=1)) +
  labs(y ="Count of tweets",x ="Source device of Twitter",title ="Device usage of twitter users")

# plot time series of tweets
siri_tweets %>%
  ts_plot("3 hours") +
  labs(title ="Frequency of siri Twitter statuses from past few days")





