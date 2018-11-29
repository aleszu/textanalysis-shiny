---
title: "Exploring Beto O'Rourke and Ted Cruz ads on Facebook in R"
author: "Aleszu Bajak"
date: "11/1/2018"
output: html_document
---

# Summary

When and how often are Beto O'Rourke ads being rolled out on Facebook and who is paying for them? What is the sentiment of those ads and what kinds of words are unique to specific advertisers? 

Using the open-source statistical software R, the following analysis, which explores over 94,000 Facebook ads collected by ProPublica's [Political Ad Collector](https://projects.propublica.org/political-ad-collector/), uses the packages "tidytext" for **textual and sentiment analysis**, "ggplot2" and "plotly" for **data visualization**, and "dplyr," "stringr" and "tidyverse" for **data wrangling**. 

The **tidytext** package, developed by David Robinson and Julia Silge, works with other R packages, such as the **tidyverse**, designed for easily ingesting, transforming, comparing and visualizing textual data (Wickham, 2014). The sentiment dictionary used is **labMTenglish** developed by Andy Reagan et al. at the University of Vermont's Computational Story Lab (Reagan, 2017).

```{r setup, include=FALSE}
knitr::opts_chunk$set(fig.width=10, fig.height=8,
                      echo=FALSE, warning=FALSE)
```

```{r}
library(dplyr)
library(tidyverse)
library(tidytext)
library(plotly)
library(stringr)
```

# Analysis

### Pull in CSV of Facebook ads data

After pulling in the spreadsheet downloaded from ProPublica's [Data Store](https://propublica.org/datastore/dataset/political-advertisements-from-facebook), I extracted and added a date column.  

```{r}
df <- read.csv("fbads.csv", header=TRUE, stringsAsFactors = FALSE) 
df$date <- as.Date(df$created_at) # Add date column
df %>% glimpse()
```

### Search for a politician or keyword

The "stringr" package can be used to detect specific chunks of text. In this case I'm looking for every mention of **Beto** in the "message" column. This won't capture every Beto O'Rourke ad but it *will* capture all that mention his first name.

```{r}
betocomments <- df %>%
  filter(str_detect(message, "Beto")) 
betocomments %>% glimpse()
```

### An overview of the Beto Facebook ads

The summary() function gives us some basic descriptive statistics of each column. Some key stats: 

* There are 1,397 ads with "Beto" in the "message"
* The "Beto" ads run from Sep 2017 to Nov 2018

By plotting a histogram of the Beto ads, I can get a sense of their distribution across time. Unsurprisingly, the ads seem to be ramping up as the midterm elections approach. 

```{r}
# Descriptive statistics
summary(betocomments)

# Histogram of Facebook ads by date
ggplot(betocomments, aes(date)) + geom_histogram(bins = 300)
```

### Visualizing waves of political ads over time

One way to visualize these Beto O'Rourke ads is to plot them by "advertiser" over time. I can also map the "impressions" to the size of the dot. Impressions is described by ProPublica as the "number of times the ad has been seen by the Political Ad Collector." It's not a measure of how many interactions the ad saw. (That would be *very* interesting.) 

```{r}
betowaves <- ggplot(betocomments, aes(date, advertiser, text = message, impressions = impressions)) + 
  geom_point(aes(size=impressions)) 
ggplotly(betowaves, tooltip=c("text","impressions"))
```

The "plotly" package allows us to make this plot interactive. Roll over a dot to see the message of individual ads. Some interesting lines:

* "Republicans are DROWNING him in attack ads" from **End Citizens United**.
* "O'Rourke is TIED in the polls, so we need to make sure EVERY Democrat actually gets out to vote" from **Progressive Turnout Project**. 
* "Beto is TOO extreme for Texas" from **Ted Cruz**.

### Plotting top advertisers 

By counting the "advertiser" column and plotting the results, I can get a sense of who's driving "Beto" ads in this sample. 

```{r}
top_advs <- betocomments %>%
  count(advertiser) %>% 
  arrange(desc(n)) %>%
  top_n(10)

ggplot(top_advs, aes(reorder(advertiser, n), n)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("advertiser") +
  ggtitle("Top 10 advertisers associated with Beto O'Rourke Facebook ads")
```

### Plotting top words and phrases

Capitalized words like DROWNING, TOO and TIED are pretty interesting. Let's extract the most common uppercase words that are three or more characters long. Notable: PAC, NRA, DESTROY, SAVE, LOSE, HUGE, FLOOD. 

```{r}
capitalwords <- betocomments %>%
  unnest_tokens(word, message, to_lower = FALSE) %>%
  filter(str_detect(word, "\\b[A-Z]{3,}\\b")) %>% 
  count(word) %>%
  arrange(desc(n)) %>% 
  glimpse()

top_20_capitalwords <- capitalwords %>% top_n(20) 

ggplot(top_20_capitalwords, aes(reorder(word, n), n)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("") +
  ggtitle("Top capitalized words appearing in Beto O'Rourke Facebook ads")
```


I can also search for and plot the top uni-, bi-, tri- and four-word phrases.  

```{r}
unigrams <- betocomments %>%
  unnest_tokens(word, message, to_lower = FALSE) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "span")) %>%
  filter(!str_detect(word, "br")) %>%
  filter(!str_detect(word, "https")) %>%
  count(word) %>%
  arrange(desc(n)) %>%
  top_n(20)

ggplot(unigrams, aes(reorder(word, n), n)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("") +
  ggtitle("Top 20 words in Beto O'Rourke Facebook ads")

bigrams <- betocomments %>% 
  unnest_tokens(ngram, message, token = "ngrams", n = 2, to_lower = FALSE) %>%
  filter(!str_detect(ngram, "span")) %>%
  filter(!str_detect(ngram, "p")) %>%
  count(ngram) %>%
  arrange(desc(n)) %>%
  top_n(20)

ggplot(bigrams, aes(reorder(ngram, n), n)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("") +
  ggtitle("Top 20 two-word phrases in Beto O'Rourke Facebook ads")

trigrams <- betocomments %>% 
  unnest_tokens(ngram,message, token = "ngrams", n = 3, to_lower = FALSE) %>%
  filter(!str_detect(ngram, "span")) %>%
  count(ngram) %>%
  arrange(desc(n)) %>%
  top_n(20)

ggplot(trigrams, aes(reorder(ngram, n), n)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("") +
  ggtitle("Top 20 three-word phrases in Beto O'Rourke Facebook ads")

fourgrams <- betocomments %>% 
  unnest_tokens(ngram,message, token = "ngrams", n = 4, to_lower = FALSE) %>%
  filter(!str_detect(ngram, "span")) %>%
  count(ngram) %>%
  arrange(desc(n)) %>%
  top_n(20)

ggplot(fourgrams, aes(reorder(ngram, n), n)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("") +
  ggtitle("Top 20 four-word phrases in Beto O'Rourke Facebook ads")
```



### Sentiment analysis 

Sentiment analysis is an interesting way to get a 20,000-foot view of the tone of a corpus. In this case, I'll use it to understand how positive or negative the ads are over time and by advertiser. First, I'll split the ads into their component words and score them using Reagan's "labMT" dictionary. 

```{r}
# Tokenize ads
tokenized_comments <- betocomments %>%
  select(paid_for_by, advertiser, date, impressions, title, message) %>%
  unnest_tokens(word, message) %>%
  anti_join(stop_words) %>%
  group_by(word, paid_for_by, advertiser, date, impressions, title) %>%
  tally() %>%
  arrange(desc(n))

#Bring in sentiment dictionary
sentiments <- read.csv("labMT2english.csv", sep ="\t" )
labMT <- sentiments %>%
  select(word, happs)

# Score words from ads
all_sentiment <- tokenized_comments %>%  
  inner_join(labMT, by = "word") %>%
  group_by(word, paid_for_by, advertiser, date, impressions, title) %>%
  summarize(sentiment = mean(happs)) %>%
  arrange(desc(sentiment)) %>%
  mutate("score" = sentiment-5.372) 

all_sentiment %>% glimpse()
```


### Plotting the sentiment of "Beto" Facebook ads

Next, I'll plot this a few different ways. I'll first plot the individual scored words across time and by sentiment score and then plot the sentiment trend over time.   

```{r}
ggplot(all_sentiment, aes(date, score)) + geom_smooth() + geom_point() 
ggplot(all_sentiment, aes(date, score)) + geom_smooth() 
```


### Sentiment of "Beto" Facebook ads over time by advertiser

To split this out by advertiser, I can plot the following:

```{r}
ggplot(all_sentiment, aes(date, score, color=score)) + geom_point() + geom_smooth() +
  facet_wrap(~advertiser) +
  scale_color_gradient(low= "red", high="blue") +
  ggtitle("Sentiment of Beto O'Rourke Facebook ads by advertiser over time")
```


### Sentiment of advertisers mentioning "Beto"

I can also calculate the average sentiment of advertisers mentioning Beto O'Rourke and plot a sorted bar chart.


```{r}
sent_advertiser <- all_sentiment %>%
  group_by(advertiser) %>%
  summarize("avgsent" = mean(score)) %>%
  arrange(desc(avgsent))

ggplot(sent_advertiser, aes(reorder(advertiser, avgsent), avgsent, fill=avgsent >0)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("darkgray", "black")) + 
  theme(legend.position = 'none') +
  ylab("Average sentiment") +
  xlab("") +
  ggtitle("Average sentiment of advertisers mentioning Beto O'Rourke")+
  coord_flip()
```


# Sentiment of capitalized words over time

After isolating the capitalized words I can score them by sentiment and plot them over time. 

```{r}
labMTupper <- labMT %>%
  mutate("word" = toupper(word))

sent_capitalwords <- betocomments %>%
  mutate("post" = message) %>%
  unnest_tokens(word, message, to_lower = FALSE) %>%
  filter(str_detect(word, "\\b[A-Z]{3,}\\b")) %>%
  inner_join(labMTupper, by = "word") %>%
  group_by(word, post, paid_for_by, advertiser, date, impressions, title) %>%
  summarize(sentiment = mean(happs)) %>%
  arrange(desc(sentiment)) %>%
  mutate("score" = sentiment-5.372)

ggplot(sent_capitalwords, aes(date, score)) + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1) +
  geom_smooth() 
```


To allow exploration, I used the "plotly" package and mapped text, post and advertiser columns to the points. 

```{r}
capwords <- ggplot(sent_capitalwords, aes(date, score, text = word, post=post, advertiser = advertiser)) + 
  geom_point() + geom_hline(yintercept =0, lty = 2)
ggplotly(capwords, tooltip=c("text", "post", "advertiser"))
```




# References

Reagan, A.J., Danforth, C.M., Tivnan, B. et al. EPJ Data Sci. (2017) 6: 28. https://doi.org/10.1140/epjds/s13688-017-0121-9

Silge, J., & Robinson, D. (2016). tidytext: Text mining and analysis using tidy data principles in r. The Journal of Open Source Software, 1(3).

Wickham, H. (2014). Tidy Data. Journal of Statistical Software, 59(10), 1–23. doi:10.18637/jss.v059.i10




