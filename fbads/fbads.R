library(dplyr)
library(tidyverse)
library(tidytext)
library(plotly)
library(stringr)
library(ggridges)

# Pull in spreadsheet
df <- read.csv("fbads.csv", header=TRUE, stringsAsFactors = FALSE) 
df %>% glimpse()

# Prompt users to look for a specific term or politician
selectcomments <- df %>%
  filter(str_detect(message, "Beto"))  # input$keyword)

# Fix date
selectcomments$date <- as.Date(selectcomments$created_at) # add date column
selectcomments %>% glimpse()

#### Descriptive stats

# Histogram of Facebook ads by date
ggplot(selectcomments, aes(date)) + geom_histogram() 

# Plot of impressions over time
ggplot(selectcomments, aes(date, impressions)) + 
  geom_point() 


# A table look at messages with most impressions
mostimpressions <- selectcomments %>%
  select(title, message, advertiser, paid_for_by, date, impressions) %>%
  arrange(desc(impressions)) 
mostimpressions %>% glimpse()

# Look at successive waves of political ads 
selectcomments %>% glimpse()

betowaves <- ggplot(selectcomments, aes(date, advertiser, text = message, impressions = impressions)) + 
  geom_point(aes(size=impressions)) 

ggplotly(betowaves, tooltip=c("text","impressions"))

# Push to plotly removes Y axis names

#betowaves <- ggplotly(betowaves, tooltip=c("text","impressions"))
#api_create(betowaves, filename = "betowaves")




#### Top advertisers

# Table of top 20 "paid-for-by"
top_paidfors <- selectcomments %>%
  count(paid_for_by) %>% 
  arrange(desc(n)) %>%
  top_n(20)
top_paidfors

# Plot of top 20 "paid-for-by"
ggplot(top_paidfors, aes(reorder(paid_for_by, n), n)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("paid_for_by")

# Table of top 20 "advertiser"
top_advs <- selectcomments %>%
  count(advertiser) %>% 
  arrange(desc(n)) %>%
  top_n(20)
top_advs

# Plot of top 20 "advertiser"
ggplot(top_advs, aes(reorder(advertiser, n), n)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("advertiser")





##### Sentiment analysis 

# Tokenize messages
tokenized_comments <- selectcomments %>%
  select(paid_for_by, advertiser, date, impressions, title, message) %>%
  unnest_tokens(word, message) %>%
  anti_join(stop_words) %>%
  group_by(word, paid_for_by, advertiser, date, impressions, title) %>%
  # filter(word != "span") %>% # input$stopword1)
  # filter(word != "span") %>% # input$stopword2) 
  # filter(word != "span") %>% # input$stopword3)
  # filter(word != "span") %>% # input$stopword4)
  # filter(word != "span") %>% # input$stopword5)
  tally() %>%
  arrange(desc(n))

tokenized_comments %>% glimpse()

# Plot of sentiment over time  
sentiments <- read.csv("labMT2english.csv", sep ="\t" )
labMT <- sentiments %>%
  select(word, happs)

all_sentiment <- tokenized_comments %>%  
  inner_join(labMT, by = "word") %>%
  group_by(word, paid_for_by, advertiser, date, impressions, title) %>%
  summarize(sentiment = mean(happs)) %>%
  arrange(desc(sentiment)) %>%
  mutate("score" = sentiment-5.372) 

all_sentiment %>% glimpse()



# Plot sentiment over time
ggplot(all_sentiment, aes(date, score)) + geom_point() + geom_smooth() 


# Plot sentiment over time and split by advertiser
ggplot(all_sentiment, aes(date, score, color=score)) + geom_point() + geom_smooth() +
  facet_wrap(~advertiser) +
  scale_color_gradient(low= "red", high="blue")


# Joy division plot with advertisers



verbsplot <- ggplot(verbs, aes(x=date, y=sentiment2, text = message, outlet = outlet, color =sentiment2 >0)) +
  # geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") + # 1 day OR 3 days OR 1 week OR 1 month
  xlab("date") + 
  ylab("sentiment score") +
  ggtitle("Sentiment of verbs in news headlines on latest IPCC report") +
  theme(legend.position = 'none') +
  geom_smooth()

ggplotly(verbsplot, tooltip=c("text","outlet"))




# Same plot but allow users to remove one "paid_for_by"
remove1 <- all_sentiment %>%
  filter(paid_for_by != "Beto for Texas") # input$remove_paid)

ggplot(remove1, aes(date, score)) + geom_point() + geom_smooth()

# Look at sentiment of just one advertiser over time
sent_one_advertiser <- all_sentiment %>%
  filter(advertiser == "End Citizens United") #input$selectadvertiser
ggplot(sent_one_advertiser, aes(date, score)) + geom_point() + geom_smooth()


# top pos table

# top neg table






# Table of mean sentiment by paid_for_by

sent_paid_for_by <- all_sentiment %>%
  group_by(paid_for_by) %>%
  summarize("avgsent" = mean(score)) %>%
  arrange(desc(avgsent))
sent_paid_for_by

# Plot of mean sentiment by paid_for_by
ggplot(sent_paid_for_by, aes(reorder(paid_for_by, avgsent), avgsent, fill=avgsent >0)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("darkgray", "black")) + 
  theme(legend.position = 'none') +
  ylab("Average sentiment") +
  xlab("") +
  coord_flip()


# Table of mean sentiment by advertiser

sent_advertiser <- all_sentiment %>%
  group_by(advertiser) %>%
  summarize("avgsent" = mean(score)) %>%
  arrange(desc(avgsent))
sent_advertiser

# Plot of mean sentiment by advertiser
ggplot(sent_advertiser, aes(reorder(advertiser, avgsent), avgsent, fill=avgsent >0)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("darkgray", "black")) + 
  theme(legend.position = 'none') +
  ylab("Average sentiment") +
  xlab("") +
  coord_flip()









##### Finding unique terms 

# TF-IDF

tf_idf_words <- tokenized_comments %>%
  bind_tf_idf(word, paid_for_by, n) %>%
  filter(!grepl("\\d", word)) %>% # remove all digits
  filter(!grepl("http", word)) %>% # remove all http and https
  arrange(desc(tf_idf)) 

tf_idf_words %>% glimpse()

# Plot tf-idf over time with words annotated

ggplot(tf_idf_words, aes(date, tf_idf, color=tf_idf>0)) + 
  geom_point() + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1) +
  scale_color_manual(values=c("darkgray", "black")) +
  theme(legend.position = 'none') 

# Table of most "unique" words and their "paid for by"
tf_idf_words


# Exploring unique terms by advertiser

tf_df_by_ad <- tf_idf_words %>% 
  filter(advertiser == "Beto for Greater Houston")  %>%
  filter(tf_idf > 0)

ggplot(tf_df_by_ad, aes(reorder(word, tf_idf), tf_idf)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("term")


#### Exploring sentiment of unique terms

# Merge sentiment and TF-IDF 
sent_tfidf <- tf_idf_words %>%
  inner_join(labMT, by="word") %>%
  filter(tf_idf > 0) %>% 
  filter(word != "stephanie") %>% # prompt users to "remove outlier term"
  mutate("score" = happs-5.372)

ggplot(sent_tfidf, aes(tf_idf, score))  + 
  geom_point() + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1) +
  theme(legend.position = 'none') +
  geom_hline(yintercept = 0, lty = 2) 

# Export table of unique words and their sentiment
sent_tfidf


