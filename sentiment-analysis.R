library(dplyr)

#1. load shakespeare.rda into r environment
load("shakespeare.rda")

shakespeare

#2. Pipe the shakespeare data frame to the next line
# Use count to find out how many titles/types there are

shakespeare %>% 
  count(title, type)

#3
library(tidytext)
#4. create and object tidy_shakespeare
# Group by the titles of the plays
# Define a new column linenumber
# Transform the non-tidy text data to tidy text data

tidy_shakespeare <- shakespeare %>%
  group_by(title,type) %>%
  unnest_tokens(word, text) %>% 
  ungroup()%>%
  mutate(linenumber = row_number())

tidy_shakespeare

#5. Pipe the tidy Shakespeare data frame to the next line
# Use count to find out how many times each word is used

tidy_shakespeare %>% 
  count(word, sort = TRUE)

#6. Sentiment analysis of tidy_shakespeare assin to object shakespeare_sentiment
# Implement sentiment analysis with the "bing" lexicon

shakespeare_sentiment <- tidy_shakespeare %>%
  inner_join(get_sentiments("bing"),multiple="all")  

shakespeare_sentiment

#7. shakespeare_sentiment
# Find how many positive/negative words each play has

shakespeare_sentiment %>%
  count(title, sentiment) 


#8. Tragedy or comedy from tidy_shakespeare  assign to sentiment_counts
# Implement sentiment analysis using the "bing" lexicon
# Count the number of words by title, type, and sentiment

sentiment_counts <- tidy_shakespeare %>%
  inner_join(get_sentiments("bing"),multiple = "all") %>% 
  count(title,type, sentiment)

sentiment_counts


#9. from sentiment_counts
# Group by the titles of the plays
# Find the total number of words in each play
# Calculate the number of words divided by the total
# Filter the results for only negative sentiment then arrange percentage in asc order

sentiment_counts %>%
  group_by(title) %>% 
  mutate(total = sum(n), 
         percent = n / total) %>% 
  filter(sentiment == "negative") %>% 
  arrange(percent)

#10 Most common positive and negative words and assign to word_count
# Implement sentiment analysis using the "bing" lexicon
# Count by word and sentiment

word_counts <- tidy_shakespeare %>%
  inner_join(get_sentiments("bing"),multiple = "all") %>%
  count(word, sentiment)

word_counts

#11. extract top 10 words from word_counts and assing to top_words
# Group by sentiment
# Take the top 10 for each sentiment and ungroup it
# Make word a factor in order of n


top_words <- word_counts %>%
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>%  
  mutate(word = reorder(word, n))

top_words

#12 Use aes() to put words on the x-axis and n on the y-axis
# Make a bar chart with geom_col()
# facet_wrap for sentiments and apply scales  as free
#Move x to y and y to x

library(ggplot2)
ggplot(top_words, aes(x = word, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()+labs(title="Top Words - 20BDS0179")


#13 from tidy_shakespeare Calculating a contribution score
# Count by title and word
# Implement sentiment analysis using the "afinn" lexicon
# Group by title
# Calculate a contribution for each word in each title

library(textdata)

sentiment_contributions <- tidy_shakespeare %>%
  count(title, word, sort = TRUE) %>%  
  inner_join(get_sentiments("afinn")) %>% 
  group_by(title)%>%   
  mutate(contribution = (n * value) / sum(n)) %>%  
  ungroup()

sentiment_contributions


#Wordcloud
library("wordcloud") 
set.seed(100)

shakespeare_wc<-shakespeare_sentiment %>% 
  count(word, sort = TRUE)
  

shakespeare_wc
par(mar = c(0, 0, 0, 0))

wordcloud(words = shakespeare_wc$word, freq = shakespeare_wc$n, min.n = 3, max.words=250,
          random.order=T, rot.per=0.30,
          colors=brewer.pal(8, "Dark2")) 
