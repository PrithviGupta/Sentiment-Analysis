# Sentiment Analysis on Shakespeare's plays

This code analyzes the sentiment of Shakespeare's plays using the dplyr, tidytext, and ggplot2 libraries in R. The data used in this analysis is contained in the shakespeare.rda file. The code performs the following tasks:

This code analyzes the sentiment of Shakespeare's plays using the dplyr, tidytext, and ggplot2 libraries in R. The data used in this analysis is contained in the shakespeare.rda file. The code performs the following tasks:

1. Load shakespeare.rda file into R and print the data.
2. Count the number of titles/types using the count() function.
3. Load the tidytext library.
4. Create a new tidy_shakespeare object by grouping by the titles of the plays, defining a new column linenumber, and transforming the non-tidy text data to tidy text data using the unnest_tokens() function.
5. Count how many times each word is used in tidy_shakespeare using the count() function and pipe it to the next line.
6. Perform sentiment analysis on tidy_shakespeare using the "bing" lexicon and create a new object shakespeare_sentiment.
7. Count the number of positive/negative words in each play using the count() function and title and sentiment columns of shakespeare_sentiment.
8. Perform sentiment analysis on tidy_shakespeare using the "bing" lexicon and count the number of words by title, type, and sentiment in a new object sentiment_counts.
9. Group sentiment_counts by the titles of the plays, find the total number of words in each play, calculate the number of words divided by the total, filter the results for only negative sentiment, and arrange the percentage in ascending order.
10. Count the most common positive and negative words in tidy_shakespeare using the "bing" lexicon and create a new object word_counts.
11. Extract the top 10 words from word_counts, group by sentiment, take the top 10 for each sentiment, make the word column a factor in order of n, and create a new object top_words.
12. Use ggplot2 library to create a bar chart of the top 10 words for each sentiment using word on the x-axis and n on the y-axis, and facet_wrap for sentiments with free scales. Finally, move x to y and y to x.
13. Calculate a contribution score for each word in each play in a new object sentiment_contributions using the "afinn" lexicon.
14. Create a word cloud of the sentiment analysis results using the wordcloud library.
    Note: The results of the analysis may vary depending on the sentiment lexicon used and the version of the libraries installed.
