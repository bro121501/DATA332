# Consumer Complaints
## Contributors
<p>Blake Oliver</p>

## Introduction
<p>We will analyze the complaints from consumers of multiple banks to determine the most used negative word by the consumer according to their complaints.</p>

## Dictionary
1. NRC: Connotation association for words using 8 feelings
2. Bing: Connotation association for words using -5 to 5

## 1. Data Cleaning
1. Filtering the blanks out of the Consumer.complaint.narrative column
 ```
complaints <- complaints %>%
  filter(Consumer.complaint.narrative != '') #Filters out all of the blank strings in the column
```

2. Filtering to only the negative sentiments in both NRC and Bing
```
nrc_negative <- get_sentiments("nrc") %>% #Filters all of the nrc sentiments to
  filter(sentiment == "negative")         #the negative sentiments and creates a variable

bing_negative <- get_sentiments("bing") %>% #Filters all of the bing sentiments to the
  filter(sentiment == "negative")           #negative sentiments and creates a variable
```

3. Cleaning the punctuation from the words to only get the word
3.1 Creating function to clean the words
```
clean_text <- function(text) { #Deletes all punctuation
  cleaned_text <- gsub('[[:punct:]]', '', text)
  cleaned_text <- tolower(cleaned_text)
  return(cleaned_text)
}
```

3.2 Sends column through the function to clean the words
```
word_row$word <- clean_text(word_row$word) #Sends column word to the function
```

3.3 Filter out blanks again as in Section 1
```
word_row <- word_row %>% #Refilters out all the blank cells in column word
  filter(word != "")
```

## 2. Data Manipulation
1. Joining NRC sentiments and Bing sentiments to the clean words
```
nrc_sentiment <- word_row %>% #Joins the negative nrc sentiment to word_row
  inner_join(nrc_negative) %>%
  count(word, sort = TRUE)

bing_sentiment <- word_row %>% #Joins the negative bing sentiments to word_row
  inner_join(bing_negative) %>%
  count(word, sort = TRUE)
```

2. Filtering the joined data to get the top ten words used according to the NRC and Bing sentiments
```
top_ten_nrc_negative <- head(nrc_sentiment, 10) #Creates a dataframe with the top 10 negative nrc
                                                #sentiments
top_ten_bing_negative <- head(bing_sentiment, 10) #Creates a data frame with the top 10 negative
                                                  #bing sentiments
```

3. Combining data frames
3.1 Combining top ten words used according to the NRC and Bing sentiments together
```
top_ten_nrc_bing_combined <- rbind(top_ten_nrc_negative, top_ten_bing_negative) #Combines both top ten
                                                                                #negative data frames
                                                                                #into one
```

3.2 Combining duplicate words
```
top_ten_nrc_bing_combined <- top_ten_nrc_bing_combined %>% #Combines duplicate words
  group_by(word) %>%
  summarize(n = sum(n))
```

3.3 Reorders the combined data frame in order of the amount of times the word was used
```
top_ten_nrc_bing_combined <- top_ten_nrc_bing_combined[order(-top_ten_nrc_bing_combined$n), ] #Reorders
                                                                                              #based on
                                                                                              #amount of
                                                                                              #times used
```

3.4 Filter the combined data to the top ten results
```
top_ten_nrc_bing_combined <- head(top_ten_nrc_bing_combined, 10) #Creates a data frame with top 10 negative
                                                                 #sentiments of both nrc and bing sentiments
                                                                 #combined
```

3.5 Combining all of the negative words according to the NRC and Bing sentiments
```
total_sentiments <- rbind(nrc_sentiment, bing_sentiment) #Binds all of the negative words according to
                                                         #NRC and Bing from the complaints into one 
                                                         #data frame
```

3.6 Combine duplicate words
```
total_sentiments <- total_sentiments %>% #Combines duplicate words
  group_by(word) %>%
  summarize(n = sum(n))
```

3.7 Reorder based on amount of times used
```
total_sentiments <- total_sentiments[order(-total_sentiments$n), ] #Reorders based on amount of times
                                                                   #used
```

4. Create Graphs
4.1 Create graph showing "Top 10 NRC Negative Words from Customer Complaints"
```
#Creates bar chart showing Top 10 NRC Negative Words
ggplot(top_ten_nrc_negative, aes(x = word, y = n, fill = word)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -.5, colour = "black") +
  labs(title = "Top 10 NRC Negative Words from Customer Complaints", x = "Word", y = "Amount of Times Used") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")
```

4.2 Create graph showing "Top 10 Bing Negative Words from Customer Complaints"
```
#Creates bar chart showing Top 10 Bing Negative Words
ggplot(top_ten_bing_negative, aes(x = word, y = n, fill = word)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -.5, colour = "black") +
  labs(title = "Top 10 Bing Negative Words from Customer Complaints", x = "Word", y = "Amount of Times Used") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")
```

4.3 Create graph showing "Top 10 NRC and Bing Negative Words from Customer Complaints"
```
#Creates bar chart showing Top 10 NRC and Bing Negative Words
ggplot(top_ten_nrc_bing_combined, aes(x = word, y = n, fill = word)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -.5, colour = "black") +
  labs(title = "Top 10 NRC and Bing Negative Words from Customer Complaints", x = "Word", y = "Amount of Times Used") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")
```

4.4 Create wordcloud showing most used negative words according to the NRC and Bing sentiments
```
#Creates word cloud from total amount of negative words used in the customers complaints
wordcloud2(total_sentiments)
```

## Data Analysis

1. Top 10 NRC Negative Words from Customer Complaints
   <div align = "center">
   <img src = "https://github.com/bro121501/DATA332/blob/Consumer-Complaints/Top%2010%20NRC.png" width = "700")>
   </div>

- Payment was the number one used negative word according to the NRC negative sentiments followed by debt, in second, and late, in third.

2. Top 10 Bing Negative Words from Customer Complaints
   <div align = "center">
   <img src = "https://github.com/bro121501/DATA332/blob/Consumer-Complaints/Top%2010%20Bing.png" width = "700")>
   </div>

- Debt was the number on used negative word according to the Bing negative sentiments followed by complaint and dispute.

3. Top 10 NRC and Bing Negative Words from Customer Complaints
   <div align = "center">
   <img src = "https://github.com/bro121501/DATA332/blob/Consumer-Complaints/Top%2010%20Combined.png" width = "700")>
   </div>

- Debt was the number on used negative word according to both the NRC and Bing negative sentiments followed by payment and complaint.

4. Wordcloud showing the freqency of all negative words used by consumers according to the negative NRC and Bing sentiments
   <div align = "center">
   <img src = "https://github.com/bro121501/DATA332/blob/Consumer-Complaints/Wordcloud.png" width = "700")>
   </div>

- Debt was the number one used negative word according to both the NRC and Bing negative sentiments. Wrong, fraud, incorrect, and illegal are also noticable in the Worldcloud.
