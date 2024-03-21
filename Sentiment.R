library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(wordcloud2)
library(reshape2)

rm(list = ls()) #Clears variables

setwd('H:\\Desktop Folder\\R Shit\\complaint') #Sets working directory

complaints <- read.csv('data/Consumer_Complaints.csv') 

complaints <- complaints %>%
  filter(Consumer.complaint.narrative != '') #Filters out all of the blank strings in the column

colnames(complaints)[colnames(complaints) == "Consumer.complaint.narrative"] <- "word" #Renames complaint column to word
                                                                                        #To create easier join later

nrc_negative <- get_sentiments("nrc") %>% 
  filter(sentiment == "negative")

bing_negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

word_row <- complaints %>%
  mutate(word = str_split(word, "\\s+"))
word_row <- unnest(word_row, word)

clean_text <- function(text) {
  cleaned_text <- gsub('[[:punct:]]', '', text)
  cleaned_text <- tolower(cleaned_text)
  return(cleaned_text)
}
word_row$word <- clean_text(word_row$word)

word_row <- word_row %>%
  filter(word != "")

nrc_sentiment <- word_row %>%
  inner_join(nrc_negative) %>%
  count(word, sort = TRUE)

bing_sentiment <- word_row %>%
  inner_join(bing_negative) %>%
  count(word, sort = TRUE)

top_ten_nrc_negative <- head(nrc_sentiment, 10)
top_ten_bing_negative <- head(bing_sentiment, 10)

ggplot(top_ten_nrc_negative, aes(x = word, y = n, fill = word)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -.5, colour = "black") +
  labs(title = "Top 10 NRC Negative Words from Customer Complaints", x = "Word", y = "Amount of Times Used") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")

ggplot(top_ten_bing_negative, aes(x = word, y = n, fill = word)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -.5, colour = "black") +
  labs(title = "Top 10 Bing Negative Words from Customer Complaints", x = "Word", y = "Amount of Times Used") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")

top_ten_nrc_bing_combined <- rbind(top_ten_nrc_negative, top_ten_bing_negative)

top_ten_nrc_bing_combined <- top_ten_nrc_bing_combined %>%
  group_by(word) %>%
  summarize(n = sum(n))

top_ten_nrc_bing_combined <- top_ten_nrc_bing_combined[order(-top_ten_nrc_bing_combined$n), ]

top_ten_nrc_bing_combined <- head(top_ten_nrc_bing_combined, 10)

ggplot(top_ten_nrc_bing_combined, aes(x = word, y = n, fill = word)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -.5, colour = "black") +
  labs(title = "Top 10 NRC and Bing Negative Words from Customer Complaints", x = "Word", y = "Amount of Times Used") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none")

total_sentiments <- rbind(nrc_sentiment, bing_sentiment)

total_sentiments <- total_sentiments %>%
  group_by(word) %>%
  summarize(n = sum(n))

total_sentiments <- total_sentiments[order(-total_sentiments$n), ]

wordcloud2(total_sentiments)

