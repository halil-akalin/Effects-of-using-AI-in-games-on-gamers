#Accessing the data set
setwd("D:/data_analysis/data_processing")
getwd()
file_path <- "cleaned_stop_words_and_conjunctions.txt"
text_content <- readLines(file_path)
#<------------------------------------------------------------------------------------------------------>

#Tokenized process
library(tokenizers)
#Function written to tokenize text
tokenized_text <- function(text) {
  tokens <- unlist(tokenize_words(text))
  return(tokens)
}
tokens <- lapply(text_content, tokenized_text)
print(tokens)
#The process of combining tokens
all_tokens <- unlist(tokens)

#Word frequency operation
word_freq <- table(all_tokens)
#Sorting word frequency from largest to smallest
sorted_word_freq <- sort(word_freq, decreasing = TRUE)
head(sorted_word_freq, 10)
#<------------------------------------------------------------------------------------------------------>

#Data visualization phase
library(wordcloud)
#Merge tokens
all_tokens <- unlist(tokens)
#Calculate word frequency
word_freq <- table(all_tokens)
##Sorting word frequency from largest to smallest
sorted_word_freq <- sort(word_freq, decreasing = TRUE)
#Creating a word cloud
wordcloud(words = names(sorted_word_freq), freq = sorted_word_freq, scale=c(3,0.5), min.freq = 15, colors=brewer.pal(8, "Dark2"))
#<------------------------------------------------------------------------------------------------------>

#Drawing phase of the bar chart of tokenized data
library(ggplot2)
print(sorted_word_freq)
#Creating a data frame with the 20 most used words in the dataset
top_words <- head(sorted_word_freq, 20)
df <- data.frame(word = names(top_words), Freq = as.numeric(top_words))
print(df)

#Plot a bar chart
ggplot(df, aes(x = Freq, y = reorder(word, Freq))) +
  geom_bar(stat = "identity", fill = "blue") +
  ggtitle("En Cok KullanD1lan Kelimelerin FrekanslarD1") +
  ylab("Kelime") +
  xlab("Frekans") +
  theme(axis.text.y = element_text(hjust = 1))
#<------------------------------------------------------------------------------------------------------>

#Sentiment analysis process 
library(tidytext)
library(tidyverse)
library(dplyr)
#Afinn emotion dictionary
afinn <- get_sentiments("afinn")
data_tidy <- data.frame(word = all_tokens)

#Scaling afinn emotion dictionary between -1 and +1
scale_factor <- 1/5
#Converting and scaling dataframe to tidy format
data_tidy_scaled <- data_tidy %>%
  inner_join(afinn, by = "word") %>%
  mutate(value = scale_factor * value)
#Collect emotion scores
total_sentiment_score_scaled <- sum(data_tidy_scaled$value)
print(data_tidy_scaled)
cat("Total sentiment score (scaled):", total_sentiment_score_scaled, "\n")
#<------------------------------------------------------------------------------------------------------>

#Data visualization based on sentiment analysis
library(ggplot2)
library(dplyr)

#Pie chart
library(ggplot2)
ggplot(data_tidy_scaled, aes(x = "", y = value, fill = ifelse(value > 0, "Positive", "Negative"))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  ggtitle("Duygu SkorlarD1") +
  theme_void() +
  theme(legend.position = "right") +
  scale_fill_manual(values = c("Positive" = "blue", "Negative" = "orange"))
#<------------------------------------------------------------------------------------------------------>

library(ggplot2)
#The process of listing the most recurring words
top_words <- names(sorted_word_freq)[1:220]
#Create a data frame containing selected words
selected_words_data <- data.frame(word = top_words)
#Conducting sentiment analysis by adding frequencies
selected_words_data <- selected_words_data %>%
  inner_join(afinn, by = "word") %>%
  mutate(frequency = sorted_word_freq[word],
         value = scale_factor * value)

#Bar chart
ggplot(selected_words_data, aes(x = reorder(word, -value), y = frequency, fill = ifelse(value > 0, "Positive", "Negative"))) +
  geom_bar(stat = "identity", color = "white") +
  ggtitle("Duygu Analizine Gore En Cok Tekrar Eden Kelimelerin Duygu ADD1rlD1DD1") +
  xlab("Kelime") +
  ylab("Frekans") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#<------------------------------------------------------------------------------------------------------>