#############  Task 2 ###############
##### EDA and Sentiment Analysis #####

#Setting new working directory
setwd('C:/Users/DELL/Downloads')
#Getting the current working directory
getwd()

# Loading the readr package
library(readr)
clean_flipkart_Products <- read_csv("clean_flipkart_Products.csv")
View(clean_flipkart_Products)

##### Analysis 1 
##### Analysis 1 is based on the ratings

##### Getting the frequencies of Rate Column
library(dplyr)
Ratings <- clean_flipkart_Products %>%
  filter(Rate %in% c(5, 4, 3, 2, 1))
rate_counts <- table(Ratings$Rate)
print(rate_counts)

#### Sentiment Mapping ########
#creating new column 'Sentiment' based on the mapping of the ratings
#If 'Ratings' given are 4 or 5, it identifies as 'Positive'; if 1 or 2, it identifies as 'Negative',
#if 3 , it identifies as 'Neutral'.
clean_flipkart_Products <- clean_flipkart_Products %>%
  mutate(Sentiment_Ratings = case_when(
    Rate %in% c(4, 5) ~ 'positive',
    Rate %in% c(1, 2) ~ 'negative',
    Rate == 3 ~ 'neutral',
    TRUE ~ as.character(Rate)))
View(clean_flipkart_Products)


##### Bar Plot ######
# Plotting the count of each sentiment category using ggplot2
library(ggplot2)

# Custom color palette for positive, negative, and neutral sentiments
colors <- c('positive' = '#1f78b4', 'negative' = '#e41a1c', 'neutral' = '#33a02c')

# Creating a bar plot to visualize the distribution of sentiments based on Ratings
ggplot(clean_flipkart_Products, aes(x = Sentiment_Ratings, fill = Sentiment_Ratings)) +
  geom_bar() +
  labs(title = 'Distribution of Sentiments based on Ratings', x = 'Sentiment', y = 'Count') +
  scale_fill_manual(values = colors) +  # Setting custom colors
  theme_minimal() +  # Using a minimal theme
  theme(
    text = element_text(family = 'Arial'),  # Setting font 
    plot.title = element_text(hjust = 0.5),  # Centering plot title
    legend.position = 'none')  # Remove legend


#######Analysis 2 ########
#Creating sentiments based on  polarity as Analysis2
#If 'Polarity' is greater than 0, it identifies as 'Positive'; if less than 0, it identifies as 'Negative'; 
#otherwise, it identifies as 'Neutral'.

library(sentimentr)
sentiment_scores <- sentimentr::sentiment(clean_flipkart_Products$Summary)

#Viewing the result
print(sentiment_scores)

polarity_score <- mean(sentiment_scores$sentiment)
polarity_score

library(dplyr)
Sentiment_analysis <- sentiment_scores %>% group_by(element_id) %>% 
  summarise(word_count = sum(word_count), mean_sentiment = mean(sentiment))
View(Sentiment_analysis)

Sentiment_analysis <- Sentiment_analysis %>%
  mutate(Sentiment_Polarity = case_when(mean_sentiment > 0 ~ 'Positive', mean_sentiment < 0 ~ 'Negative',
                                        TRUE ~ 'Neutral'))
View(Sentiment_analysis)

ggplot(Sentiment_analysis, aes(x = Sentiment_Polarity, fill = Sentiment_Polarity)) +
  geom_bar() +
  scale_fill_manual(values = c('Positive' = '#C5D4EB', 'Negative' = '#F8766D', 'Neutral' = '#A3A3A3')) +
  labs(title = 'Sentiment Distribution based on Polarity', x = 'Sentiment', y = 'Count') +
  theme_minimal()


#########  Generating Word Cloud ########

library(tm)
library(wordcloud)

#Creating Corpus
corpus <- Corpus(VectorSource(clean_flipkart_Products$Review))

#Making the list of custom stop_words
custom_stopwords <- c("flipkart","also","like","item","one","much","even","product")

#Preprocessing the text
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, custom_stopwords)

# Creating a term document matrix
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

# Creating the word cloud
wordcloud(d$word, d$freq, random.order = FALSE,rot.per = 0.3, scale = c(4,.5), 
          max.words=100, colors = brewer.pal(8,"Dark2"))
