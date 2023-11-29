########## Task 2 ############
###### EDA and Sentiment Analysis ######

# Installing Necessary packages
install.packages('Snowballc') # for text stemming
install.packages('wordcloud2') # for words cloud generator

#Loading the packages
library('tm')      # for text mining
library('Snowballc')  # for text stemming
library('wordcloud2')  # word cloud generator
library('RColorBrewer')  # color palettes
library('syuzhet')   # for sentiment analysis (for graph)
library('sentimentr') # for sentiment analysis 
library('ggplot2') #for plotting graph

# Read the dataset
setwd('C:/Users/DELL/Downloads')
getwd()
library(readr)
clean_flipkart_Products <- read_csv("clean_flipkart_Products.csv")
View(clean_flipkart_Products)

#creating sample of dataset in order to deal with memory error for matrix conversion
clean_flipkart_Products <- read.csv("clean_flipkart_Products.csv")
# Getting 5000 rows 
sample_rows <- sample(nrow(clean_flipkart_Products), 5000)
sample_clean_flipkart <- clean_flipkart_Products[sample_rows, ]
View(sample_clean_flipkart)

#Load the data as Corpus
SummaryDoc <- Corpus(VectorSource(sample_clean_flipkart$Summary))

####### Preprocessing the text #######
#Removing numbers from summary
SummaryDoc <- tm_map(SummaryDoc, removeNumbers)
#Removing English common words
SummaryDoc <- tm_map(SummaryDoc, removeWords, stopwords('english'))
stopwords()
#Removing customized stopwords
SummaryDoc <- tm_map(SummaryDoc, removeWords, c("flipkart","also","like","item","one","much","even","product"))
# Text Stemming, reducing the words to their root form
SummaryDoc <- tm_map(SummaryDoc, stemDocument)


###### Building the term document matrix #######
#Building a term_document matrix
SummaryDoc_dtm <- TermDocumentMatrix(SummaryDoc)
dtm_m <- as.matrix(SummaryDoc_dtm)
# Sorting by descending value of matrix
dtm_v <- sort(rowSums(dtm_m), decreasing = TRUE)
dtm_d <- data.frame(word =names(dtm_v), freq = dtm_v)
# Displaying the top 10 most frequest words
head(dtm_d,10)


######  Plotting the most frequent words ########
barplot(dtm_d[1:10,]$freq, las = 2, names.arg = dtm_d[1:10,]$word,
        col = 'lightgreen', main = 'Top 10 most frequent words',
        ylab = 'Word frequencies')


######  Generating Word Cloud ######
wordcloud2(dtm_d,
           size = 0.4,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

####### Word Association #######
# Finding associations
findAssocs(SummaryDoc_dtm, terms = c('good','qualiti','price'), corlimit = 0.10)

####### Sentiment Analysis #######
# Converting the corpus to a character vector
input <- unlist(sapply(SummaryDoc, function(x) as.character(x)))

# Converting to UTF-8 encoding
input <- iconv(input, to = "UTF-8")

# Getting sentiment scores
sentiment <- get_nrc_sentiment(input)

 # Display sentiment scores
print(head(sentiment, 20))

# Bar plot of sentiment scores
barplot(colSums(sentiment),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = "Sentiment Scores on Reviews")

###### Sentiment Analysis (based on Polarity Score) ########
#If 'Polarity' is greater than 0, it identifies as 'Positive'; if less than 0, it identifies as 'Negative'; 
#otherwise, it identifies as 'Neutral'.
sentiment_scores <- sentimentr::sentiment(sample_clean_flipkart$Summary)

#Viewing the result
print(sentiment_scores)

document_polarity_score <- mean(sentiment_scores$sentiment)
document_polarity_score

library(dplyr)
mygroup <- sentiment_scores %>% group_by(element_id) %>% 
  summarise(word_count = sum(word_count), mean_sentiment = mean(sentiment))
View(mygroup)

mygroup <- mygroup %>%
  mutate(Sentiment_Polarity = case_when(mean_sentiment > 0 ~ 'Positive', mean_sentiment < 0 ~ 'Negative',
    TRUE ~ 'Neutral'))
View(mygroup)

ggplot(mygroup, aes(x = Sentiment_Polarity, fill = Sentiment_Polarity)) +
  geom_bar() +
  scale_fill_manual(values = c('Positive' = '#C5D4EB', 'Negative' = '#F8766D', 'Neutral' = '#A3A3A3')) +
  labs(title = 'Sentiment Distribution based on Polarity', x = 'Sentiment', y = 'Count') +
  theme_minimal()
