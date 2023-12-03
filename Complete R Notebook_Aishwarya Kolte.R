##### This is a complete R notebook, right from data cleaning to business insights. ######

############################ Data Cleaning ##################################
#Getting the current working directory
getwd()
#Setting new working directory
setwd('C:/Users/DELL/Downloads')

# Loading the readr package
library(readr)

#Import the dataset and specifying the column types for "Price" as numeric and "Rate" as integer, treating "NA" as missing values.
flipkart <- read_csv("flipkart_product.csv", col_types = cols(Price = col_number(), Rate = col_integer()), na = "NA")
View(flipkart)

# Removing Rows with missing values
sum(is.na(flipkart))
flipkart <- na.omit(flipkart)

# Checking the structure of the Summary Column
str(flipkart$Summary)

# Now we have to remove punctuation from summary column
library(dplyr)
library(stringi) # for advanced string manipulation 

# The stri_trans_general function from the stringi package is used here to convert non-Latin characters to their closest Latin equivalents.
flipkart <- flipkart %>%
  mutate(
    Summary = stri_trans_general(Summary, "Latin-ASCII"),
    ProductName = stri_trans_general(ProductName, "Latin-ASCII"))

# Removing the punctuation
flipkart <- flipkart %>%
  mutate(Summary = gsub("[[:punct:]]", "", Summary))
View(flipkart)

######## Removing characters with undefined names
str(flipkart)
library(dplyr)
library(stringr)

#This gsub function removes all characters that are not letters, digits, spaces, or forward slashes.
flipkart <- flipkart %>%
  mutate(
    ProductName = gsub("[^a-zA-Z0-9/ ]", "", ProductName))
View(flipkart)

# The gsub function replaces characters with undefined names in the specified columns with a space.
flipkart <- flipkart %>%
  mutate(
    Review = gsub('[^a-zA-Z0-9(/)]', ' ', Review),
    Summary = gsub('[^a-zA-Z0-9(/)]', ' ', Summary))

# The 'str_squish' function collapses multiple adjacent white spaces into a single space.
flipkart <- flipkart %>%
  mutate(
    Summary = str_squish(Summary),
    Review = str_squish(Review))

# Converting the summary and review columns into lower case.
flipkart <- flipkart %>%
  mutate(
    Summary = tolower(Summary),
    Review = tolower(Review))

View(flipkart)



################################  EDA  ########################################
#Importing and Loading the necessary libraries and Dataset
library(ggplot2)
library(tm)
library(wordcloud)

#Dimensions of the dataset
dim(flipkart)     # ( 189869 rows, 5 columns)

#Check the structure of the each column
str(flipkart)

# This dataset is already cleaned in the last component(i.e. Data Cleaning).
# Therefore there are no null values in this dataset.


####### Summary Statistics ######
summary(flipkart)

#From the Summary Statistics, we can infer following things
#Price Range: The minimum price is 59, while the maximum is 86990, indicating a wide range of products with varying prices.
#The first quartile (25th percentile) is at 339, and the third quartile (75th percentile) is at 3399. 
#This confirms a positively skewed distribution with a concentration of products at lower price points.
#Ratings Column: The minimum rating is 1, the maximum is 5, and the mean is approximately 4.11. 
#This indicates a generally positive rating trend, with most products having high ratings.
#The third quartile (75th percentile) being 5 suggests that a significant portion of products has received the highest rating, implying high customer satisfaction.


####### Data Visualization #######

#Lets get histogram for price distribution
hist(flipkart$Price, col = "purple", main = "Price Distribution", xlab = "Price")
#This validates the fact that the products are concentrated at lower price points.


#Lets get barplot for the ratings distribution
barplot(table(flipkart$Rate), col = "red", main = "Ratings Distribution", xlab = "Ratings", ylab = "Frequency")
#This verifies the fact that the majority of products have received the highest rating.


#Lets get Scatter plot for Price and Ratings.
ggplot(flipkart, aes(x = Price, y = Rate)) +
  geom_point(alpha = 0.5, color = 'blue') +
  labs(title = 'Scatter Plot of Ratings vs. Price', x = 'Price', y = 'Ratings') +
  theme_minimal()

#Lets get Boxplot for Ratings by Price Range  
boxplot(flipkart$Rate ~ cut(flipkart$Price, breaks = c(0, 500, 1000, 2000, 5000, Inf)), col = "pink", main = "Boxplot: Ratings by Price Range", xlab = "Price Range", ylab = "Ratings")
#The medians are relatively consistent across different price ranges, it indicates that the price alone may not be a strong factor influencing customer ratings. 
#Other factors, such as product features or customer service, might play a more significant role.


########### word Cloud ##########

#Creating Corpus
corpus <- Corpus(VectorSource(flipkart$Review))

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

#This suggests that majority of customers are happy with their purchase and are highly satisfied.


########## Feature Engineering ##############

##### Categorize the Product Ratings
flipkart <- flipkart %>%
  mutate(RatingCategory = case_when(
    Rate == 5 ~ 'Excellent',
    Rate == 4 ~ 'Good',
    Rate == 3 ~ 'Average',
    Rate == 2 ~ 'Poor',
    Rate == 1 ~ 'Very Poor'))

# Count the occurrences of each rating category
rating_counts <- flipkart %>%
  count(RatingCategory)
rating_counts

# Calculate percentage of counts
rating_counts$percentage <- (rating_counts$n / sum(rating_counts$n)) * 100

# Lets create a pie chart using ggplot2
ggplot(rating_counts, aes(x = "", y = n, fill = RatingCategory)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  theme_void() +
  labs(
    title = 'Distribution of Product Ratings',
    subtitle = "Percentage of Counts",
    caption = "Note: Percentages are rounded for display"
  ) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5)) +
  theme(legend.position = "bottom")

#The majority of customers (57.2%) gave an "excellent" rating, indicating high satisfaction. 
#However, 21.9% of customers expressed average or below-average satisfaction. 
#While most customers are satisfied, there is room for improvement in certain areas.



########## Sentiment Analysis ###########
library(sentimentr)
sentiment_scores <- sentimentr::sentiment(flipkart$Summary)

Sentiment_analysis <- sentiment_scores %>% group_by(element_id) %>% 
  summarise(word_count = sum(word_count), mean_sentiment = mean(sentiment))
View(Sentiment_analysis)

# In order to merge two dataframes with no common column,adding row numbers as identifiers to both dataframes
flipkart$flipkart_row <- seq_len(nrow(flipkart))
Sentiment_analysis$element_id <- seq_len(nrow(Sentiment_analysis))
# Merge dataframes based on row numbers
flipkart_with_sentiment <- left_join(flipkart, Sentiment_analysis, by = c("flipkart_row" = "element_id"))
# Remove the temporary row identifier
flipkart_with_sentiment$flipkart_row <- NULL
#Display the updated dataframe
View(flipkart_with_sentiment)


#If 'Polarity' or 'Sentiment Score' is greater than 0, it identifies as 'Positive'; if less than 0, it identifies as 'Negative'; 
#otherwise, it identifies as 'Neutral'
flipkart_with_sentiment <- flipkart_with_sentiment %>%
  mutate(Sentiment_Polarity = case_when(mean_sentiment > 0 ~ 'Positive', mean_sentiment < 0 ~ 'Negative',
                                        TRUE ~ 'Neutral'))
View(flipkart_with_sentiment)

#Create a bar plot
ggplot(flipkart_with_sentiment, aes(x = Sentiment_Polarity, fill = Sentiment_Polarity)) +
  geom_bar() +
  scale_fill_manual(values = c('Positive' = '#98FB98', 'Negative' = '#FF6961', 'Neutral' = '#FFFF99')) +
  labs(title = 'Sentiment Distribution based on Polarity', x = 'Sentiment', y = 'Count') +
  theme_minimal()

#The majority of customers express positive sentiments about the products or services.
#Positive sentiments suggest that customers are satisfied with their experiences.


############################### Conclusion #########################################
# Based on the exploratory data analysis (EDA) and sentiment analysis of the Flipkart dataset, 
# we can derive the following business insights:

# Major Positive Points:
# 1) Affordable Product Range:
# Majority of products are concentrated at lower price points, catering to a broad customer base.
# 2)High Ratings:
# Significant portion of products receives the highest rating of 5, indicating positive customer satisfaction.
# 3)Positive Sentiment:
# Word cloud and sentiment analysis show a generally positive sentiment in customer reviews.

# Major Negative Points:
# 1)Neutral or Negative Sentiments:
# A portion of customers expresses neutral or negative sentiments, suggesting areas for improvement.
# 2)Areas for Improvement:
# Notable percentage of customers express average or below-average satisfaction, indicating areas that need attention.

##### Conclusion
# Flipkart performs well with an affordable and diverse product range, receiving high ratings and positive sentiments. 
# However, addressing concerns of customers with neutral or negative sentiments and focusing on areas for improvement will contribute to sustained success.