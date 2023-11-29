#####  Task 1 ########
##### Data Cleaning #####

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

# Saving the data frame flipkart to a CSV file
write.csv(flipkart, "clean_flipkart_Products.csv", row.names = FALSE)
