###############################################################################
# Creating a data graphic in R (Assignment 1)
#
# Author: Sri Vallabh Tiwari
# Created 2020-08-22
###############################################################################


# List of packages which are being used in this script --------------------

packages <- c("tidyverse","lubridate","radarchart","tidytext")

# Installing packages and loading them ------------------------------------

package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


# Reading and loading data ------------------------------------------------

# Reading the main dataset which consists of tweets 
# The dataset is a combination of 3 datasets, the URLs are
# Dataset 1 <- https://www.kaggle.com/kingburrito666/better-donald-trump-tweets
# Dataset 2 <- https://github.com/ecdedios/into-heart-of-darkness
# Dataset 3 <- Was gathered by me using rtweets library
# The datasets have been formatted and binded for further use in the assignment
# The sentiment data set is taken from tidytex package, using the function
# get_sentiment("nrc")
# Extract the dataset folder from the zip file, update the path if needed

trump_data <- read.csv(file = 'Datasets/Datasets/trump_twitter_dataset.csv')

# Reading dataset which consists of NRC sentiments

sentiments <- read.csv(file = 'Datasets/Datasets/sentiments.csv')

#Checking the structure of the dataset

str(trump_data)

#Converting created_at column to Date class, it uses the lubridate library

trump_data$created_at <- mdy(trump_data$created_at)


# Adding new features to the dataset and text mining ----------------------

trump_data <- trump_data %>%
  
  # Adding year column which will identify the year of the tweet
  
  mutate(year = year(created_at)) %>%
  
  #Filtering out the years 2009, 2010, 2011 which have low number of tweets
  
  filter(! year %in% c(2009,2010,2011)) %>%
  
  # Removing the URLs from the tweets and storing in new column
  
  mutate(stripped_text = gsub("https.*", "", tweet)) %>%
  
  # Adding column which identifies the if the tweet was tweeted before 
  # Trump's presidency or after he was elected as a president
  
  mutate(position = ifelse(year < 2017, 
                           "Before Presidency (2012-2016) ","As a president (2017-Present)"))

#Set of words which are to be removed with the stop words
undesirable_words <- c("http","t.co","rt","amp")

# Generating tokens for each tweet and removing stop words

trump_data_words <- trump_data %>%
  # Tokenising each tweet
  unnest_tokens(word, stripped_text) %>%
  # Removing words which are occurring due to formatting error
  filter(! word %in% undesirable_words) %>%
  # Removing single length words/letters
  filter(nchar(word) > 1) %>%
  # Using anti_join to remove stop words provided by tidyverse
  anti_join(stop_words)



# Assigning Sentiments to each word ---------------------------------------

# The sentiments are based on NRC sentiments, they have been pre-downloaded 
# In the sentiments.csv

# Joining the senitments to each word in the tweet

trump_data_nrc <- trump_data_words %>%
  inner_join(sentiments)


# Calculating sentiment percent for his position ---------------------------

sentiment_nrc <- trump_data_nrc %>%
  # Grouping by year and sentiments
  group_by(position, sentiment) %>%
  # Counting the number of times each sentiment occurs in an year
  count(position, sentiment) %>%
  select(position, sentiment, sentiment_position_count = n)

# Calculating the total occurrences of sentiments over the position ------

total_sentiment_position <- trump_data_nrc %>%
  count(position) %>%
  select(position, position_total = n)


# Visualising using Radar Chart -------------------------------------------

# Generating the percentage of each sentiment over the position

position_radar_chart <- sentiment_nrc %>%
  
  #Joining the overall count of sentiments
  
  inner_join(total_sentiment_position, by = "position") %>%
  
  #Filtering out sentiments which have low usage,
  #It can be commented out, to get a graph with all the sentiments
  
  filter(! sentiment %in%  c("Sadness","Disgust","Anger",
                             "Anticipation")) %>%
  
  # Calculating the percentage of each sentiment over the position
  
  mutate(percent = sentiment_position_count / position_total * 100 ) %>%
  select(-c(sentiment_position_count, position_total)) %>%
  
  #Spreading the values over all the sentiments and position
  
  spread(position, percent) %>%
  
  #Creating the radar chart, using pre-defined color matrix
  
  chartJSRadar(showToolTipLabel = TRUE,
               main = "Sentiments of Tweets by Donald Trump",
               polyAlpha = 0.1, 
               lineAlpha = 2, 
               maxScale = 30,colMatrix = matrix(c(255,139, 125 , 0, 255, 255),
                                                byrow = F, nrow = 3),
               scaleStepWidth = 5)

#Plotting the graph

position_radar_chart












