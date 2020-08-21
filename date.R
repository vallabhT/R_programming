library(tidyverse)
library(wordcloud2)
library(tidytext)
library(date)
trumpdata <- read.csv(file = file.choose())

typeof(trumpdata$created_at)
str(trumpdata)
trumpdata$created_at <- as.Date.character(trumpdata$created_at,format = c("%m/%d/%Y"))

date.mdy(trumpdata$created_at)
