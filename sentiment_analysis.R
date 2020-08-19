library(tidyverse)
library(ggplot2)
library(gridExtra)
library(tidytext)
library(wordcloud2)
library(knitr)
library(kableExtra)
library(formattable)

##Reading the csv file
prince_orig <- read.csv("prince_raw_data.csv",
                        stringsAsFactors = FALSE)

#Column names
names(prince_orig)

#selecting required columns for this project

prince <- prince_orig %>%
  select(lyrics = text,song,year,album,peak,
         us_pop = US.Pop, us_rnb = US.R.B)


#checking one row
glimpse(prince[139,])

##checking the total number of rows and columns
dim(prince)

#checking the structure of the text/lyrics
str(prince[139,]$lyrics,nchar.max = 300)

##Data cleaning (using gsub to replace the strings)
fix.contractions <- function(doc){
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not",doc)
  doc <- gsub("n't", " not",doc)
  doc <- gsub("'ll", " will",doc)
  doc <- gsub("'re", " are",doc)
  doc <- gsub("'ve", " have",doc)
  doc <- gsub("'m", " am",doc)
  doc <- gsub("'d", " would",doc)
  doc <- gsub("'s","",doc)
  return(doc)
}

##Appling the function of data cleaning on lyrics

prince$lyrics <- sapply(prince$lyrics,fix.contractions)


#Removing special characters

removeSpecialChars <- function(x) gsub("[^a-zA-Z0-0]", " ",x)


#Removing special characters

prince$lyrics <- sapply(prince$lyrics,removeSpecialChars)


#converting all the text to lower case

prince$lyrics <- sapply(prince$lyrics,tolower)

#checking the lyrics again
str(prince[139,]$lyrics,nchar.max = 300)



summary(prince)


##Creating a decade column using ifelse
prince <- prince %>%
  mutate(decade =
           ifelse(prince$year %in% 1978:1979,"1970s",
           ifelse(prince$year %in% 1980:1989,"1990s",
           ifelse(prince$year %in% 1990:1999,"1990s",
           ifelse(prince$year %in% 2000:2009,"2000s",
           ifelse(prince$year %in% 2010:2015,"2010s",
                  "NA"))))))


#Creating a column for chart_level
prince <- prince %>%
  mutate(char_level =
           ifelse(prince$peak %in% 1:10, "Top 10",
           ifelse(prince$peak %in% 11:100, "Top 100",
                  "Uncharted")))

##New field to check if the song was charted or not

prince <- prince %>%
  mutate(charted = 
           ifelse(prince$peak %in% 1:100, "Charted", "Uncharted"))


write.csv(prince,file = "prince_new.csv")


##defining a theme for ggplots
my_color <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

theme_lyrics <- function(){
  
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}

#plotting the average number of songs w.r.t decade

prince %>%
  filter(decade != "NA") %>%
  group_by(decade, charted) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() +
  geom_bar(aes(x = decade, y = number_of_songs, fill = charted),
           stat = "identity") +
  theme(plot.title =  element_text(hjust = 0.5),
        legend.title = element_blank(),
        #used to remove the horizontal gridlines, 
        #can be used to color them too
        panel.grid.minor = element_blank(),
        # used to remove the vertical gridlines
        panel.grid.major = element_blank(),
        #used to remove the background from a plot
        panel.background = element_rect(fill = NA))+
  ggtitle("Released Songs")+
  labs(x = NULL, y = "Song Count")


#plotting the chart_level
charted_songs_over_time <- prince %>%
  filter(peak > 0) %>%
  group_by(decade, char_level) %>%
  summarise(number_of_songs = n())
  

  charted_songs_over_time %>%
    ggplot()+
    geom_bar(aes(x = decade, y = number_of_songs,
                 fill = char_level), stat = "identity") +
    theme(plot.title = element_text(hjust = 0.5),
          #to remove the title over legend
          legend.title = element_blank(),
          #used to remove the horizontal gridlines, 
          #can be used to color them too
          panel.grid.minor = element_blank(),
          # used to remove the vertical gridlines
          panel.grid.major = element_blank(),
          #used to remove the background from a plot
          panel.background = element_rect(fill = NA))+
    labs(x = NULL, y = "Song Count")+
  ggtitle("Charted Songs")

  
#plotting all the songs
  prince %>%
    group_by(decade, char_level) %>%
    summarise(number_of_songs = n()) %>%
    ggplot() +
    geom_bar(aes(x = decade, y = number_of_songs,
                 fill = char_level), stat = "identity") +
    theme(plot.title = element_text(hjust = 0.5),
          #to remove the title over legend
          legend.title = element_blank(),
          #used to remove the horizontal gridlines, 
          #can be used to color them too
          panel.grid.minor = element_blank(),
          # used to remove the vertical gridlines
          panel.grid.major = element_blank(),
          #used to remove the background from a plot
          panel.background = element_rect(fill = NA))+
    labs(x = NULL, y = "Song Count")+
    ggtitle("All songs in data")

  
#Pirnting the top songs


prince %>%
  filter(peak == "1") %>%
  select(year, song, peak) %>%
  arrange(year) %>%
  mutate(year = color_tile("lightblue","lightgreen")(year)) %>%
  mutate(peak = color_tile("lightgreen","lightgreen")(peak)) %>%
  kable("html",escape = FALSE, align = "c", caption = "Prince's No. 1 Songs")%>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"),
                full_width = FALSE)


# ?tidytext
# browseVignettes(package = "tidytext")
undesirable_words <- c("prince", "chorus", "repeat", "lyrics", 
                       "theres", "bridge", "fe0f", "yeah", "baby", 
                       "alright", "wanna", "gonna", "chorus", "verse", 
                       "whoa", "gotta", "make", "miscellaneous", "2", 
                       "4", "ooh", "uurh", "pheromone", "poompoom", "3121", 
                       "matic", " ai ", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats")

#eg of stopwords
head(sample(stop_words$word,15),15)

#creation of tidy dataframe
prince_words_filtered <- prince %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)
class(prince_words_filtered)

dim(prince_words_filtered)

#creating HTML for word race in each song
prince_words_filtered %>%
  filter(word == "race") %>%
  select(word, song, year, peak, decade, char_level, charted) %>%
  arrange() %>%
  top_n(10,song) %>%
  mutate(song = color_tile("lightblue","lightblue")(song)) %>%
  mutate(word = color_tile("lightgreen","lightgreen")(word)) %>%
  kable("html", escape = FALSE, align = "c", 
        caption = " Tokenized Format example") %>%
  kable_styling(bootstrap_options = 
                  c("stripped", "condensed", "bordered"),
                full_width = FALSE)

#word frequency 
full_word_count <- prince %>%
  unnest_tokens(word,lyrics) %>%
  group_by(song,char_level) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

full_word_count[1:10,] %>%
  ungroup(num_words,song) %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  mutate(song = color_tile("lightpink","lightpink")(song)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Songs With Highest Word Count") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)

full_word_count %>%
  ggplot()+
  geom_histogram(aes(x = num_words,
                     fill = char_level),binwidth = 20)+
  ylab("Song Count") +
  xlab("Word Count Per Song") +
  ggtitle("Word Count Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank() )


##Identifying word count per song
full_word_count %>%
  ggplot() +
  geom_histogram(aes(x = num_words, fill = 
                       char_level))+
  ylab("Song Count") +
  xlab("Word Count Per Song") +
  ggtitle("Word Count Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())

#TOP words
prince_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot() +
  geom_col(aes(word,n),fill = my_color[4])+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Song Count") +
  ggtitle("Most Frequently Used Words in Prince Lyrics") +
  coord_flip()


#Wordcloud

prince_words_counts <- prince_words_filtered %>%
  count(word,sort=TRUE)

wordcloud2(prince_words_counts[1:300,], size = 0.5)

wordcloud2(prince_words_counts[1:300,],
           figPath = "peaceAndLove.jpg",
           size = 1.5,color = "skyblue", backgroundColor="black")

letterCloud(prince_words_counts[1:300,],
            word = "Donald", size = 2)



library(devtools)
devtools::install_git("lchiffon/wordcloud2")

install.Rtools()
find_rtools()




install.packages("git")


writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
Sys.which("make")
