library(tidyverse)
library(wordcloud2)
library(tidytext)
library(date)
library(lubridate)
library(rtweet)
library(ggplot2)
library(textdata)
library(yarrr)
library(circlize)
library(ggrepel) #`geom_label_repel`
library(gridExtra) #`grid.arrange()` for multi-graphs
library(knitr) #Create nicely formatted output tables
library(kableExtra) #Create nicely formatted output tables
library(formattable) #For the color_tile function
library(radarchart) #Visualizations
library(igraph) #ngram network diagrams
library(ggraph) #ngram network diagrams
library(tidytext)


library(fmsb)




trumpdata <- read.csv(file = file.choose())

typeof(trumpdata$created_at)
str(trumpdata)


trumpdata$created_at <- mdy(trumpdata$created_at)

year(trumpdata_words_filtered$created_at)

trumpdata <- trumpdata %>%
  mutate(position = 
           ifelse(year(trumpdata$created_at) < 2017, "Before presidency",
           "As a president"))
'%ni%' <- Negate("%in%")

trumpdata <- trumpdata %>%
  filter( !year(created_at) %in% c(2009,2010,2011))

#Removing urls
trumpdata$stripped_text <- gsub("https.*","",trumpdata$tweet) 


trumpdata_words_filtered <- trumpdata %>%
  unnest_tokens(word,stripped_text) %>%
  filter(! word %in% undesirable_words2) %>%
  filter(nchar(word)>1) %>%
  anti_join(stop_words)

undesirable_words2 <- c("http","t.co","rt","amp","trump")

trumpdata_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(x = word,y = n)) +
  geom_col() +
  coord_flip()


bing_trump1 <- trumpdata_words_filtered %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%
  ungroup()

bing_trump1 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n,fill = sentiment)) +
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment,scales = "free_y") +
  labs(title= "Tweets from trump")+
  coord_flip()+theme_bw()



#func for sentiment of tweet
# sentiment_bing = function(twt) {
#   twt_tbl = tibble(text= twt) %>%
#     mutate(
#       stripped_text = gsub("https.*","",text)
#     )%>%
#     unnest_tokens(word,stripped_text) %>%
#     anti_join(stop_words) %>%
#     filter(! word %in% undesirable_words2) %>%
#     filter(nchar(word)>1) %>%
#     inner_join(get_sentiments("bing"))%>%
#     count(word,sentiment,sort = TRUE) %>%
#     ungroup() %>%
#     mutate(score =
#              case_when(
#                sentiment == 'negative' ~n*(-1),
#                sentiment == 'positive' ~n*1
#              ))
#   
#   sent.score = case_when(
#     nrow(twt_tbl) == 0 ~0,
#     nrow(twt_tbl) > 0 ~ sum(twt_tbl$score)
#   )
#   zero.type = case_when(
#     nrow(twt_tbl)==0 ~ " Type 1",
#     nrow(twt_tbl) >0 ~ " Type 2"
#   )
#   list(score = sent.score, type = zero.type,twt_tbl = twt_tbl)
# }


# trumpsent <- lapply(trumpdata$tweet, function(x){sentiment_bing(x)})



glimpse(trumpdata_words_filtered)


word_summary <- trumpdata_words_filtered %>%
  group_by(position, tweet, year(created_at)) %>%
  mutate(word_count = n_distinct(word)) %>%
  select(tweet,Tweeted = created_at,word_count) %>%
  distinct() %>%
  ungroup()

word_summary <- rename(word_summary, created_at = "year(created_at)")

pirateplot(formula = word_count ~ created_at+position,
           data = word_summary,
           xlab = NULL, ylab = "Tweet Distinct Word Count", #Axis labels
           main = "Lexical Diversity Per Year", #Plot title
           pal = "google", #Color scheme
           point.o = .2, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = .9, cex.names = .7) #Axis label size)



tweets_year <- trumpdata %>%
  select(tweet,created_at) %>%
  group_by(year(created_at)) %>%
  summarise(tweet_count = n ())

tweets_year
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}

new_sentiments <- get_sentiments(lexicon = c("nrc"))
names(new_sentiments)[names(new_sentiments) == 'value'] <- 'score'
new_sentiments <- new_sentiments %>% mutate(lexicon = "nrc",
                                            words_in_lexicon = n_distinct((word)))


new_sentiments2 <- get_sentiments(leixcon = c("bing", "afinn", "nrc"))


new_sentiments %>%
  group_by(lexicon,sentiment,words_in_lexicon) %>%
  summarise(distinct_words = n_distinct(word))%>%
  ungroup() %>%
  spread(sentiment,distinct_words) %>%
  mutate(lexicon = color_tile("lightblue", "lightblue")(lexicon),
         words_in_lexicon = color_bar("lightpink")(words_in_lexicon)) %>%
  my_kable_styling(caption = "Word Counts Per Lexicon")

glimpse(trumpdata_words_filtered)

trumpdata_words_filtered %>%
  mutate(number_of_words_in_tweets = n_distinct(word)) %>%
  inner_join(new_sentiments) %>%
  group_by(lexicon,number_of_words_in_tweets,words_in_lexicon) %>%
  summarise(lex_match_words = n_distinct(word)) %>%
  ungroup() %>%
  mutate(total_match_words = sum(lex_match_words), #Not used but good to have
         match_ratio = lex_match_words / number_of_words_in_tweets) %>%
  select(lexicon, lex_match_words,  number_of_words_in_tweets, match_ratio) %>%
  mutate(lex_match_words = color_bar("lightpink")(lex_match_words),
         lexicon = color_tile("lightgreen", "lightgreen")(lexicon)) %>%
  my_kable_styling(caption = "Lyrics Found In Lexicons")


new_sentiments2 <- get_sentiments("bing")

new_sentiments2 <- new_sentiments2 %>% mutate(lexicon = "bing",
                                            words_in_lexicon = n_distinct((word)))

new_sentiments3 <- get_sentiments("afinn")
names(new_sentiments3)[names(new_sentiments3) == 'value'] <- 'score'
new_sentiments3 <- new_sentiments3 %>% mutate(lexicon = "afinn",ifelse(score >=0,"positve","negative"),
                                            words_in_lexicon = n_distinct((word)))


trump_nrc <- trumpdata_words_filtered %>%
  inner_join(get_sentiments("nrc"))
trump_nrc  

trump_nrc <- trump_nrc%>%
  mutate(year = year(created_at))


trump_nrc %>%
  group_by(sentiment)%>%
  count()
trump_nrc

position_sentiment_nrc <- trump_nrc %>%
  # filter(! sentiment %in% c("positive","negative"))%>%
  group_by(position,sentiment) %>%
  count(position,sentiment) %>%
  select(position,sentiment,sentiment_position_count =n)
year_sentiment_nrc


total_sentiment_position <- trump_nrc %>%
  count(position) %>%
  select(position, position_total = n)
total_sentiment_year

position_radar_chart <- position_sentiment_nrc %>%
  inner_join(total_sentiment_position, by = "position") %>%
  filter(! sentiment %in% c("surprise","sadness","disgust","anger")) %>%
  mutate(percent = sentiment_position_count / position_total * 100 ) %>%
  select(-sentiment_position_count, -position_total) %>%
  spread(position, percent) %>%
  chartJSRadar(showToolTipLabel = TRUE,
               main = "Sentiment Analysis of Tweets by Trump")


position_radar_chart

year_sentiment_nrc <- trump_nrc %>%
  # filter(! sentiment %in% c("positive","negative"))%>%
  group_by(year,sentiment) %>%
  count(year,sentiment) %>%
  select(year,sentiment,sentiment_year_count =n)
year_sentiment_nrc


total_sentiment_year <- trump_nrc %>%
  count(year) %>%
  select(year, year_total = n)
total_sentiment_year

colMatrix = matrix(c(255,0,0,0,255,0,0,0,255), 
                   byrow = F,nrow = 3)

new_sentiments
write.csv(as.data.frame(new_sentiments),'sentiments.csv')
colMatrix

year_radar_chart <- year_sentiment_nrc %>%
  inner_join(total_sentiment_year, by = "year") %>%
 filter(! sentiment %in% c("surprise","sadness","disgust","anger","fear")) %>%
  filter( year %in% c("2014", "2017","2019"))%>%
  mutate(percent = sentiment_year_count / year_total * 100 ) %>%
  select(-sentiment_year_count, -year_total) %>%
  spread(year, percent) %>%
  chartJSRadar(showToolTipLabel = TRUE,
               main = "Sentiment Analysis of Tweets by Trump",
               polyAlpha = 0.1, 
               lineAlpha = 2, 
               maxScale = 30,colMatrix = colMatrix)


year_radar_chart

trump_data %>%
  group_by(year)%>%
  summarise(year_count = n())


year_sentiment_nrc <- year_sentiment_nrc %>%
  inner_join(total_sentiment_year, by = "year") %>%
  filter(! sentiment %in% c("surprise","sadness","disgust","anger")) %>%
  filter( year %in% c("2015", "2017","2019"))%>%
  mutate(percent = sentiment_year_count / year_total * 100 ) %>%
  select(-sentiment_year_count, -year_total)%>%
  spread(year,percent)


radarchart(year_sentiment_nrc)




year_sentiment_nrc <- as.data.frame(year_sentiment_nrc)


set.seed(99)
data <- as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
colnames(data) <- c("math" , "english" , "biology" , "music" , "R-coding" )
rownames(data) <- paste("mister" , letters[1:3] , sep="-")

data <- rbind(rep(20,5) , rep(0,5) , data)


year_sentiment_nrc <- rbind(rep(30,2) , rep(0,2) , year_sentiment_nrc)
data


radarchart(year_sentiment_nrc)
