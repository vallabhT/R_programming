library(rtweet)
library(ggplot2)
library(tidyverse)
rt <-   get_timeline("realDonaldTrump", n = 3200)%>%
  select(c(user_id,screen_name,text,created_at))


users_data(rt)

ts_plot(rt, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstats Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


ta <- trends_available()
levels(as.factor(ta$country))

gt <- get_trends("Australia")
gt

st <- stream_tweets("", ge)


syd <- lookup_coords("sydney")

melb <- lookup_coords("melbourne")

bendigo <- lookup_coords("")

vignette("rtweet")

?lookup_coords
rt <- search_tweets("",n=18000,geocode=melb)


rt <- rt%>%
  select(c(user_id,screen_name,text,source,hashtags,lang,place_name,
           place_full_name,geo_coords,coords_coords,bbox_coords))
write_as_csv(rt,file_name = "tweetdtrump.csv")

djt <- search_tweets(q = "realdonaldtrump", n = 10000) %>%
  select(c(user_id,screen_name,text,created_at))
as.Date(rt$created_at)


trump1 <- read.csv(file = file.choose())

trump2 <- read.csv(file = file.choose())

trump3 <- read.csv(file = file.choose())

length(trump1$tweet)
length(trump2$tweet)


mode(trump1)
mode(trump2)

finaltrump <- bind_rows(trump1,trump2)

write_as_csv(finaltrump,file_name = "trumptwitter_dataset.csv")





finaltrump$tweet <- fix.contractions(finaltrump$tweet)


finaltrump$tweet <- sapply(finaltrump$tweet,removeSpecialChars)


finaltrump$tweet <- sapply(finaltrump$tweet,tolower)



as.numeric(finaltrump$created_at)

mode(finaltrump$created_at[5000])

as.Date(finaltrump$created_at, "%Y/%m/%d %H:%M")

mode(rt$created_at[32])

typeof(rt$created_at)
