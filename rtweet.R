library(rtweet)
library(ggplot2)
library(tidyverse)
rt <- search_tweets("#rstats",n=1000,include_rts = FALSE)
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
write_as_csv(rt,file_name = "tweet.csv")


