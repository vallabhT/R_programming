library(ggplot2)
library(hrbrthemes)

value1 <- abs(rnorm(26))*2
don <- data.frame(
  x=LETTERS[1:26], 
  value1=value1, 
  value2=value1+1+rnorm(26, sd=1) 
) %>%
  rowwise() %>% 
  mutate( mymean = mean(c(value1,value2) )) %>% 
  arrange(mymean) %>% 
  mutate(x=factor(x, x))

don
trump_data_nrc
data2 <- sentiment_nrc %>%
  
  #Joining the overall count of sentiments
  
  inner_join(total_sentiment_position, by = "position") %>%
  
  #Filtering out sentiments which have low usage,
  #It can be commented out, to get a graph with all the senitments
  
  #filter(! sentiment %in%  c("Sadness","Disgust","Anger","Trust",
                             #"Anticipation")) %>%
  
  # Calculating the percentage of each sentiment over the position
  
  mutate(percent = sentiment_position_count / position_total * 100 ) %>%
  select(-c(sentiment_position_count, position_total)) %>%
  
  #Spreading the values over all the sentiments and position
  
  spread(position, percent)

data2 <- data2%>%
  mutate( mymean = mean(c(`As a president (2017-Present)`, 
                          `Before Presidency (2012-2016) `))) %>%
  arrange(mymean) %>%
  mutate(sentiment = factor(sentiment,sentiment))


names(data2) <- c("x","value1","value2","mymean")


data2
ggplot(data2) +
  geom_segment( aes(x=sentiment, xend=sentiment, y=`As a president (2017-Present)`,
                    yend=`Before Presidency (2012-2016) `), color="grey") +
  geom_point( aes(x=sentiment, y=`As a president (2017-Present)`),
              color=rgb(0.2,0.7,0.1,0.8), size=3 ) +
  geom_point( aes(x=sentiment, y=`Before Presidency (2012-2016) `),
              color=rgb(0.7,0.2,0.1,0.8), size=3 ) +
  coord_flip()+
  theme_ipsum() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("") +
  ylab("Value of Y")

ggplot(don) +
  geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color="grey") +
  geom_point( aes(x=x, y=value1), color=rgb(0.2,0.7,0.1,0.8), size=3 ) +
  geom_point( aes(x=x, y=value2), color=rgb(0.7,0.2,0.1,0.8), size=3 ) +
  coord_flip()



# Library
library(ggplot2)
library(dplyr)
library(hrbrthemes)

# Create data
value1 <- abs(rnorm(26))*2
data <- data.frame(
  x=LETTERS[1:26], 
  value1=value1, 
  value2=value1+1+rnorm(26, sd=1) 
)

# Reorder data using average? Learn more about reordering in chart #267
data <- data %>% 
  rowwise() %>% 
  mutate( mymean = mean(c(value1,value2) )) %>% 
  arrange(mymean) %>% 
  mutate(x=factor(x, x))

# Plot
ggplot(data) +
  geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color="grey") +
  geom_point( aes(x=x, y=value1), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=x, y=value2), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_ipsum() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Value of Y")

don


data2

x
data2 %>%
  arrange(value1)%>%
  ggplot() +
  geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color="grey") +
  geom_point( aes(x=x, y=value1), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=x, y=value2), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_ipsum() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Value of Y")















