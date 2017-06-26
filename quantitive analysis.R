
library(ggplot2)
library(dplyr)

# Quantitive analysis ----------------------------------


## Number of tweets depends on publish time
tweets2$when %>%
  table() %>%
  as.data.frame.table() %>%
  mutate(.data = ., Var1 = `.`) %>%
  select(2:3) %>%
  ggplot(data = ., aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Liczba tweetów") +
  theme_minimal()

## Bounds of time intervals when tweets were published
## Needed to generate interval background on plots
rect_edges <- tweets2 %>%
  group_by(when) %>%
  summarise(
    t_start = min(created_at),
    t_end = max(created_at)
  ) %>%
  as.data.frame()

## Cumultative number of tweets depends on time of pubilcation with time intervals
tweets2 %>%
  mutate(one = 1) %>%
  mutate(tweets.cumultative = cumsum(one)) %>%
  ggplot(data = ., aes(x = created_at, y = tweets.cumultative)) +
  geom_rect(data = rect_edges, aes(xmin = t_start, xmax = t_end, ymin = -Inf, ymax = Inf, fill = when), alpha = .4, inherit.aes = FALSE) +
  geom_line(aes(x = created_at, y = tweets.cumultative)) +
  labs(x = "", y = "skumulowana liczba tweetów") +
  theme_minimal() + 
  theme(legend.title = element_blank())

