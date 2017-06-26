
library(dplyr)
library(tidyr)
library(ggplot2)

# Analiza wydźwięku -------------------------------------------------------

## list of polish words with their emotional content measured in sevaral scales
## data from http://exp.lobi.nencki.gov.pl/nawl-analysis
## applied threshold of indexes to determine these database are: H=3.5, A=5.8, S=6.0, F=5.6, D=6.3, N=3.5
polish.sentiment <- read.csv(file = "nawl-analysis-improved.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
polish.sentiment$category <- as.factor(polish.sentiment$category)

## polish stopwords
polish_stopwords <- readLines("polish.stopwords.txt")

## Bounds of time intervals when tweets were published
## Needed to generate interval background on plots
rect_edges <- tweets2 %>%
  group_by(when) %>%
  summarise(
    t_start = min(created_at),
    t_end = max(created_at)
  ) %>%
  as.data.frame()

## get rid unclasiffied words (optional)
# polish.sentiment <- filter(polish.sentiment, category!="U")

## function to determine mean values of word emotions in each 10 minutes wide interval
source("funs/sentimentAnalysis1.R", local = TRUE, encoding = "UTF-8")

emotions_means <- sentimentAnalysis1(data = tweets2, 
                                     time.var = "created_at", 
                                     text.var = "text", 
                                     sentiments = polish.sentiment, 
                                     stopwords = polish_stopwords)

## mean sentiment index for words in each interval
emotions_means %>%
  gather(key = emotion, value = value, 2:6) %>%
  ggplot(data = .) +
  geom_rect(data = rect_edges, aes(xmin = t_start, xmax = t_end, ymin = -Inf, ymax = Inf, fill = when), alpha = .2, inherit.aes = FALSE) +
  geom_line(aes(x = time, y = value, color = emotion, group = emotion)) +
  scale_fill_manual(values = brewer.pal(6, "Greys")[2:6]) +
  scale_color_manual(values = brewer.pal(5, "Set1"), 
                     labels = c("złość", "niesmak", "strach", "szczęście", "smutek")) +
  labs(x = "", y = "Wskaźnik") +
  guides(fill = guide_legend(title = "Kiedy"),
         color = guide_legend(title = "Jakie\nemocje:")) +
  theme_minimal()

## Smooth line instead of real data line
emotions_means %>%
  gather(key = emotion, value = value, 2:6) %>%
  ggplot(data = .) +
  geom_rect(data = rect_edges, aes(xmin = t_start, xmax = t_end, ymin = -Inf, ymax = Inf, fill = when), alpha = .2, inherit.aes = FALSE) +
  geom_smooth(aes(x = time, y = value, color = emotion, group = emotion), method = "loess", se = FALSE) +
  scale_fill_manual(values = brewer.pal(6, "Greys")[2:6]) +
  scale_color_manual(values = brewer.pal(5, "Set1"), 
                     labels = c("złość", "niesmak", "strach", "szczęście", "smutek")) +
  labs(x = "", y = "Średnie wskaźniki emocji") +
  guides(fill = guide_legend(title = "Kiedy"),
         color = guide_legend(title = "Jakie\nemocje:")) +
  theme_minimal()
