
library(wordcloud)
library(dplyr)

# Text analysis ---------------------------------------------------------

## Function cleaning text from unnessecery words, chars and expressions ect.
source("funs/cleanWords.R", local = TRUE, encoding = "UTF-8")

## polish stopwords
polish_stopwords <- readLines("polish.stopwords.txt")

## clean data texts
tweets2$text <- sapply(tweets2$text, cleanWords)
## remove empty tweets
tweets2 <- filter(tweets2, text != "")
## Determine separate words
words <- strsplit(tweets2$text, " ")

## clan words from stopwords
clean_words <- lapply(words, function(x) {
  res <- x[! str_to_lower(x)%in%polish_stopwords]
  return(res)
})

## Transform words from list to vector
all_words <- unlist(clean_words)

## Frequency of occurance of words
words_table <- table(all_words) %>%
  as.data.frame.table() %>%
  filter(all_words!="") %>%
  arrange(desc(Freq)) 

## Wordcloud of most often apearing words
wordcloud(words =  words_table$all_words, 
          freq = words_table$Freq, 
          min.freq = 10, 
          max.words = 100,
          scale = c(5, .5),
          colors = brewer.pal(9, "Set1")[rev(c(1,3,4,7, 9))])

