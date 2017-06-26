library(rtweet)

# Load and adjust data -------------------------------------------

file_name <- "poleng_tweets"
tweets <- parse_stream(file_name = paste0(file_name, ".json"))

tweets2 <- tweets %>%
  filter(!is_retweet) %>%
  filter(lang=="pl") %>%
  filter(!screen_name %in% c("iPourpreCroyant")) ## get rid a spammer

## Move time for 2 hours becouse of time zone
tweets2$created_at <- tweets2$created_at + 7200

## Create another variable with information of  in which part of match tweets was published
tweets2$when <- ""
tweets2$when <- ifelse(tweets2$created_at <= as.POSIXct("2017-06-22 20:45:00", "UTC"), "przed meczem", 
                       ifelse(tweets2$created_at <= as.POSIXct("2017-06-22 21:30:00", "UTC"), "I połowa",
                              ifelse(tweets2$created_at <= as.POSIXct("2017-06-22 21:45:00", "UTC"), "przerwa", 
                                     ifelse(tweets2$created_at <= as.POSIXct("2017-06-22 22:30:00", "UTC"), "II połowa", "po meczu"))))
tweets2$when <- factor(tweets2$when, levels = c("przed meczem", "I połowa", "przerwa", "II połowa", "po meczu"))

