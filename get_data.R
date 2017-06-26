# install.packages("rtweet")
# install.packages("httpuv")

library(rtweet)

# Generate token ----------------------------------------------------

consumer_key <- "TWÓJ CONSUMER KEY"
consumer_secret <- "TWÓJ CONSUMER SECRET"

twitter_token <- create_token(
  app = "TWOJA NAZWA APLIKACJI",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret
)

save(twitter_token, file = "twitter_token.RData")

load("twitter_token.RData")

# Settings --------------------------------------------------------------

t_start <- Sys.time() 
t_end <- as.POSIXct("2017-06-22 23:59")

t_sec <- round(as.numeric(t_end - t_start), 1) * 3600

file_name <- "poleng_tweets"

stream_tweets("#poleng",
              timeout = t_sec,
              file_name = file_name,
              parse = FALSE)

tweets <- parse_stream(file_name = paste0(file_name, ".json"))


