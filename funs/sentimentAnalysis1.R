
sentimentAnalysis1 <- function(data, sentiments, time.var, text.var, stopwords) {
  
  ## load required packages
  require(lubridate)
  require(stringr)
  
  ## determine list of single words
  words <- strsplit(data[, text.var] , " ")
  
  ## Add time variable to these words. Needed to establish when word was typed.
  clean_words <- list(length(words))
  for(i in 1:length(words)) {
    clean_words[[i]] <- data.frame(text = words[[i]],
                                   time = data[i, time.var])
  }
  
  ## Tranform to data frame and get rid stop words
  words_all <- do.call("rbind", clean_words)
  words_all <- filter(words_all, !text %in% stopwords)
  
  ## Indicate start time and end time of first interval and indicate time threshold
  lower.time <- min(data[, time.var])
  upper.time <- lower.time + minutes(10)
  max.time <- max(data[ , time.var])
  
  ## Data frame with necessery columns
  emotion_sums <- data.frame(time = NULL, happiness = NULL, anger = NULL, sadness = NULL, fear = NULL, disgust = NULL)
  
  ## Loop that determine a mean emotions in 5 categories
  ## Result object is a data frame, that have 5 columns:
  ## upper time of each interval
  ## and mean of emotions for each category in particular interval
  while(upper.time<=max.time) {
    
    ## Reduction of data by time conditions
    temp.data <- words_all[ words_all$time >= lower.time & words_all$time <= upper.time , ]
    
    ## Add emotion variables to one interval individual results
    n <- nrow(temp.data)
    temp.data$happiness <- numeric(n)
    temp.data$anger <- numeric(n)
    temp.data$sadness <- numeric(n)
    temp.data$fear <- numeric(n)
    temp.data$disgust <- numeric(n)
    
    ## Match words from tweets and from sentiment data base and add emotion rates
    for(i in 1:n) {
      if( str_to_lower(temp.data[ i, "text" ]) %in% sentiments$word) {
        which_row <- sentiments$word==str_to_lower(temp.data[ i, "text" ])
        temp.data$happiness[i] <- sentiments[which_row , "mean.Happiness"]
        temp.data$anger[i] <- sentiments[which_row , "mean.Anger"]
        temp.data$sadness[i] <- sentiments[which_row , "mean.Sadness"]
        temp.data$fear[i] <- sentiments[which_row , "mean.Fear"]
        temp.data$disgust[i] <- sentiments[which_row , "mean.Disgust"]
      }
    }
    
    ## row results for one interval
    new_row <- data.frame(time = upper.time, 
                          happiness = mean(temp.data$happiness),
                          anger = mean(temp.data$anger),
                          sadness = mean(temp.data$sadness),
                          fear = mean(temp.data$fear),
                          disgust = mean(temp.data$disgust))
    ## add one interval results to general results
    emotion_sums <- rbind(emotion_sums, new_row)
    
    ## change interval to next level
    lower.time <- lower.time + minutes(5)
    upper.time <- upper.time + minutes(5)
  }
  
  return(emotion_sums)
}
