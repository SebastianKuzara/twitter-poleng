

sentimentAnalysis2 <- function(data, sentiments, time.var, text.var, stopwords) {
  
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
  
  ## Final result data frame with necessery columns
  category_table <- data.frame(time = lower.time, A = 0, D = 0, `F` = 0, H = 0, N = 0, S = 0, U = 0)
  
  
  ## Loop that determine a mean emotions in 7 categories
  ## Result object is a data frame, that have 7 columns:
  ## upper time of each interval
  ## and mean of emotions for each category in particular interval
  while(upper.time<=max.time) {
    
    ## Reduction of data by time conditions
    temp.data <- words_all[ words_all$time >= lower.time & words_all$time <= upper.time , ]
    
    ## Add emotion variables to one interval individual results
    ## category indicate to which category assign each word
    ## weight is value connected with elapsed time from typed tweet and upper treshold of time for interval
    ## the more time has passed means that the weight is lower and conversely
    n <- nrow(temp.data)
    temp.data$category <- factor(n, levels = levels(polish.sentiment$category))
    temp.data$weight <- numeric(n)
    
    ## determine appropriate values for each words
    for(i in 1:n) {
      if( str_to_lower(temp.data[ i, "text" ]) %in% sentiments$word) {
        which_row <- sentiments$word==str_to_lower(temp.data[ i, "text" ])
        temp.data$category[i] <- sentiments[which_row, "category"]
        ## Waga znaczenia słowa. Im starsza tym mniej ważna
        temp.data$weight[i] <- 1/sqrt(10 - as.numeric(difftime(temp.data$time[i], lower.time, units = "mins")) + 1)
        # temp.data$weight[i] <- 1
      }
    }
    
    ## determine mean weight for each category which occured
    ## and transform data to suitable form
    temp.data <- temp.data %>%
      filter(!is.na(category)) %>%
      group_by(category) %>%
      summarise(
        # index_sum = sum(weight)
        index_mean = mean(weight)
      ) %>%
      as.data.frame()
    rownames(temp.data) <- as.character(temp.data$category)
    category_row <- select(temp.data, 2) %>% t() %>% as.data.frame()
    
    ## Join data for one interval to general results
    category_row$time <- upper.time 
    rownames(category_row) <- 1
    category_table <- plyr::rbind.fill(category_table, category_row)
    
    ## Move to next interval
    lower.time <- lower.time + minutes(5)
    upper.time <- upper.time + minutes(5)
  }
  
  ## Remove unneseccery row
  category_table <- category_table[-1,]
  
  ## replace NA values to zeros
  for(i in 2:length(category_table)) {
    category_table[,i] <- ifelse(is.na(category_table[,i]), 0, category_table[,i])
  }
  
  return(category_table)
}