
## Funkcja oczyszczająca teksty z niechcianych znaków, wyrazów, wyrażeń itp.
## Dotyczy wpisów na twitterze

cleanWords <- function(words) {
  require(stringr)
  
  clean_words <- gsub("&amp", "", words)
  # clean urls
  clean_words <- gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", clean_words) 
  clean_words <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_words)
  # clean user names
  clean_words <- gsub("@\\w+", "", clean_words)
  # clean punctuation marks
  clean_words <- gsub("[[:punct:]]", " ", clean_words)
  # clean_words <- gsub("[[:digit:]]", "", clean_words)
  # clean_words <- gsub("http\\w+", "", clean_words)
  clean_words <- gsub("[ \t]{2,}", "", clean_words)
  # clean spaces on begin or end of expression
  clean_words <- gsub("^\\s+|\\s+$", "", clean_words)
  # clean_words = gsub("\\\\[A-Za-z0-9]+", "", clean_words)
  clean_words <- gsub('\\p{So}|\\p{Cn}', '', clean_words, perl = TRUE)
  # clean new line signs
  clean_words <- gsub("\\n", "", clean_words)
  
  # get rid hashtags
  clean_words <- gsub("poleng", "", clean_words, ignore.case = TRUE)
  clean_words <- gsub("engpol", "", clean_words, ignore.case = TRUE)
  clean_words <- gsub("u21euro", "", clean_words, ignore.case = TRUE)
  clean_words <- gsub("eurou21", "", clean_words, ignore.case = TRUE)
  
  
  #get rid of unnecessary spaces
  clean_words <- str_replace_all(clean_words," "," ")
  # Take out retweet header, there is only one
  clean_words <- str_replace(clean_words,"RT @[a-z,A-Z]*: ","")
  # Get rid of hashtags
  clean_words <- str_replace_all(clean_words,"#[a-z,A-Z]*","")
  # Get rid of references to other screennames
  clean_words <- str_replace_all(clean_words,"@[a-z,A-Z]*","")   
  
  return(clean_words)
}