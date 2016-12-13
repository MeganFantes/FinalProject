require(tm)
require(SnowballC)
require(wordcloud)
require(streamR)
require(stringr)
require(dplyr)
require(memoise)

options(expressions = 5e5)

# CLEAN TEXT:
# We want to isolate only the text protion of the tweets, removing handles, hashtags, URLs, and special
# characters.

# remove twitter handles (usernames starting with @) from tweets
# remove special characters
# remove URLs
# send all text to lowercase, to standardize all capitalization
trim_tweet <- function(tweet.text){
  # remove handles
  clean <- gsub("@\\w+ *", "", tweet.text)
  
  # remove newline character
  clean <- gsub("\n", " ", clean)
  
  # we want to get rid of anything that is not a letter, space, or hashtag
  # we do not want numbers because they are not text
  clean <- gsub("[^a-zA-Z #]","",clean)
  
  # remove URLs
  clean <- gsub("https\\w+ *", "", clean)
  
  # send all characters to lowercase
  clean <- tolower(clean)
  
  # trim spaces that may have piled up after removing hastags, handles, and URLs
  clean <- str_trim(clean, side = "both")
  
  # return the cleaned tweet, as a STRING
  return(clean)
}

# function to clean text of whole dataframe at once
# input must be a vector
clean_text <- function(tweets.vector){
  if (!is.vector(tweets.vector)){
    warning("To trim and analyze tweets, you must input a vector. Input '[*parsed tweets dataframe name*]$text' into function to continue")
  }else{
    # clean tweets
    clean_tweets <- sapply(X = tweets.vector, FUN = trim_tweet, simplify = TRUE, USE.NAMES = FALSE)
    
    # convert tweets to dataframe
    tweets.df <- data.frame(clean_tweets, stringsAsFactors = FALSE)
    # name the column of the new dataframe
    colnames(tweets.df) <- c("text")
    
    # return all of the cleaned tweets, as a DATAFRAME
    return(tweets.df)
  }
}

##########################################################################################################

# EXTRACT HASHTAGS:
# We want to extract and isolate only the hashtags to analyze what hashtags are trending
# in the dataset. These are different than regular text because there can be hashtags used
# by everyone, because they are more universal/standardized.

# Creating a vector of ONLY hashtags
hashtags <- function(tweet.text){
  # extract all words that start with #
  clean <- str_extract_all(tweet.text, "#\\w+ *")
  #convert to dataframe
  clean.vector <- unlist(clean, use.names = FALSE)
  
  # send all characters to lowercase to standardize capitalization
  hashtags.vector <- sapply(X = clean.vector, FUN = tolower, simplify = TRUE, USE.NAMES = FALSE)
  
  # return the isolated hashtags, as a VECTOR
  return(hashtags.vector)
}

# function to extract hashtags from whole dataframe
# input must be a vector
extract_hashtags <- function(tweets.vector){
  if (!is.vector(tweets.vector) || !is.character(tweets.vector[1])){
    warning("Input must be a character vector.")
  }else{
    # extract hashtags to a LIST
    htags.list <- apply(X = matrix(tweets.vector, nrow = 1), MARGIN = 2, FUN = hashtags)
    
    # convert list to VECTOR to remove list names and empty list entries,
    # and split entries of multiple hashtags into distinct values
    htags.vector <- unlist(htags.list, use.names = FALSE)
    
    # create dataframe of hashtags
    hashtags.df <- data.frame("hashtags" = htags.vector)
    
    # return ALL of the isolated hashtags, as a DATAFRAME
    return(hashtags.df)
  }
}

####################################################################################


# AVERAGE NUMBER OF WORDS PER TWEET

# calculate number of words in tweet
num_words <- function(tweet.text){
  # split the tweets at the spaces to isloate each word
  # the output is a LIST
  word_list <- str_split(tweet.text, " ")
  # convert the list into a VECTOR
  word_vector <- unlist(word_list, use.names = FALSE)
  # remove any blank entries in the vector
  # (indicating there were multiple spaces in the tweet text)
  words <- word_vector[word_vector != ""]
  
  # return NUMBER of words
  return(length(words))
}

# calculate AVERAGE number of words per tweet in the dataset
# input must be a vector
avg_words <- function(tweets.vector){
  if (!is.vector(tweets.vector) || !is.character(tweets.vector[1])){
    warning("Input must be a character vector.")
  }else{
    # calculate number of words per tweet
    words_per_tweet <- sapply(X = tweets.vector, FUN = num_words, simplify = TRUE, USE.NAMES = FALSE)
    
    # calculate descriptive statistics
    m <- mean(words_per_tweet)
    md <- median(words_per_tweet)
    v <- var(words_per_tweet)
    s <- sd(words_per_tweet)
    
    # return a DATAFRAME with the descriptive statistics of the number of words per tweet
    return(data.frame("mean" = m, "median" = md, "variance" = v, "standard.deviation" = s, row.names = c("num.words")))
  }
}


####################################################################################


# AVERAGE LENGTH (number of characters) OF TWEET

# calculate number of characters (chars) in tweet
tweet_length <- function(tweet.text){
  # split the tweets at the spaces to isloate each word
  # the output is a LIST
  char_list <- str_split(tweet.text, "")
  # convert the list into a VECTOR
  char_vector <- unlist(char_list, use.names = FALSE)
  # remove any spaces
  chars <- char_vector[char_vector != " "]
  
  # return NUMBER of words
  return(length(chars))
}

# calculate AVERAGE length of a tweet in the dataset
# input must be a vector
avg_length <- function(tweets.vector){
  if (!is.vector(tweets.vector) || !is.character(tweets.vector[1])){
    warning("Input must be a character vector.")
  }else{
    # initialize vector to hold length of tweet
    lengths_of_tweets <- sapply(X = tweets.vector, FUN = tweet_length, simplify = TRUE, USE.NAMES = FALSE)
    
    # calculate descriptive statistics
    m <- mean(lengths_of_tweets)
    md <- median(lengths_of_tweets)
    v <- var(lengths_of_tweets)
    s <- sd(lengths_of_tweets)
    
    # return a DATAFRAME with the descriptive statistics of the length of the tweets
    return(data.frame("mean" = m, "median" = md, "variance" = v, "standard.deviation" = s, row.names = c("tweet.length")))
  }
}


####################################################################################


# AVERAGE LENGTH OF WORD
# Here, we do NOT want to include hashtags in our analysis. We want to know the average length
# of the words that people use to communicate their meaning, whereas hashtags are TAGS for their
# content to associate it with a larger conversation.

## This is the function we will use to possibly detect Australian slang


# calculate length of words in tweet
# input must be a string
lengths_of_words <- function(tweet.text){
  # remove all hashtags
  no.hashtags <- gsub("#\\w+ *", "", tweet.text)
  
  # split the tweets at the spaces to isloate each word
  # the output is a LIST
  word_list <- str_split(no.hashtags, " ")
  # convert the list into a VECTOR
  word_vector <- unlist(word_list, use.names = FALSE)
  # remove any blank entries in the vector
  # (indicating there were multiple spaces in the tweet text)
  words <- word_vector[word_vector != ""]
  
  # calculate word lengths
  word_lengths <- sapply(X = words, FUN = nchar, simplify = TRUE, USE.NAMES = FALSE)
  
  # return a VECTOR of the number of words
  return(word_lengths)
}

# calculate AVERAGE length of words in the dataset
# input must be a vector
avg_wrd_lngth <- function(tweets.vector){
  if (!is.vector(tweets.vector) || !is.character(tweets.vector[1])){
    warning("Input must be a character vector.")
  }else{
    # create a LIST of the word lengths in each tweet
    word_lengths.list <- apply(X = matrix(tweets.vector, nrow = 1), MARGIN = 2, FUN = lengths_of_words)
    
    # unlist the list to create a VECTOR of word lengths
    lengths.vector <- unlist(word_lengths.list, use.names = FALSE)
    
    # calculate descriptive statistics
    m <- mean(lengths.vector)
    md <- median(lengths.vector)
    v <- var(lengths.vector)
    s <- sd(lengths.vector)
    
    # return a DATAFRAME with the descriptive statistics of the number of words per tweet
    return(data.frame("mean" = m, "median" = md, "variance" = v, "standard.deviation" = s, row.names = c("word.length")))
  }
}


####################################################################################


# AVERAGE NUMBER OF HASHTAGS PER TWEET


# Creating a data frame of ONLY hashtags
num_hashtags <- function(tweet.text){
  # extract all words that start with #
  # result is a LIST
  htags.list <- str_extract_all(tweet.text, "#\\w+ *")
  
  # convert to vector
  htags.vector <- unlist(htags.list, use.names = FALSE)
  
  # return NUMBER of hashtags
  return(length(htags.vector))
}

avg_num_hashtags <- function(tweets.vector){
  if (!is.vector(tweets.vector) || !is.character(tweets.vector[1])){
    warning("Input must be a character vector.")
  }else{
    # calculate number of hashtags per tweet
    num.htags <- sapply(X = tweets.vector, FUN = num_hashtags, simplify = TRUE, USE.NAMES = FALSE)
    
    # calculate descriptive statistics
    m <- mean(num.htags)
    md <- median(num.htags)
    v <- var(num.htags)
    s <- sd(num.htags)
    
    # return a DATAFRAME with the descriptive statistics of the number of words per tweet
    return(data.frame("mean" = m, "median" = md, "variance" = v, "standard.deviation" = s, row.names = c("num.hashtags")))
  }
}


####################################################################################


# COMPLETE TEXT ANALYSIS
## combining all above functions into one function


analyze_text <- function(tweets.vector){
  if (!is.vector(tweets.vector) || !is.character(tweets.vector[1])){
    warning("Input must be a character vector.")
  }else{
    # clean the text
    clean.txt <- clean_text(tweets.vector)
    
    # number of words per tweet
    num.words <- avg_words(clean.txt$text)
    
    # length of tweets
    twt.lngth <- avg_length(clean.txt$text)
    
    # length of words
    wrd.length <- avg_wrd_lngth(clean.txt$text)
    
    # number of hashtags
    num.htags <- avg_num_hashtags(clean.txt$text)
    
    # create of dataframe of analysis results
    analysis.df <- rbind(num.words, twt.lngth, wrd.length, num.htags)
    
    return(analysis.df)
  }
}

##########################################################################################################
##########################################################################################################
##########################################################################################################

# If we want to conduct t-tests on the above qualities of the tweets in each city
# (number of words per tweet, length of tweet, length of word, number of hashtags per tweet),
# we need to have access to numeric vectors. The above functions return dataframes of
# descriptive statistics, so the functions below return the original vectors used
# to calculate the descriptive statistics.

# NUMBER OF WORDS PER TWEET

# get a vector of the number of words in every tweet collected
num_words.vector <- function(tweets.vector){
  if (!is.vector(tweets.vector) || !is.character(tweets.vector[1])){
    warning("Input must be a character vector.")
  }else{
    # crate LIST of number of words per tweet
    words_per_tweet <- sapply(X = tweets.vector, FUN = num_words, simplify = TRUE, USE.NAMES = FALSE)
    
    return(words_per_tweet)
  }
}


####################################################################################


# LENGTH (number of characters) OF TWEET

# get a vector of the length of every tweet collected
tweet_length.vector <- function(tweets.vector){
  if (!is.vector(tweets.vector) || !is.character(tweets.vector[1])){
    warning("Input must be a character vector.")
  }else{
    # create LIST of lengths of tweet
    lengths_of_tweets <- sapply(X = tweets.vector, FUN = tweet_length, simplify = TRUE, USE.NAMES = FALSE)
    
    return(lengths_of_tweets)
  }
}


####################################################################################


# LENGTH OF WORD

# get a vector of the length of every word tweeted
word_length.vector <- function(tweets.vector){
  if (!is.vector(tweets.vector) || !is.character(tweets.vector[1])){
    warning("Input must be a character vector.")
  }else{
    # create a LIST of word lengths
    word_lengths <- apply(X = matrix(tweets.vector, nrow = 1), MARGIN = 2, FUN = lengths_of_words)
    
    # unlist the list into a VECTOR
    lengths.vector <- unlist(word_lengths, use.names = FALSE)
    
    return(lengths.vector)
  }
}


####################################################################################


# NUMBER OF HASHTAGS PER TWEET

# get a vector of the number of hashtags in every tweet collected
num_hashtags.vector <- function(tweets.vector){
  if (!is.vector(tweets.vector) || !is.character(tweets.vector[1])){
    warning("Input must be a character vector.")
  }else{
    # initialize vector to hold number of hashtags per tweet
    num.htags <- sapply(X = tweets.vector, FUN = num_hashtags, simplify = TRUE, USE.NAMES = FALSE)
    
    return(num.htags)
  }
}

##########################################################################################################
##########################################################################################################
##########################################################################################################

# GET TWEETS FROM JSON FILES

tweets.Boston <- parseTweets("tweetsBoston.json")
tweets.Sydney <- parseTweets("tweetsSydney.json")


# Boston tweets

boston.text <- clean_text(tweets.Boston$text)

# Sydney tweets

sydney.text <- clean_text(tweets.Sydney$text)

# Descriptive statistics of Boston tweets

boston.descriptives <- analyze_text(tweets.Boston$text)

# Descriptive statistics of Sydney tweets

sydney.descriptives <- analyze_text(tweets.Sydney$text)

# Vectors of Boston tweet qualities

b.num.words <- num_words.vector(boston.text$text)
b.tweet.length <- tweet_length.vector(boston.text$text)
b.word.length <- word_length.vector(boston.text$text)
b.num.hashtags <- num_hashtags.vector(boston.text$text)

# Vectors of Sydney tweets qualities

s.num.words <- num_words.vector(sydney.text$text)
s.tweet.length <- tweet_length.vector(sydney.text$text)
s.word.length <- word_length.vector(sydney.text$text)
s.num.hashtags <- num_hashtags.vector(sydney.text$text)