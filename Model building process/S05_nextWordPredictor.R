########################################################################
# Predicting next word function, based on a backoff strategy: it always
# looks for the best nextWords table. If typed ngram is not found there then 
# its first token is deleted and process proceed with the next down table.
# The result is a list of the ngrams finally used and the list of most
# probable three next words for this ngram (in descending order)
########################################################################
library(data.table)

# Loading ngram tables as data.table for speed of searching
bigramNextWord <- as.data.table(readRDS("nextWords_2_sbkf.rds"))
trigramNextWord <- as.data.table(readRDS("nextWords_3_sbkf.rds"))
fourgramNextWord <- as.data.table(readRDS("nextWords_4_sbkf.rds"))

# The function received the typed ngram
predNextWord <- function(typedNgram, mode = "phrase"){
  
  # Initializing next words vector and ngrams looked for
  usedNgrams <- vector(mode = "character")
  nextWords <- vector(mode = "character")
  
  # Adding the sentence mark at the beguinning of the string if the mode
  # is phrase (mode used in prediction). If you are estimating performance
  # mode should be "word"
  if (mode == "phrase"){
    typedNgram <- c("<s/> ", typedNgram)
  }
  
  # Coverting typedNgram to lower case
  typedNgram <- tolower(typedNgram)
  
  # Splitting typed ngram in its tokens after replace punctuations by one <s/>
  typedNgram <- gsub("[.|,|;|:|!|?]", " <s/> ", typedNgram)
  typedNgram <- gsub("<s/>\\s*<s/>(\\s*<s/>)*", "<s/> ", typedNgram)
  tokens <- unlist(strsplit(typedNgram, "\\s+"))
  tokens <- unlist(lapply(tokens, function(x){x <- x[x != ""]}))
  
  # If typed ngram is longer than 3 we reduce it to its 3 last tokens
  # (maximum number of tokens we can process with a fourgram table).
  if (length(tokens) > 3) {
    tokens <- tokens[(length(tokens) - 2):length(tokens)]
  }
  
  # In any case we "rebuild" the typedNgram from the tokens (for deleting
  # unnecesary blanks/spaces generated after replacing punctuation)
  typedNgram <- paste(tokens, collapse = " ")

  # We try to find the ngram reducing its number of tokens each time
  # until there is no tokens
  while (length(tokens) > 0){
    
    # Depending on the number of tokens we select the corresponding ngram
    # table
    if (length(tokens) == 1){
      selNgram <- bigramNextWord[ngram == typedNgram]
    } else if (length(tokens) == 2){
      selNgram <- trigramNextWord[ngram == typedNgram]
    } else if (length(tokens) == 3){
      selNgram <- fourgramNextWord[ngram == typedNgram]
    }
    
    # Cheking the results of the search
    if(nrow(selNgram) > 0){
      
      # If the ngram has been found, its next words (3 columns) are selected
      words <- unlist(selNgram[, 2:4, with = FALSE])
      nextWords <- c(nextWords, words)
      
      # The found ngram is registered (more than one ngram can be used to
      # get 3 predictions)
      usedNgrams <- c(usedNgrams, typedNgram)
      
      # If at least three words have been found the process is stopped
      if (length(nextWords) >= 3){
          break
      }
    }
      
    # If the ngram doesn't exist in the table or the number of words found 
    # is below 3 we reduce it deleting the first token, provided 
    # the number of tokens is greater than 1. 
    if (length(tokens) > 1){
        tokens <- tokens[2:length(tokens)]
        typedNgram <- paste(tokens, collapse = " ")
    } else {
        tokens <- NULL
    }
  }
  # Add the most common words just in case we haven't finally reach at least
  # three words y delete any repetition
  nextWords <- c(nextWords, c("the", "to", "and"))
  nextWords <- unique(nextWords)
  
  # We return a list with the ngram finally found and the next words for it
  return(list(usedNgrams = usedNgrams, nextWords = nextWords[1:3]))
}