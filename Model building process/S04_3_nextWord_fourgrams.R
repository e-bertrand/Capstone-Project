############################################################################
# Creating the table with the most probable continuation after three words
# from the  4-gram model. If no 4-gram for that sequence exists, stupid backoff
# with lambda = 0.4 is applied
########################################################################

library(dplyr)
library(tidyr)
library(data.table)
library(stringr)

# Usualy the three most probable continuations are selected. As it could
# include a number of <s/> and NA double number is processed
freqLimit <- 3 * 2

# Loading ngrams models of lower order than the basic one
ngram_4 <- as.data.table(readRDS("ngram_4.rds")) 
ngram_3 <- as.data.table(readRDS("ngram_3.rds")) 
ngram_2 <- as.data.table(readRDS("ngram_2.rds")) 
ngram_1 <- as.data.table(readRDS("ngram_1.rds"))

# Selecting all the unique bases from the basic model
baseList <- unique(ngram_4$base)
numBases <- length(baseList)

# Selecting the most frequent unigrams as residual words if necessary 
# (last step in stupid backoff algorithm just in case nothing else apears
# in high-order ngrams).
# As distance for basic model is 3, lamba is raised to the power of 3
freqWords <- ngram_1[order(freq, decreasing = TRUE)][ngram != "<s/>"][1:freqLimit]
residualWords <- freqWords$ngram
residualScores <- freqWords$prob * (0.4 ^ 3)

iniTime <- Sys.time()
startTime <- Sys.time()

# iInitializing vector of the three most likely continuation for each base
w_1 <- vector(mode = "character", length = numBases)
w_2 <- vector(mode = "character", length = numBases)
w_3 <- vector(mode = "character", length = numBases)

# For each base we get the three most likely continuations
for (i in 1:numBases){
  
  # Initialize the list of next words and its scores following stupid backoff
  nextWords <- vector(mode = "character")
  scores <- vector(mode = "numeric")
  
  # For each base we get the last two and one word
  base3 <- baseList[i]
  tokens <- unlist(strsplit(base3, "\\s+"))
  base2 <- paste(tokens[2:3], collapse = " ")
  base1 <- tokens[3]
  
  # Get the frequency for each base from the corresponding low-order ngrams
  num3 <- ngram_3[ngram == base3]$freq
  num2 <- ngram_2[ngram == base2]$freq
  num1 <- ngram_1[ngram == base1]$freq
   
  # Starting from the high order ngram we get the most frequent ngrams,
  # calculate their scores after stupid backoff algorithm with lambda = 0.4 
  # and add to the global list of next words and scores
  most4 <- ngram_4[base == base3][order(freq, decreasing = TRUE)][1:freqLimit]
  most4 <- most4[!is.na(ngram)]
  if (!identical(num3, numeric(0))){
    nextWords4 <- sapply(str_split(most4$ngram, " "), function(x){x[4]})
    scores4 <- most4$freq / num3
    nextWords <- c(nextWords, nextWords4)
    scores <- c(scores, scores4)
  }

  most3 <- ngram_3[base == base2][order(freq, decreasing = TRUE)][1:freqLimit]
  most3 <- most3[!is.na(ngram)]
  if (!identical(num2, numeric(0))){
    nextWords3 <- sapply(str_split(most3$ngram, " "), function(x){x[3]})
    scores3 <- most3$freq / num2 * (0.4 ^ 1)
    nextWords <- c(nextWords, nextWords3)
    scores <- c(scores, scores3)
  }
  
  most2 <- ngram_2[base == base1][order(freq, decreasing = TRUE)][1:freqLimit]
  most2 <- most2[!is.na(ngram)]
  if (!identical(num1, numeric(0))){
    nextWords2 <- sapply(str_split(most2$ngram, " "), function(x){x[2]})
    scores2 <- most2$freq / num1 * (0.4 ^ 2)
    nextWords <- c(nextWords, nextWords2)
    scores <- c(scores, scores2)
  }
  
  # Adding to the lists the residual words
  nextWords <- c(nextWords, residualWords)
  scores <- c(scores, residualScores)
  
  # Ordering word list by decreasing scores, removing duplicates and <s/>
  # and getting the first three
  scores <- scores[order(scores, decreasing = TRUE)]
  nextWords <- nextWords[order(scores, decreasing = TRUE)]
  
  acceptable <- !duplicated(nextWords) & nextWords != "<s/>"
  nextWords <- nextWords[acceptable]
  scores <- scores[acceptable]

  w_1[i] = nextWords[1]
  w_2[i] = nextWords[2]
  w_3[i] = nextWords[3]
  
  # Reporting progress
  if (i %% 10000 == 0){
      cat("\nProcessed", i, "ngrams from", numBases, "in",
          difftime(Sys.time(), iniTime, units = "mins"), "m.")
      iniTime <- Sys.time()
  }
}

# Creating and saving final data.table
next_words_4 <- data.table(ngram = baseList,
                           w_1 = w_1,
                           w_2 = w_2,
                           w_3 = w_3)

cat("\nTotal time:, ", difftime(Sys.time(), startTime, units = "mins"), "m.")

saveRDS(next_words_4,  paste0("nextWords_4_sbkf.rds"))

