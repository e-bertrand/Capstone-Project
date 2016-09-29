#############################################################################
# Evaluate the perfomance of a ngram model against an unseen corpus of text
# Accuracy as a ratio of correct predictions over total predictions is
# estimated from each ngram order.
# Two options are evaluated: a prediction is right if it matches only the first 
# next word predicted; or it is right if it is in the three next word proposed 
# ###########################################################################
rm(list = ls()); gc(verbose = FALSE)

library(stringr)
library(dplyr)
library(knitr)
library(data.table)
source("F06_1_get_ngrams.R")
source("S05_nextWordPredictor.R")

# Selecting a sample of elements from the test corpus
sampleSize <- 1000

# Loading the test corpus and getting a sample
iniTime <- Sys.time()
cat("\n Loading data... ")
text <- readRDS("text_test.RDS")

set.seed(15061958)
text <- text[sample(1:length(text), sampleSize)]

# We will test if ngram order and two criteria of accuracy. A table 
# for storing the results is initializated
accuracy_table <- data.frame(model = character(),
                             accuracy_1w = numeric(),
                             accuracy_3w = numeric())

# For each ngram order from 2 to 4 we estimated the accuracy
for (i in 2:4){
    
    cat("\nProcessin ngrams =", i)
    
    ngramSize <- i
    
    # Giving a name to the model depending on the order
    model <- ifelse(ngramSize == 2, "bigrams",
             ifelse(ngramSize == 3, "trigrams", "4-grams"))
    
    # We get the ngrams of order under evaluation from the test sample
    ngrams <- getNgrams(text, ngramSize)
    
    # Initializing hits counters
    hits_3w <- 0
    hits_1w <- 0
    
    # For each ngram extracted from the test sample we check the perdiction
    iniTime <- Sys.time()
    for (i in 1: length(ngrams)){
        
        # Get the first tokens of the ngram
        tokens <- unlist(strsplit(ngrams[i], "\\s+"))
        tokens <- unlist(lapply(tokens, function(x){x <- x[x != ""]}))
        sequence <- paste(tokens[1:(ngramSize - 1)], collapse = " ")
        
        # Get the prediction (mode should be "word" for avoiding
        # automatic addtion of <s/> as initial token of a phrase)
        prediction <- predNextWord(sequence, "word")
        
        # Check if the last word of the ngram matches the predictions
        if (tokens[ngramSize] %in% prediction$nextWords){
            hits_3w <- hits_3w + 1
        }
        if (tokens[ngramSize] == prediction$nextWords[1]){
            hits_1w <- hits_1w + 1
        }
        
        # Reporting progress
        if (i %% 1000 == 0){
            cat("\nProcessed", i, "ngrams from", length(ngrams), "with", 
                hits_1w, "/", hits_3w, "hits")
            cat(" in", difftime(Sys.time(), iniTime, units = "secs"), "s.")
            iniTime <- Sys.time()
        }
    }
    
    # Add results to the accuracy table as percentage of hits
    accuracy_table <- rbind(accuracy_table,
                            data.frame(
                                model = model,
                                accuracy_1w = round(hits_1w / length(ngrams) * 100, 2),
                                accuracy_3w = round(hits_3w / length(ngrams) * 100, 2)))
}

# Print results (not saved programmaticaly to allow comparison between tests)
print(kable(accuracy_table, format = "pandoc"))