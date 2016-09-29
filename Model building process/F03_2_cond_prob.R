#############################################################################
# Calculating the conditioned probabilties of each ngrams to the base (root
# words) and adding them as a column to a ngram frequency table
#############################################################################

library(dplyr)
library(tidyr)
library(data.table)

addCondProb <- function (ngram){
    
    ini <- Sys.time()
    
    numNgrams <- nrow(ngram)
    
    # Grouping ngram by the base
    baseFreq <- ngram %>%
        group_by(base) %>%
        summarise(freq = sum(freq))
    
    baseFreq <- as.data.table(baseFreq)
    
    cat("\n    - Grouping ngrams", 
        difftime(Sys.time(), ini, units = "secs"), "s.")
    
    # Creating the vector to store the base frequency of each ngram 
    freqBase <- vector(mode = "integer", length = numNgrams)
    
    # Assigning the first base with its frequency
    currentBase <- ngram$base[1]
    currentFreq <- baseFreq[base == ngram$base[1]]$freq
    
    ini <- Sys.time()
    
    for (i in 1:numNgrams){
        
        # If the base has changed update the base and its frequency
        if (ngram$base[i] != currentBase) {
            currentBase <- ngram$base[i]
            currentFreq <- baseFreq[base == ngram$base[i]]$freq
        }
        
        # Assigning the base frequency to each ngram
        freqBase[i] <- currentFreq
        
        if (i %% 100000 == 0){
            cat("\n    - Assigned probability to", i, "of", numNgrams, 
                "ngrams in", difftime(Sys.time(), ini, units = "secs"), "s.")
            ini <- Sys.time()
        }
    }
    
    # Creating new column with the conditional probability to the base
    ngram$probCond <- ngram$freq / freqBase
    
    rm(freqBase); gc(verbose = FALSE)
    
    return(ngram)
    
}