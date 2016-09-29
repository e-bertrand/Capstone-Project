library(data.table)

getStatistics <- function(source, cutFreq = 1000, verbose=FALSE){
    if (verbose == TRUE) cat("\nReading data files for", source, "...")
  
    source_cleaned <-  readRDS(paste0("clean_", source, "_sample.rds"))
    source_unigram <-  readRDS(paste0("ngram_1_", source, "_sample.rds"))
    source_bigram <-   readRDS(paste0("ngram_2_", source, "_sample.rds"))
    source_trigram <-  readRDS(paste0("ngram_3_", source, "_sample.rds"))
    source_fourgram <-  readRDS(paste0("ngram_4_", source, "_sample.rds"))
    
    # Number of elements
    elements <- length(source_cleaned)
    # Number of chars
    chars <- sum(nchar(source_cleaned))
    # Number of sentences
    sentences <- source_unigram[which(source_unigram$ngram == "<s/>"), ]$freq
    # Number of words
    # It would be possible to consider words without <s/> marks
    # words <- sum(source_unigram[which(source_unigram$ngram != "<s/>"), ]$freq)
    words <- sum(source_unigram$freq)
    # Number of unigrams, bigrams, trigrams
    unigrams <- nrow(source_unigram)
    bigrams <- nrow(source_bigram)
    trigrams <- nrow(source_trigram)
    fourgrams <- nrow(source_fourgram)
    
    # Ratios
    charsSentence <- chars / sentences
    wordsSentence <- words / sentences
    charsWord <- chars / words
    
    # Summary record
    
    summary_record <- data.frame(source,
                                 elements, chars, sentences, words,
                                 unigrams, bigrams, trigrams, fourgrams,
                                 charsSentence, wordsSentence, charsWord,
                                 stringsAsFactors = FALSE)
    
    unigramDT <- as.data.table(source_unigram)
    
    # Most frequent words
    mostFreqWords <- data.table(source, unigramDT[order(freq, decreasing = TRUE)][1:500])
    mostFreqWords <- mostFreqWords[ngram != "<s/>"]

    # Word and ngrams vs frecuency
    if (verbose == TRUE) cat("\nBuilding frequency table...")
    ngramFreq <- data.frame(source = character(cutFreq),
                            minFreq = numeric(cutFreq),
                            totNgrams = numeric(cutFreq),
                            totWords = numeric(cutFreq),
                            stringsAsFactors = FALSE)
    
    for (i in 1:cutFreq){
      
        if (verbose == TRUE){
          if (i %% round(cutFreq / 10, 0) == 0){
              cat("\n       > Processing ", round(i / cutFreq * 100, 0), "%")
          }
        }
        
        minFreq <- i
        totNgrams <- nrow(unigramDT[freq >= i])
        totWords <- sum(unigramDT[freq >= i]$freq)
        ngramFreq[i, ] <- data.frame(source, minFreq, totNgrams, totWords,
                                     stringsAsFactors = FALSE)
    }
    
    
    rm(source_cleaned, source_unigram, source_bigram, source_trigram,
       source_fourgram)
    gc(verbose = FALSE)
    
    return(list(summary = summary_record, 
                mostFreqWords = mostFreqWords,
                freqTable = ngramFreq))
}