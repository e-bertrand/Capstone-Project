######################################################################
# Create a ngram (size variable) frequency table from a string of text
######################################################################

library(stylo)
library(data.table)

generateNgramFreq <- function(text, ngSize){

    # Collapse the data elements in a string with end of sentences
    # between them It could happen that several <s/> apears together an must
    # be reduced to only one
    iniTime <- Sys.time()
    cat("\n Collapsing data elements... ")
    
    text_all <- paste("<s/>", paste(text, collapse = " <s/> "))
    text_all <- gsub("<s/>\\s*<s/>(\\s*<s/>)*", "<s/> ", text_all)
    
    rm(text); gc(verbose = FALSE)
    procTime <- difftime(Sys.time(), iniTime, units = "secs")
    cat(procTime, "s.", "/ Memory used:", memory.size(TRUE), "MB.")
 
    # Using 'stylo' for tokenizing the text in words. All english contractions
    # are preserved
    iniTime <- Sys.time()
    cat("\n Extracting words from data... ")
    
    ttw <- txt.to.words.ext(text_all, language = "English.all", 
                            splitting.rule = "[[:space:]]+")
    
    procTime <- difftime(Sys.time(), iniTime, units = "secs")
    cat(procTime, "s.", "/ Memory used:", memory.size(TRUE), "MB.")
    rm(text_all); gc(verbose = FALSE)

    # Producing the ngrams of selected size from the list of words
    iniTime <- Sys.time()
    cat("\n Making ngrams... ")
    
    ng <- make.ngrams(ttw, ngram.size = ngSize)
    
    procTime <- difftime(Sys.time(), iniTime, units = "secs")
    cat(procTime, "s.", "/ Memory used:", memory.size(TRUE), "MB.")
    rm(ttw); gc(verbose = FALSE)

    # Creating the frequency list of the ngrams extracted
    iniTime <- Sys.time()
    cat("\n Making frecuency list... ")
    
    freqList <- make.frequency.list(ng, value = TRUE, relative = FALSE)
    
    procTime <- difftime(Sys.time(), iniTime, units = "secs")
    cat(procTime, "s.", "/ Memory used:", memory.size(TRUE), "MB.")
    rm(ng); gc(verbose = FALSE)

    # Transforming the frequency list in an ordered data.table
    iniTime <- Sys.time()
    cat("\n Building ngrams data frame... ")
    
    ngramFreqTot <- data.frame(ngram = names(freqList), 
                               freq = as.vector(freqList),
                               stringsAsFactors = FALSE)
    ngramFreqTot <- as.data.table(ngramFreqTot)[order(ngram)]
    procTime <- difftime(Sys.time(), iniTime, units = "secs")
    
    cat(procTime, "s.", "/ Memory used:", memory.size(TRUE), "MB.")
    rm(freqList); gc(verbose = FALSE)
    
    # Returning the frequency table
    return(ngramFreqTot)
}